from __future__ import annotations

from collections import defaultdict
from collections.abc import Callable, Iterable, Mapping, Sequence
from typing import IO, NoReturn, Self, assert_never

from retroasm.utils import bad_type

from .codeblock import (
    BasicBlock,
    CodeGraph,
    CodeNode,
    FunctionBody,
    Load,
    LoadedValue,
    Store,
    walk_nodes,
)
from .expression import (
    Expression,
    IntLiteral,
    Select,
    ZeroTest,
    is_literal_false,
    is_literal_true,
)
from .expression_simplifier import simplify_expression
from .function import Function
from .input import BadInput, ErrorCollector, InputLocation
from .reference import BitString, FixedValue, Reference, SingleStorage, bad_reference
from .storage import ArgStorage, IOStorage, Register, Storage, Variable


def no_args_to_fetch(name: str) -> NoReturn:
    """Argument fetcher that can be used when a function has no arguments."""
    raise ValueError(f'No arguments provided, "{name}" requested')


def _check_undefined(operations: Sequence[Load | Store], collector: ErrorCollector) -> None:
    """Report uses of uninitialized local variables."""

    with collector.check():
        for operation in operations:
            match operation:
                case Load(storage=Variable(name=name), location=location):
                    # Any remaining loads are undefined: if the value was defined,
                    # it would have been replaced by its traced value.
                    collector.error(
                        f'Variable "{name}" is used while undefined',
                        location=location,
                    )


def returned_bits(ret_ref: Reference | None) -> Sequence[BitString]:
    return () if ret_ref is None else (ret_ref.bits,)


class CodeBlockBuilder:
    @classmethod
    def with_stored_values(cls, stored_values: Mapping[Storage, Expression]) -> Self:
        builder = cls()
        builder._current_block._stored_values = dict(stored_values)
        return builder

    def __init__(self) -> None:
        self._current_block = BasicBlockBuilder()
        self._current_node = self._entry = CodeNode()
        self._nodes_by_label: dict[str, CodeNode] = {}
        self._branches: dict[str, list[InputLocation]] = {}

    def dump(self, *, file: IO[str] | None = None) -> None:
        """Prints the current state of this code block builder on stdout."""

        self._current_block.dump(file=file)

    def _check_labels(self, collector: ErrorCollector) -> None:
        """
        Verify that labels are used correctly.
        Undefined labels are logged as errors, unused labels as warnings.
        Raises `BadInput` for each undefined label that is branched to.
        """

        with collector.check():
            for label, node in self._nodes_by_label.items():
                if node.label is None:
                    collector.error(
                        f'Label "{label}" does not exist', location=self._branches[label]
                    )
                elif label not in self._branches:
                    collector.warning(f'Label "{label}" is unused', location=node.location)

    def emit_load_bits(self, storage: Storage, location: InputLocation | None) -> Expression:
        """
        Loads the value from the given storage by emitting a Load node on
        this builder.
        Returns an expression that represents the loaded value.
        """
        if self._current_node.location is None:
            self._current_node.location = location
        return self._current_block.emit_load_bits(storage, location)

    def emit_store_bits(
        self, storage: Storage, value: Expression, location: InputLocation | None
    ) -> None:
        """
        Stores the value of the given expression in the given storage by
        emitting a Store node on this builder.
        """
        if self._current_node.location is None:
            self._current_node.location = location
        self._current_block.emit_store_bits(storage, value, location)

    def _node_for_label(self, label: str) -> CodeNode:
        """
        Return the code node for the given label, creating it if necessary.
        A new node will not have its `label` field filled in initially;
        that will only happen when `add_label()` is called for the label.
        """
        nodes_by_label = self._nodes_by_label
        node = nodes_by_label.get(label)
        if node is None:
            node = CodeNode()
            nodes_by_label[label] = node
        return node

    def emit_label(self, label: str, location: InputLocation | None = None) -> None:
        """
        Add a label that identifies the current position in the code block.
        Raises `BadInput` if the label was already defined.
        """

        node = self._node_for_label(label)
        if node.label is not None:
            raise BadInput(f'label "{label}" already defined', location, node.location)
        node.label = label

        # Complete previous node and link it to label node.
        prev_node = self._current_node
        self._complete_node()
        prev_node.outgoing.append((IntLiteral(1), node))

        self._current_node = node
        self._current_node.location = location

    def emit_branch(
        self,
        label: str,
        condition: Expression = IntLiteral(1),
        *,
        label_location: InputLocation | None = None,
    ) -> None:
        """
        Add a branch to a given label.
        """

        # Remember used labels so we can verify their existence later.
        locations = self._branches.setdefault(label, [])
        if label_location is not None:
            locations.append(label_location)

        # Find the destination nodes.
        node_from = self._current_node
        self._complete_node()
        node_false = self._current_node
        node_true = self._node_for_label(label)

        # Link outgoing edges.
        condition_false = simplify_expression(ZeroTest(condition))
        condition_true = simplify_expression(ZeroTest(condition_false))
        if not is_literal_false(condition_true):
            node_from.outgoing.append((condition_true, node_true))
        if not is_literal_false(condition_false):
            node_from.outgoing.append((condition_false, node_false))

    def inline_function_call(
        self,
        func: Function,
        arg_map: Mapping[str, BitString],
        _location: InputLocation | None = None,
    ) -> BitString | None:
        """
        Inlines a call to the given function with the given arguments.
        All arguments should be passed as references: value arguments should
        have their expression wrapped in a FixedValue.
        If an argument value is None, that argument won't be substituted and
        can appear in the inlined body and in the returned reference.
        Returns a BitString containing the reference returned by the inlined
        function, or None if the function does not return anything.
        """

        code = func.code
        if code is None:
            # Missing body, probably because of earlier errors.
            ret_type = func.ret_type
            if ret_type is None:
                return None
            else:
                return bad_reference(ret_type, "returned by broken function").bits

        bad_args = arg_map.keys() - func.args.keys()
        if bad_args:
            raise KeyError("Non-existing arguments passed: " + ", ".join(bad_args))
        missing_args = func.args.keys() - arg_map.keys()
        if missing_args:
            raise KeyError("Missing values for arguments: " + ", ".join(missing_args))

        returned = self.inline_block(code, arg_map.__getitem__)
        if len(returned) == 1:
            return returned[0]
        else:
            assert len(returned) == 0, returned
            return None

    def inline_block(
        self, code: FunctionBody, arg_fetcher: Callable[[str], BitString] = no_args_to_fetch
    ) -> list[BitString]:
        """
        Inlines another code block into this one.
        The given argument fetcher function, when called with an argument name,
        should return the bit string passed for that argument, or None if the
        argument should remain an argument in the inlined block.
        Returns a list of BitStrings containing the values returned by the
        inlined block.
        Raises ValueError if there is mismatch between arguments and the values
        fetched for them.
        """

        load_results: dict[Expression, Expression] = {}

        def import_expr(expr: Expression) -> Expression:
            return expr.substitute(load_results.get)

        def import_storage_uncached(storage: Storage) -> BitString:
            match storage:
                case ArgStorage(name=name) as arg:
                    try:
                        bits = arg_fetcher(name)
                    except Exception as ex:
                        raise ValueError(f"Error fetching argument: {ex}") from ex
                    if arg.width != bits.width:
                        raise ValueError(
                            f'Argument "{name}" is {arg.width} bits wide, '
                            f"but the fetched value is {bits.width} bits wide"
                        )
                    return bits
                case storage:
                    return SingleStorage(storage.substitute_expressions(import_expr))

        storage_cache: dict[Storage, BitString] = {}

        def import_storage(storage: Storage) -> BitString:
            """
            Returns a bit string containing the imported version of the given
            storage.
            """
            bits = storage_cache.get(storage)
            if bits is None:
                bits = import_storage_uncached(storage)
                storage_cache[storage] = bits
            return bits

        # Copy nodes.
        for operation in code.operations:
            bits = import_storage(operation.storage)
            match operation:
                case Load(expr=expr, location=location):
                    value = bits.emit_load(self, location)
                    load_results[expr] = value
                case Store(expr=expr, location=location):
                    new_expr = import_expr(expr)
                    bits.emit_store(self, new_expr, location)
                case operation:
                    assert_never(operation)

        # Determine return value.
        return [
            ret_bits.substitute(storage_func=import_storage, expression_func=import_expr)
            for ret_bits in code.returned
        ]

    def create_code_block(
        self,
        returned: Iterable[BitString],
        collector: ErrorCollector | None = None,
        location: InputLocation | None = None,
    ) -> FunctionBody:
        """
        Returns a CodeBlock object containing the items emitted so far.
        The state of the builder does not change.
        The 'returned' sequence contains the bits strings that will be the
        returned values for the created block.
        Raises `BadInput*` if this builder does not represent a valid code block.
        """

        if collector is None:
            collector = ErrorCollector()

        def fixate_variable(storage: Storage) -> FixedValue | None:
            if isinstance(storage, Variable):
                value = self.emit_load_bits(storage, location)
                return FixedValue(value, storage.width)
            return None

        operations = self._current_block._operations

        # Fixate returned variables.
        returned = [bits.substitute(storage_func=fixate_variable) for bits in returned]

        # Simplify returned expressions.
        # This can also help find additional unused loads, if a loaded value is dropped
        # during simplification because it doesn't affect the expression's value.
        # TODO: Is this still needed as a separate step?
        _update_expressions_in_bitstrings(returned)

        # Local variables don't exist after exiting the block, so once their values have
        # been traced, we don't need the stores anymore.
        _remove_variable_stores(operations)

        # With known-value loads removed by the builder, some prior stores to the same
        # storages may have become redundant.
        _remove_overwritten_stores(operations)

        self._complete_node()

        self._check_labels(collector)

        # Fill in the incoming nodes.
        for node in walk_nodes(self._entry):
            for _cond, succ in node.outgoing:
                succ.incoming.append(node)

        # Replace empty nodes with a single fixed successor by that successor.
        for node in list(walk_nodes(self._entry)):
            if node.empty and len(node.outgoing) == 1:
                ((cond, new_succ),) = node.outgoing
                if is_literal_true(cond):
                    if node is self._entry:
                        self._entry = new_succ
                    else:
                        for prev in node.incoming:
                            for idx, (cond, succ) in enumerate(prev.outgoing):
                                if succ is node:
                                    prev.outgoing[idx] = (cond, new_succ)
                        new_succ.incoming.remove(node)
                        new_succ.incoming += node.incoming
                    if (label := node.label) is not None:
                        new_succ.label = label

        # Add a synthetic label to each unlabeled node.
        label_idx = 0
        for node in walk_nodes(self._entry):
            if node.label is None:
                node.label = str(label_idx)
                label_idx += 1

        _trace_stored_values(self._entry, returned)

        # Removal of unused loads will not enable any other simplifications.
        _remove_unused_loads(self._entry, returned)

        for node in walk_nodes(self._entry):
            if (block := node.block) is not None:
                _check_undefined(block.operations, collector)
                # TODO: Enable checking of all blocks once value tracing works across nodes.
                break

        code = CodeGraph(self._entry)
        return FunctionBody(code, returned)

    def _complete_node(self) -> None:
        # Complete and archive previous node.
        node = self._current_node
        node.block = self._current_block.create_basic_block()

        # Start a new node.
        self._current_node = CodeNode()
        self._current_block = BasicBlockBuilder()


class BasicBlockBuilder:
    def __init__(self) -> None:
        self._operations: list[Load | Store] = []
        self._stored_values: dict[Storage, Expression] = {}

    def dump(self, *, file: IO[str] | None = None) -> None:
        """Prints the current state of this code block builder on stdout."""

        for operation in self._operations:
            operation.dump(file=file)

    def emit_load_bits(self, storage: Storage, location: InputLocation | None) -> Expression:
        """
        Loads the value from the given storage by emitting a Load node on
        this builder.
        Returns an expression that represents the loaded value.
        """

        stored_values = self._stored_values
        storage = storage.substitute_expressions(simplify_expression)

        if storage.can_load_have_side_effect():
            _handle_side_effects(storage, stored_values)
        elif (value := stored_values.get(storage)) is not None:
            # Use known value instead of loading it.
            return value

        load = Load(storage, location)
        self._operations.append(load)
        value = load.expr
        if storage.is_load_consistent():
            # Remember loaded value.
            stored_values[storage] = value
        return value

    def emit_store_bits(
        self, storage: Storage, value: Expression, location: InputLocation | None
    ) -> None:
        """
        Stores the value of the given expression in the given storage by
        emitting a Store node on this builder.
        """

        stored_values = self._stored_values
        storage = storage.substitute_expressions(simplify_expression)

        # Simplifying gets rid of unnecessary truncation.
        value = simplify_expression(value)

        if storage.can_store_have_side_effect():
            _handle_side_effects(storage, stored_values)
        elif stored_values.get(storage) == value:
            # Current value is rewritten.
            return

        self._operations.append(Store(value, storage, location))
        if storage.is_sticky():
            # Remember stored value.
            stored_values[storage] = value

        # Remove stored values for storages that might be aliases.
        for storage2 in list(stored_values.keys()):
            if storage != storage2 and storage.might_be_same(storage2):
                del stored_values[storage2]

    def create_basic_block(self) -> BasicBlock:
        """
        Build a basic block containing the operations emitted so far.
        The state of the builder does not change.
        """

        operations = self._operations
        # TODO: Are there worthwhile simplifications that can be performed at this point
        #       or is it better to wait until the graph is finallized?

        return BasicBlock(operations)


def decompose_store(ref: Reference, value: Expression) -> Mapping[Storage, Expression]:
    """
    Decompose the storing of a value to a reference.
    Returns the values written to the underlying storages.
    """

    # Generate code for storing the value.
    builder = CodeBlockBuilder()
    ref.emit_store(builder, value, None)

    # Derive storage mapping from generated store nodes.
    mapping = {}
    for operation in builder._current_block._operations:
        assert isinstance(operation, Store), operation
        mapping[operation.storage] = operation.expr
    return mapping


def _handle_side_effects(storage: Storage, stored_values: dict[Storage, Expression]) -> None:
    """Drop stored values that might be invalidated by an I/O side effect."""

    if isinstance(storage, (IOStorage, ArgStorage)):
        # A load or store side effect on an I/O channel can affect other addresses
        # and even other channels, but it wouldn't affect registers.
        for storage2 in list(stored_values.keys()):
            if not isinstance(storage2, (Register, Variable)):
                del stored_values[storage2]


def _update_expressions_in_bitstrings(returned: list[BitString]) -> None:
    """Simplifies each expression in the given bit strings."""

    for i, ret_bits in enumerate(returned):
        returned[i] = ret_bits.simplify()


def _remove_variable_stores(operations: list[Load | Store]) -> None:
    """Remove stores to local variables."""

    for i in range(len(operations) - 1, -1, -1):
        match operations[i]:
            case Store(storage=Variable()):
                del operations[i]


def _remove_overwritten_stores(operations: list[Load | Store]) -> None:
    """Remove side-effect-free stores that will be overwritten."""

    will_be_overwritten = set()
    for i in range(len(operations) - 1, -1, -1):
        operation = operations[i]
        storage = operation.storage
        if not storage.can_store_have_side_effect():
            match operation:
                case Load():
                    will_be_overwritten.discard(storage)
                case Store():
                    if storage in will_be_overwritten:
                        del operations[i]
                    else:
                        will_be_overwritten.add(storage)


def _trace_stored_values(entry: CodeNode, returned: list[BitString]) -> None:
    """
    Trace the value of registers and local variables.
    This simplification step replaces `LoadedValue` uses by known expressions,
    which could also be `LoadedValue` expressions from earlier loads.
    Removal of loads of which the value is no longer used can be done in a later step.
    """

    exit_values: defaultdict[Storage, dict[str, Expression]] = defaultdict(dict)
    replacements: dict[Expression, Expression] = {}

    for node in walk_nodes(entry):
        # Gather storage value known at the start of this basic block.
        stored_values: dict[Storage, Expression] = {}
        for storage, value_by_label in exit_values.items():
            incoming_values = []
            for inc in node.incoming:
                assert inc.label is not None
                value = value_by_label.get(inc.label)
                if value is None:
                    break
                incoming_values.append(value)
            else:
                stored_values[storage] = simplify_expression(Select(*incoming_values))

        if (block := node.block) is not None:
            operations = list(block.operations)
            for idx, operation in enumerate(operations):
                expr: Expression
                match operation:
                    case Load(storage=old_storage, expr=expr):
                        storage = old_storage.substitute_expressions(
                            replacements.get
                        ).simplify()
                        if storage is not old_storage:
                            operations[idx] = load = Load(storage)
                            replacements[expr] = load.expr
                        if storage.can_load_have_side_effect():
                            _handle_side_effects(storage, stored_values)
                        elif (value := stored_values.get(storage)) is not None:
                            replacements[expr] = value
                            continue
                        if storage.is_load_consistent():
                            stored_values[storage] = expr

                    case Store(storage=old_storage, expr=old_expr):
                        expr = old_expr.substitute(replacements.get)
                        storage = old_storage.substitute_expressions(
                            replacements.get
                        ).simplify()
                        if expr is not old_expr or storage is not old_storage:
                            operations[idx] = Store(expr, storage)
                        if storage.can_store_have_side_effect():
                            _handle_side_effects(storage, stored_values)
                        if storage.is_sticky():
                            stored_values[storage] = expr
                        # Remove stored values for storages that might be aliases.
                        for storage2 in list(stored_values.keys()):
                            if storage != storage2 and storage.might_be_same(storage2):
                                del stored_values[storage2]

                    case operation:
                        bad_type(operation)

            # TODO: We should probably have a CodeNodeBuilder that wraps BasicBlockBuilder.
            setattr(block, "operations", tuple(operations))

        # Remember last known value for each storage.
        label = node.label
        assert label is not None
        for storage, value in stored_values.items():
            exit_values[storage][label] = value

        for idx, (cond, succ) in enumerate(node.outgoing):
            if (new_cond := cond.substitute(replacements.get)) is not cond:
                node.outgoing[idx] = (simplify_expression(new_cond), succ)

    # Replace known loaded values in returned bitstrings.
    for i, ret_bits in enumerate(returned):
        returned[i] = ret_bits.substitute(expression_func=replacements.get).simplify()


def _remove_unused_loads(entry: CodeNode, returned: list[BitString]) -> None:
    """Remove side-effect-free loads of which the LoadedValue is unused."""

    # Keep track of how often each LoadedValue is used.
    use_counts = defaultdict[LoadedValue, int](int)

    def update_counts(expr: Expression, delta: int = 1) -> None:
        for loaded in expr.iter_instances(LoadedValue):
            use_counts[loaded] += delta

    # Compute initial use counts.
    for node in walk_nodes(entry):
        if (block := node.block) is not None:
            for operation in block.operations:
                if isinstance(operation, Store):
                    update_counts(operation.expr)
                for expr in operation.storage.iter_expressions():
                    update_counts(expr)
        for cond, _succ in node.outgoing:
            update_counts(cond)

    for ret_bits in returned:
        for expr in ret_bits.iter_expressions():
            update_counts(expr)

    # Remove unnecesary Loads.
    for node in walk_nodes(entry):
        block = node.block
        if block is None:
            # TODO: Is it actually useful to have an optional block?
            #       We could have empty blocks instead.
            continue
        operations = list(block.operations)
        for i in range(len(operations) - 1, -1, -1):
            match operations[i]:
                case Load(expr=expr, storage=storage):
                    if use_counts[expr] == 0 and not storage.can_load_have_side_effect():
                        del operations[i]
                        # Update use_counts, so we can remove earlier Loads that
                        # became unused because the Load we just removed was
                        # the sole user of their LoadedValue.
                        for expr in storage.iter_expressions():
                            update_counts(expr, -1)
        # TODO: We should probably have a CodeNodeBuilder that wraps BasicBlockBuilder.
        setattr(block, "operations", tuple(operations))
