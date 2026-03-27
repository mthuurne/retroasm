from __future__ import annotations

from collections import defaultdict
from collections.abc import Callable, Iterable, Mapping, Sequence
from dataclasses import dataclass, field
from typing import IO, NoReturn, Self, assert_never

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
    OrOperator,
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
from .utils import bad_type


def no_args_to_fetch(name: str) -> NoReturn:
    """Argument fetcher that can be used when a function has no arguments."""
    raise ValueError(f'No arguments provided, "{name}" requested')


def _check_undefined(entry: CodeNode, collector: ErrorCollector) -> None:
    """Report uses of uninitialized local variables."""

    with collector.check():
        # TODO: Until we trace looped variables, there will be false positives here.
        for node in walk_nodes(entry):
            for operation in node.block.operations:
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


class CodeGraphBuilder:
    @classmethod
    def with_stored_values(cls, stored_values: Mapping[Storage, Expression]) -> Self:
        builder = cls()
        builder._nodes[0].block._stored_values = dict(stored_values)
        return builder

    def __init__(self) -> None:
        self._nodes: list[CodeNodeBuilder] = []
        self._add_node()
        self._labels: dict[str, CodeNodeBuilder] = {}
        self._branches: dict[str, list[InputLocation]] = {}

    def _add_node(self) -> CodeNodeBuilder:
        """Create a new node builder and add it to the end of the nodes list."""
        node = CodeNodeBuilder()
        self._nodes.append(node)
        return node

    def dump(self, *, file: IO[str] | None = None) -> None:
        """Print the code graph under construction."""

        for node in self._nodes:
            print(" ".join(f"@{label}" for label in node.labels), file=file)
            node.block.dump(file=file)
            if node.branch:
                condition, out_label = node.branch
                cond = "" if is_literal_true(condition) else f" if {condition}"
                print(f"    goto @{out_label}{cond}", file=file)

    def _check_labels(self, collector: ErrorCollector) -> None:
        """
        Verify that labels are used correctly.
        Undefined labels are logged as errors, unused labels as warnings.
        Raises `BadInput` for each undefined label that is branched to.
        """

        with collector.check():
            for label, location in self._branches.items():
                if label not in self._labels:
                    collector.error(f'Label "{label}" does not exist', location=location)
            for node in self._nodes:
                for label in node.labels:
                    if label not in self._branches:
                        collector.warning(f'Label "{label}" is unused', location=node.location)

    def emit_load_bits(self, storage: Storage, location: InputLocation | None) -> Expression:
        """
        Loads the value from the given storage by emitting a Load node on
        this builder.
        Returns an expression that represents the loaded value.
        """
        node = self._nodes[-1]
        if node.location is None:
            node.location = location
        return node.block.emit_load_bits(storage, location)

    def emit_store_bits(
        self, storage: Storage, value: Expression, location: InputLocation | None
    ) -> None:
        """
        Stores the value of the given expression in the given storage by
        emitting a Store node on this builder.
        """
        node = self._nodes[-1]
        if node.location is None:
            node.location = location
        node.block.emit_store_bits(storage, value, location)

    def emit_label(self, label: str, location: InputLocation | None = None) -> None:
        """
        Add a label that identifies the current position in the code block.
        Raises `BadInput` if the label was already defined.
        """

        if (node := self._labels.get(label)) is not None:
            raise BadInput(f'label "{label}" already defined', location, node.location)

        node = self._nodes[-1]
        if node.empty:
            # Add label to fresh node.
            node.labels.append(label)
            if node.location is None:
                node.location = location
        else:
            # Start a new node.
            node = self._add_node()
            node.labels.append(label)
            node.location = location
        self._labels[label] = node

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

        # Record the branch condition + target.
        node_from = self._nodes[-1]
        assert node_from.branch is None
        condition_bool = simplify_expression(ZeroTest(ZeroTest(condition)))
        node_from.branch = (condition_bool, label)

        # Branches can only occur after a basic block, so start a new node.
        self._add_node()

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

        # Fixate returned variables.
        returned = [bits.substitute(storage_func=fixate_variable) for bits in returned]

        # Simplify returned expressions.
        # This can also help find additional unused loads, if a loaded value is dropped
        # during simplification because it doesn't affect the expression's value.
        # TODO: Is this still needed as a separate step?
        _update_expressions_in_bitstrings(returned)

        self._check_labels(collector)

        entry = _create_graph(self._nodes)

        # Try several simplifications in successing until no more changes are made.
        changed = True
        while changed:
            changed = False
            changed |= _trace_stored_values(entry, returned)
            changed |= _remove_overwritten_stores(entry)
            changed |= _remove_unused_loads(entry, returned)
            changed |= _remove_variable_stores(entry)
            entry, reduced = _reduce_graph(entry)
            changed |= reduced

        _check_undefined(entry, collector)

        return FunctionBody(CodeGraph(entry), returned)


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

        return BasicBlock(self._operations)


@dataclass(slots=True, kw_only=True)
class CodeNodeBuilder:
    block: BasicBlockBuilder = field(default_factory=BasicBlockBuilder)
    labels: list[str] = field(default_factory=list)
    location: InputLocation | None = None
    branch: tuple[Expression, str] | None = None

    @property
    def empty(self) -> bool:
        """Is this a node without operations?"""
        return not self.block._operations


def decompose_store(ref: Reference, value: Expression) -> Mapping[Storage, Expression]:
    """
    Decompose the storing of a value to a reference.
    Returns the values written to the underlying storages.
    """

    # Generate code for storing the value.
    builder = CodeGraphBuilder()
    ref.emit_store(builder, value, None)

    # Derive storage mapping from generated store nodes.
    mapping = {}
    for operation in builder._nodes[-1].block._operations:
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


def _remove_variable_stores(entry: CodeNode) -> bool:
    """
    Remove stores to local variables.
    Local variables don't exist after exiting the block, so once their values have
    been traced, we don't need the stores anymore.
    """

    # Check which variables are still being loaded.
    vars_to_keep = set()
    for node in walk_nodes(entry):
        for operation in node._block.operations:
            match operation:
                case Load(storage=Variable() as storage):
                    vars_to_keep.add(storage)

    # Remove stores to all other variables.
    changed = False
    for node in walk_nodes(entry):
        idx_to_remove = []
        for idx, operation in enumerate(node._block.operations):
            match operation:
                case Store(storage=Variable() as storage):
                    if storage not in vars_to_keep:
                        idx_to_remove.append(idx)
        if idx_to_remove:
            changed = True
            operations = list(node._block.operations)
            for idx in reversed(idx_to_remove):
                del operations[idx]
            node._block = BasicBlock(operations)
    return changed


def _remove_overwritten_stores(entry: CodeNode) -> bool:
    """
    Remove side-effect-free stores that will be overwritten.

    TODO: Only overwrites within the same basic block are currently considered,
          but overwrites in nodes that are guaranteed to be executed later should
          also be taken into account.
          However, the removal is only valid if there are no possible intermediate
          loads; within a single block those loads are replaced by value tracing,
          but between nodes that is not always feasible.
    """

    changed = False
    for node in walk_nodes(entry):
        operations = node._block.operations
        will_be_overwritten = set()
        idx_to_remove = []
        for idx in range(len(operations) - 1, -1, -1):
            operation = operations[idx]
            storage = operation.storage
            if not storage.can_store_have_side_effect():
                match operation:
                    case Load():
                        will_be_overwritten.discard(storage)
                    case Store():
                        if storage in will_be_overwritten:
                            idx_to_remove.append(idx)
                        else:
                            will_be_overwritten.add(storage)
        if idx_to_remove:
            changed = True
            operations = list(operations)
            for idx in idx_to_remove:
                del operations[idx]
            node._block = BasicBlock(operations)
    return changed


def _trace_stored_values(entry: CodeNode, returned: list[BitString]) -> bool:
    """
    Trace the value of registers and local variables.
    This simplification step replaces `LoadedValue` uses by known expressions,
    which could also be `LoadedValue` expressions from earlier loads.
    Removal of loads of which the value is no longer used can be done in a later step.
    """

    exit_values: defaultdict[Storage, dict[CodeNode, Expression]] = defaultdict(dict)
    replacements: dict[Expression, Expression] = {}
    changed = False

    for node in walk_nodes(entry):
        # Gather storage value known at the start of this basic block.
        stored_values: dict[Storage, Expression] = {}
        for storage, value_by_node in exit_values.items():
            incoming_values = []
            for inc_node in node.incoming:
                value = value_by_node.get(inc_node)
                if value is None:
                    break
                incoming_values.append(value)
            else:
                stored_values[storage] = simplify_expression(Select(*incoming_values))

        patches: list[tuple[int, Load | Store]] = []
        for op_idx, operation in enumerate(node._block.operations):
            expr: Expression
            match operation:
                case Load(storage=old_storage, expr=expr):
                    storage = old_storage.substitute_expressions(replacements.get).simplify()
                    if storage is not old_storage:
                        load = Load(storage)
                        replacements[expr] = load.expr
                        patches.append((op_idx, load))
                    if storage.can_load_have_side_effect():
                        _handle_side_effects(storage, stored_values)
                    elif (value := stored_values.get(storage)) is not None:
                        replacements[expr] = value
                        continue
                    if storage.is_load_consistent():
                        stored_values[storage] = expr

                case Store(storage=old_storage, expr=old_expr):
                    expr = simplify_expression(old_expr.substitute(replacements.get))
                    storage = old_storage.substitute_expressions(replacements.get).simplify()
                    if expr is not old_expr or storage is not old_storage:
                        store = Store(expr, storage)
                        patches.append((op_idx, store))
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

        # Apply mutations, if any.
        if patches:
            changed = True
            operations = list(node._block.operations)
            for op_idx, operation in patches:
                operations[op_idx] = operation
            node._block = BasicBlock(operations)

        # Remember last known value for each storage.
        for storage, value in stored_values.items():
            exit_values[storage][node] = value

        for out_idx, (cond, succ) in enumerate(node._outgoing):
            if (new_cond := cond.substitute(replacements.get)) is not cond:
                changed = True
                node._outgoing[out_idx] = (simplify_expression(new_cond), succ)

    # Replace known loaded values in returned bitstrings.
    for i, ret_bits in enumerate(returned):
        returned[i] = ret_bits.substitute(expression_func=replacements.get).simplify()

    return changed


def _remove_unused_loads(entry: CodeNode, returned: list[BitString]) -> bool:
    """Remove side-effect-free loads of which the LoadedValue is unused."""

    # Keep track of how often each LoadedValue is used.
    use_counts = defaultdict[LoadedValue, int](int)

    def update_use_counts(expr: Expression) -> None:
        for loaded in expr.iter_instances(LoadedValue):
            use_counts[loaded] += 1

    # Compute initial use counts.
    for node in walk_nodes(entry):
        for operation in node.block.operations:
            if isinstance(operation, Store):
                update_use_counts(operation.expr)
            for expr in operation.storage.iter_expressions():
                update_use_counts(expr)
        for cond, _succ in node.outgoing:
            update_use_counts(cond)

    for ret_bits in returned:
        for expr in ret_bits.iter_expressions():
            update_use_counts(expr)

    # Remove unnecesary Loads.
    any_changed = False
    for node in walk_nodes(entry):
        changed = False
        operations = list(node.block.operations)
        for i in range(len(operations) - 1, -1, -1):
            match operations[i]:
                case Load(expr=loaded, storage=storage):
                    if use_counts[loaded] == 0 and not storage.can_load_have_side_effect():
                        del operations[i]
                        changed = True
                        any_changed = True
        if changed:
            node._block = BasicBlock(operations)

    return any_changed


def _create_graph(builders: Iterable[CodeNodeBuilder]) -> CodeNode:
    """Create final code graph from node builders."""

    labels = {label: idx for idx, builder in enumerate(builders) for label in builder.labels}
    nodes = [CodeNode(builder.block.create_basic_block()) for builder in builders]
    for idx, (node, builder) in enumerate(zip(nodes, builders, strict=True)):
        if branch := builder.branch:
            cond, out_label = branch
            out_node = nodes[labels[out_label]]
            node._outgoing.append((cond, out_node))
            out_node._incoming.append(node)
            cond_else = simplify_expression(ZeroTest(cond))
        else:
            cond_else = IntLiteral(1)
        if not is_literal_false(cond_else) and idx + 1 < len(nodes):
            else_node = nodes[idx + 1]
            node._outgoing.append((cond_else, else_node))
            else_node._incoming.append(node)
    return nodes[0]


def _simplify_outgoing(node: CodeNode) -> bool:
    """
    Combine overlapping outgoing edges.
    """
    destinations = defaultdict(list)
    for cond, out in node.outgoing:
        if not is_literal_false(cond):
            destinations[out].append(cond)
    if len(destinations) < len(node.outgoing):
        node._outgoing[:] = [
            (simplify_expression(OrOperator(*exprs)), out)
            for out, exprs in destinations.items()
        ]
        return True
    else:
        return False


def _reduce_graph(entry: CodeNode) -> tuple[CodeNode, bool]:
    """
    Reduce the number of nodes in the given graph, if possible.
    Empty nodes with a single fixed successor are removed.
    """
    changed = False
    done = set()
    remaining = {entry}
    while remaining:
        node = remaining.pop()
        changed |= _simplify_outgoing(node)

        # Merge nodes:
        # - an empty node that flows unconditionally into a single other node
        # - any two nodes with a single unconditional edge between them
        if len(node.outgoing) == 1:
            ((cond, new_succ_node),) = node.outgoing
            first_ops = node.block.operations
            if is_literal_true(cond) and (
                not first_ops
                or (len(new_succ_node.incoming) == 1 and new_succ_node is not entry)
            ):
                changed = True
                for prev_node in node.incoming:
                    for idx, (cond, old_succ_node) in enumerate(prev_node.outgoing):
                        if old_succ_node is node:
                            prev_node._outgoing[idx] = (cond, new_succ_node)
                    _simplify_outgoing(prev_node)
                # TODO: Once we start caring about the Select indices, this incoming edges
                #       merge has to be performed more carefully.
                new_succ_node._incoming.remove(node)
                for inc in node.incoming:
                    if inc not in new_succ_node._incoming:
                        new_succ_node._incoming.append(inc)
                if first_ops:
                    merged_ops = list(first_ops)
                    merged_ops += new_succ_node.block.operations
                    new_succ_node._block = BasicBlock(merged_ops)
                if node is entry:
                    entry = new_succ_node
        done.add(node)
        for _cond, out in node.outgoing:
            if out not in done:
                remaining.add(out)
    return (entry, changed)
