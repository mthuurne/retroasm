from __future__ import annotations

from collections.abc import Callable, Iterable, Mapping, Sequence
from typing import IO, NoReturn, Self, assert_never

from .codeblock import BasicBlock, CodeGraph, CodeNode, FunctionBody, Load, Store
from .codeblock_simplifier import simplify_block
from .expression import Expression, IntLiteral, ZeroTest, is_literal_false
from .expression_simplifier import simplify_expression
from .function import Function
from .input import BadInput, ErrorCollector, InputLocation
from .reference import BitString, FixedValue, Reference, SingleStorage, bad_reference
from .storage import ArgStorage, IOStorage, Register, Storage, Variable


def no_args_to_fetch(name: str) -> NoReturn:
    """Argument fetcher that can be used when a function has no arguments."""
    raise ValueError(f'No arguments provided, "{name}" requested')


def _simplify_storage(storage: Storage) -> Storage:
    """
    Return a simplified version of the given storage.
    If no simplification is possible, the given storage is returned.
    """
    if isinstance(storage, IOStorage):
        original_index = storage.index
        simplified_index = simplify_expression(original_index)
        if simplified_index is not original_index:
            return IOStorage(storage.channel, simplified_index)
    return storage


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
        condition_location: InputLocation | None = None,
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

        # Fixate returned variables.
        returned = [bits.substitute(storage_func=fixate_variable) for bits in returned]

        operations = self._current_block._operations
        simplify_block(operations, returned)

        _check_undefined(operations, collector)

        self._complete_node()

        self._check_labels(collector)

        code = CodeGraph(self._entry)

        # Add a synthetic label to each unlabeled node.
        label_idx = 0
        for node in code.iter_nodes():
            if node.label is None:
                node.label = str(label_idx)
                label_idx += 1

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
        storage = _simplify_storage(storage)

        if storage.can_load_have_side_effect():
            self._handle_side_effects(storage)
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
        storage = _simplify_storage(storage)

        # Simplifying gets rid of unnecessary truncation.
        value = simplify_expression(value)

        if storage.can_store_have_side_effect():
            self._handle_side_effects(storage)
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

    def _handle_side_effects(self, storage: Storage) -> None:
        """
        Drop stored values that might be invalidated by an I/O side effect.
        """
        stored_values = self._stored_values
        if isinstance(storage, (IOStorage, ArgStorage)):
            # A load or store side effect on an I/O channel can affect other addresses
            # and even other channels, but it wouldn't affect registers.
            for storage2 in list(stored_values.keys()):
                if not isinstance(storage2, (Register, Variable)):
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
