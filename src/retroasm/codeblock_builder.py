from __future__ import annotations

from abc import ABC, abstractmethod
from collections.abc import Callable, Iterable, Mapping, Sequence
from typing import ClassVar, NoReturn, Self, assert_never, override

from .codeblock import BasicBlock, FunctionBody, Load, Store
from .codeblock_simplifier import simplify_block
from .expression import Expression, ZeroTest
from .expression_simplifier import simplify_expression
from .function import Function
from .input import BadInput, ErrorCollector, InputLocation
from .reference import BitString, FixedValue, Reference, SingleStorage, Variable, bad_reference
from .storage import ArgStorage, IOStorage, Keeper, Register, Storage
from .types import IntType, mask_for_width


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


class _InitialValue(Expression):
    """
    Expression that represents the value of a traced variable at the start
    of a basic block.
    """

    __slots__ = ("_variable", "_block_id", "_location")

    @property
    def location(self) -> InputLocation | None:
        return self._location

    @property
    def name(self) -> str:
        return self._variable.name

    @property
    @override
    def mask(self) -> int:
        # Note that sign extension is added at the Reference level,
        # we only need to care about width here.
        return mask_for_width(self._variable.width)

    def __init__(self, variable: Variable, block_id: int, location: InputLocation | None):
        self._variable = variable
        self._block_id = block_id
        self._location = location
        Expression.__init__(self)

    @override
    def _ctorargs(self) -> tuple[Variable, int]:
        return (self._variable, self._block_id)

    @override
    def __str__(self) -> str:
        return f"(initial value of {self._variable} in block {self._block_id})"

    @override
    def _equals(self, other: _InitialValue) -> bool:
        return self._variable == other._variable and self._block_id == other._block_id

    @property
    @override
    def complexity(self) -> int:
        return 8


def _check_undefined(
    operations: Sequence[Load | Store], returned: Sequence[BitString], collector: ErrorCollector
) -> None:
    """Report uses of uninitialized local variables."""

    with collector.check():
        for operation in operations:
            if isinstance((storage := operation.storage), IOStorage):
                for value in storage.index.iter_instances(_InitialValue):
                    collector.error(
                        f'Undefined value of variable "{value.name}" is used as an I/O index',
                        location=value.location,
                    )
            if isinstance(operation, Store):
                for value in operation.expr.iter_instances(_InitialValue):
                    collector.error(
                        f'Undefined value of variable "{value.name}" is stored',
                        location=value.location,
                    )
        for ret_bits in returned:
            for expr in ret_bits.iter_expressions():
                for value in expr.iter_instances(_InitialValue):
                    collector.error(
                        f'Undefined value of variable "{value.name}" is returned',
                        location=value.location,
                    )


def returned_bits(ret_ref: Reference | None) -> Sequence[BitString]:
    return () if ret_ref is None else (ret_ref.bits,)


class CodeBlockBuilder(ABC):
    _next_block_id: ClassVar[int] = 0

    @classmethod
    def _create_block_id(cls) -> int:
        """Return a unique identifier for the block we're building."""
        block_id = cls._next_block_id
        cls._next_block_id = block_id + 1
        return block_id

    def __init__(self) -> None:
        super().__init__()
        self._block_id = self._create_block_id()
        self._variables: dict[str, Expression] = {}
        self._labels: dict[str, InputLocation | None] = {}
        self._branches: dict[str, list[InputLocation]] = {}

    def dump(self) -> None:
        """Prints the current state of this code block builder on stdout."""

    @abstractmethod
    def emit_load_bits(self, storage: Storage, location: InputLocation | None) -> Expression:
        """
        Loads the value from the given storage by emitting a Load node on
        this builder.
        Returns an expression that represents the loaded value.
        """

    @abstractmethod
    def emit_store_bits(
        self, storage: Storage, value: Expression, location: InputLocation | None
    ) -> None:
        """
        Stores the value of the given expression in the given storage by
        emitting a Store node on this builder.
        """

    def read_variable(self, var: Variable, location: InputLocation | None) -> Expression:
        name = var.name
        try:
            return self._variables[name]
        except KeyError:
            initial = _InitialValue(var, self._block_id, location)
            self._variables[name] = initial
            return initial

    def write_variable(
        self,
        var: Variable,
        value: Expression,
        location: InputLocation | None,  # pylint: disable=unused-argument
    ) -> None:
        self._variables[var.name] = value

    def add_label(self, label: str, location: InputLocation | None = None) -> None:
        """
        Add a label that identifies the current position in the code block.
        """
        if label in self._labels:
            old_location = self._labels[label]
            raise BadInput(f'label "{label}" already defined', location, old_location)
        else:
            self._labels[label] = location

    def add_branch(
        self,
        label: str,
        condition: Expression | None = None,
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

        # Force the condition to be computed.
        if condition is not None:
            ref = Reference(SingleStorage(Keeper(1)), IntType.u(1))
            ref.emit_store(self, ZeroTest(condition), condition_location)

    @abstractmethod
    def inline_function_call(
        self,
        func: Function,
        arg_map: Mapping[str, BitString],
        location: InputLocation | None = None,
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


class IllegalStateAccess(BadInput):
    """
    Raised when an operation is attempted that reads or writes state
    in a situation where that is not allowed.
    """

    def __init__(self, msg: str, location: InputLocation | None):
        locations: Sequence[InputLocation]
        if location is None:
            locations = ()
        else:
            locations = (location,)
        super().__init__(msg, *locations)


class StatelessCodeBlockBuilder(CodeBlockBuilder):
    """
    A CodeBlockBuilder that raises IllegalStateAccess when its users attempt
    touch any state, such as performing register access or I/O.
    """

    @override
    def emit_load_bits(self, storage: Storage, location: InputLocation | None) -> Expression:
        raise IllegalStateAccess(f"attempt to read state: {storage}", location)

    @override
    def emit_store_bits(
        self, storage: Storage, value: Expression, location: InputLocation | None
    ) -> None:
        raise IllegalStateAccess(f"attempt to write state: {storage}", location)

    @override
    def inline_function_call(
        self,
        func: Function,
        arg_map: Mapping[str, BitString | None],
        location: InputLocation | None = None,
    ) -> BitString | None:
        # TODO: This is probably overly strict: calling a function that does
        #       not touch state should be fine.
        raise IllegalStateAccess("attempt to call function", location)


class SemanticsCodeBlockBuilder(CodeBlockBuilder):
    @classmethod
    def with_stored_values(cls, stored_values: Mapping[Storage, Expression]) -> Self:
        builder = cls()
        builder._stored_values = dict(stored_values)
        return builder

    def __init__(self) -> None:
        super().__init__()
        self.operations: list[Load | Store] = []
        self._stored_values: dict[Storage, Expression] = {}

    @override
    def dump(self) -> None:
        for operation in self.operations:
            operation.dump()
        super().dump()

    def _check_labels(self, collector: ErrorCollector) -> None:
        """
        Verify that labels are used correctly.
        Undefined labels are logged as errors, unused labels as warnings.
        Raises `BadInput` for each undefined label that is branched to.
        """

        with collector.check():
            defined_labels = self._labels.keys()
            unused_labels = set(defined_labels)
            for label, locations in self._branches.items():
                if label in defined_labels:
                    unused_labels.remove(label)
                else:
                    collector.error(f'Label "{label}" does not exist', location=locations)
            for label in unused_labels:
                collector.warning(f'Label "{label}" is unused', location=self._labels[label])

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

        self._check_labels(collector)

        def fixate_variable(var: Variable) -> FixedValue:
            value = self.read_variable(var, location)
            return FixedValue(value, var.width)

        # Fixate returned variables.
        returned = [bits.substitute(variable_func=fixate_variable) for bits in returned]

        operations = self.operations
        simplify_block(operations, returned)

        _check_undefined(operations, returned, collector)

        block = BasicBlock(operations)
        return FunctionBody(block, returned)

    @override
    def emit_load_bits(self, storage: Storage, location: InputLocation | None) -> Expression:
        stored_values = self._stored_values
        storage = _simplify_storage(storage)

        if storage.can_load_have_side_effect():
            self._handle_side_effects(storage)
        elif (value := stored_values.get(storage)) is not None:
            # Use known value instead of loading it.
            return value

        load = Load(storage, location)
        self.operations.append(load)
        value = load.expr
        if storage.is_load_consistent():
            # Remember loaded value.
            stored_values[storage] = value
        return value

    @override
    def emit_store_bits(
        self, storage: Storage, value: Expression, location: InputLocation | None
    ) -> None:
        stored_values = self._stored_values
        storage = _simplify_storage(storage)

        # Simplifying gets rid of unnecessary truncation.
        value = simplify_expression(value)

        if storage.can_store_have_side_effect():
            self._handle_side_effects(storage)
        elif stored_values.get(storage) == value:
            # Current value is rewritten.
            return

        self.operations.append(Store(value, storage, location))
        if storage.is_sticky():
            # Remember stored value.
            stored_values[storage] = value

        # Remove stored values for storages that might be aliases.
        for storage2 in list(stored_values.keys()):
            if storage != storage2 and storage.might_be_same(storage2):
                # However, if the store wouldn't alter the value,
                # there is no need to remove it.
                if stored_values[storage2] != value:
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
                if not isinstance(storage2, Register):
                    del stored_values[storage2]

    @override
    def inline_function_call(
        self,
        func: Function,
        arg_map: Mapping[str, BitString],
        location: InputLocation | None = None,
    ) -> BitString | None:
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


def decompose_store(ref: Reference, value: Expression) -> Mapping[Storage, Expression]:
    """
    Decompose the storing of a value to a reference.
    Returns the values written to the underlying storages.
    """

    # Generate code for storing the value.
    builder = SemanticsCodeBlockBuilder()
    ref.emit_store(builder, value, None)

    # Derive storage mapping from generated store nodes.
    mapping = {}
    for operation in builder.operations:
        assert isinstance(operation, Store), operation
        mapping[operation.storage] = operation.expr
    return mapping
