from __future__ import annotations

from collections.abc import Iterable, Mapping, Sequence, Set
from dataclasses import dataclass, field
from typing import Final, override

from .expression import Expression
from .input import InputLocation
from .reference import BitString, Variable
from .storage import ArgStorage, Storage
from .types import mask_for_width
from .utils import const_property


@dataclass(frozen=True, slots=True, eq=False)
class Load:
    """An operation that loads a value from a storage location."""

    storage: Storage
    location: InputLocation | None = None
    expr: LoadedValue = field(init=False, repr=False)

    def __post_init__(self) -> None:
        object.__setattr__(self, "expr", LoadedValue(self, mask_for_width(self.storage.width)))

    @override
    def __str__(self) -> str:
        return f"load from {self.storage}"

    def dump(self) -> None:
        print(f"    {self} ({self.storage.width}-bit)")


@dataclass(frozen=True, slots=True, eq=False)
class Store:
    """An operation that stores a value into a storage location."""

    expr: Expression
    storage: Storage
    location: InputLocation | None = None

    @override
    def __str__(self) -> str:
        return f"store {self.expr} in {self.storage}"

    def dump(self) -> None:
        print(f"    {self} ({self.storage.width}-bit)")


class LoadedValue(Expression):
    """A value loaded from a storage location."""

    __slots__ = ("_load", "_mask")

    @property
    def load(self) -> Load:
        return self._load

    @property
    @override
    def mask(self) -> int:
        return self._mask

    def __init__(self, load: Load, mask: int):
        Expression.__init__(self)
        self._load = load
        self._mask = mask

    @override
    def _ctorargs(self) -> tuple[Load, int]:
        return self._load, self._mask

    @override
    def __repr__(self) -> str:
        return f"LoadedValue({self._load!r}, {self._mask:d})"

    @override
    def __str__(self) -> str:
        return f"load({self._load.storage})"

    @override
    def _equals(self, other: LoadedValue) -> bool:
        return self._load is other._load

    @property
    @override
    def complexity(self) -> int:
        # Since loaded values are only available at runtime, they are not
        # desirable in analysis, so assign a high cost to them.
        return 8


def verify_loads(
    operations: Iterable[Load | Store], returned: Iterable[BitString] = ()
) -> bool:
    """
    Performs consistency checks on the LoadedValues in the given operations and
    returned bit strings.
    Raises AssertionError if an inconsistency is found.
    Returns True on success, never returns False.
    """

    # Check that every LoadedValue has an associated Load operation, which must
    # execute before the LoadedValue is used.
    loads: set[Load] = set()
    for operation in operations:
        # Check that all expected loads have occurred.
        match operation:
            case Store(expr=expr):
                for value in expr.iter_instances(LoadedValue):
                    assert value.load in loads, value
        for expr in operation.storage.iter_expressions():
            for value in expr.iter_instances(LoadedValue):
                assert value.load in loads, value
        # Remember this load.
        match operation:
            case Load() as load:
                loads.add(load)

    # Check I/O indices in the returned bit string.
    for ret_bits in returned:
        for storage in ret_bits.iter_storages():
            for expr in storage.iter_expressions():
                for value in expr.iter_instances(LoadedValue):
                    assert value.load in loads, value

    return True


class BasicBlock:
    """
    A sequence of load/store operations without any branches.
    """

    __slots__ = ("operations", "storages", "arguments")

    def __init__(self, operations: Iterable[Load | Store]):
        operations = tuple(operations)
        assert verify_loads(operations)
        self.operations: Final[Sequence[Load | Store]] = operations
        """The load/store operations in this block."""

        storages = {operation.storage for operation in operations}
        self.storages: Final[Set[Storage]] = storages
        """A set of all storages that are accessed by this block."""

        # Reject multiple arguments with the same name.
        arguments = _find_arguments(storages)
        self.arguments: Final[Mapping[str, ArgStorage]] = arguments
        """All arguments that occur in this block, mapped by name."""

    def dump(self) -> None:
        """Print this basic block on stdout."""
        for operation in self.operations:
            operation.dump()


def _find_arguments(storages: Iterable[Storage]) -> Mapping[str, ArgStorage]:
    """
    A name to storage mapping containing all arguments among the given storages.
    ValueError is raised if the same name is used for multiple arguments.
    """
    args: dict[str, ArgStorage] = {}
    for storage in storages:
        match storage:
            case ArgStorage(name=name) as arg:
                if args.setdefault(name, arg) is not arg:
                    raise ValueError(f'multiple arguments named "{name}"')
    return args


class InitialValue(Expression):
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
    def _equals(self, other: InitialValue) -> bool:
        return self._variable == other._variable and self._block_id == other._block_id

    @property
    @override
    def complexity(self) -> int:
        return 8


class FunctionBody:
    """
    A code block with returned bit strings.
    """

    __slots__ = ("_block", "_returned", "_storages", "_arguments")

    def __init__(self, operations: Iterable[Load | Store], returned: Iterable[BitString]):
        self._block = BasicBlock(operations)
        self._returned = list(returned)
        assert verify_loads(self.operations, self._returned)

    def dump(self) -> None:
        """Print this function body on stdout."""
        self._block.dump()
        for ret_bits in self._returned:
            print(f"    return {ret_bits}")

    @property
    def block(self) -> BasicBlock:
        return self._block

    @property
    def operations(self) -> Sequence[Load | Store]:
        return self._block.operations

    @property
    def returned(self) -> Sequence[BitString]:
        return self._returned

    @const_property
    def storages(self) -> Set[Storage]:
        """
        A set of all storages that are accessed or referenced by this function body.
        """
        storages = set(self._block.storages)
        for ret_bits in self._returned:
            storages.update(ret_bits.iter_storages())
        return storages

    @const_property
    def arguments(self) -> Mapping[str, ArgStorage]:
        args = dict(self._block.arguments)
        for ret in self._returned:
            for storage in ret.iter_storages():
                if isinstance(storage, ArgStorage):
                    args[storage.name] = storage
        return args
