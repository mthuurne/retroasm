from __future__ import annotations

from collections.abc import Iterable, Iterator, Mapping, Sequence, Set
from dataclasses import dataclass, field
from typing import IO, Final, override

from .expression import Expression, is_literal_true
from .input import InputLocation
from .reference import BitString
from .storage import ArgStorage, Storage
from .types import mask_for_width


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

    def dump(self, *, file: IO[str] | None = None) -> None:
        print(f"    {self}", file=file)


@dataclass(frozen=True, slots=True, eq=False)
class Store:
    """An operation that stores a value into a storage location."""

    expr: Expression
    storage: Storage
    location: InputLocation | None = None

    @override
    def __str__(self) -> str:
        return f"store {self.expr} in {self.storage}"

    def dump(self, *, file: IO[str] | None = None) -> None:
        print(f"    {self}", file=file)


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

    TODO: Update this for code graphs.
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

    __slots__ = ("operations", "storages", "traceables")

    def __init__(self, operations: Iterable[Load | Store]):
        operations = tuple(operations)
        self.operations: Final[Sequence[Load | Store]] = operations
        """The load/store operations in this block."""

        storages = {operation.storage for operation in operations}
        self.storages: Final[Set[Storage]] = storages
        """A set of all storages that are accessed by this block."""

        traceables = {storage for storage in storages if storage.traceable}
        self.traceables: Final[Set[Storage]] = traceables
        """A set of all traceable storages that are accessed by this block."""

    def dump(self, *, file: IO[str] | None = None) -> None:
        """Print this basic block."""
        for operation in self.operations:
            operation.dump(file=file)


class CodeGraph:
    __slots__ = ("entry",)

    def __init__(self, entry: CodeNode):
        self.entry: Final[CodeNode] = entry
        """The entry point for this code graph."""

        # TODO: Verify loads across the graph.

    def iter_blocks(self) -> Iterator[BasicBlock]:
        for node in walk_nodes(self.entry):
            yield node.block

    @property
    def operations(self) -> Sequence[Load | Store]:
        # TODO: What do callers use this for?
        return [operation for block in self.iter_blocks() for operation in block.operations]

    def dump(self, *, file: IO[str] | None = None) -> None:
        """Print this code graph."""

        nodes = list(walk_nodes(self.entry))
        for idx, node in enumerate(nodes):
            if idx != 0 or any(out is node for node2 in nodes for _cond, out in node2.outgoing):
                print(f"@{idx:d}", file=file)
            node.block.dump(file=file)
            if node.outgoing:
                verb = "goto"
                for condition, out_node in node.outgoing:
                    cond = "" if is_literal_true(condition) else f" if {condition}"
                    print(f"    {verb} @{nodes.index(out_node):d}{cond}", file=file)
                    verb = " " * len(verb)


class CodeNode:
    __slots__ = ("_block", "_incoming", "_outgoing")

    @property
    def block(self) -> BasicBlock:
        return self._block

    @property
    def incoming(self) -> Sequence[CodeNode]:
        return self._incoming

    @property
    def outgoing(self) -> Sequence[tuple[Expression, CodeNode]]:
        return self._outgoing

    def __init__(self, block: BasicBlock):
        self._block: BasicBlock = block
        self._incoming: list[CodeNode] = []
        self._outgoing: list[tuple[Expression, CodeNode]] = []


def walk_nodes(entry: CodeNode) -> Iterator[CodeNode]:
    # Note: For the tiny graphs of single instructions, lists are fast enough.
    #       If we ever start building much larger graphs, reconsider.
    done = []
    pending = [entry]
    while pending:
        node = pending.pop()
        done.append(node)
        for _condition, out_node in node.outgoing:
            if out_node not in done and out_node not in pending:
                if out_node.outgoing:
                    pending.append(out_node)
                else:
                    # Place the exit node last.
                    pending.insert(0, out_node)
        yield node


class FunctionBody:
    """
    A code block with returned bit strings.
    """

    __slots__ = ("code", "returned", "arguments")

    def __init__(self, code: CodeGraph, returned: Iterable[BitString]):
        returned = tuple(returned)
        storages = {storage for block in code.iter_blocks() for storage in block.storages}
        storages.update(storage for ret in returned for storage in ret.iter_storages())

        self.code: Final[CodeGraph] = code
        self.returned: Final[Sequence[BitString]] = returned

        # Compute the mapping now because _find_arguments() will reject multiple arguments
        # with the same name.
        self.arguments: Final[Mapping[str, ArgStorage]] = dict(_find_arguments(storages))
        """The arguments that occur in this function body, mapped by name."""

        assert verify_loads(self.operations, self.returned)

    def dump(self, *, file: IO[str] | None = None) -> None:
        """Print this function body."""

        self.code.dump(file=file)
        for ret_bits in self.returned:
            print(f"    return {ret_bits}", file=file)

    @property
    def operations(self) -> Sequence[Load | Store]:
        return self.code.operations


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
