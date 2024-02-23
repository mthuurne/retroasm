from __future__ import annotations

from collections.abc import Callable, Iterable, Mapping, Sequence, Set
from typing import cast

from .expression import Expression
from .parser.linereader import InputLocation
from .reference import BitString
from .storage import ArgStorage, Storage
from .types import mask_for_width
from .utils import const_property


class AccessNode:
    """Base class for Load and Store."""

    __slots__ = ("_expr", "_storage", "_location")

    def __init__(
        self, expr: Expression, storage: Storage, location: InputLocation | None
    ):
        self._expr = expr
        self._storage = storage
        self._location = location

    @property
    def expr(self) -> Expression:
        return self._expr

    @expr.setter
    def expr(self, expr: Expression) -> None:
        self._expr = expr

    @property
    def storage(self) -> Storage:
        return self._storage

    @storage.setter
    def storage(self, storage: Storage) -> None:
        self._storage = storage

    @property
    def location(self) -> InputLocation | None:
        return self._location

    def dump(self) -> None:
        print(f"    {self} ({self._storage.width}-bit)")

    def clone(self) -> AccessNode:
        """Create a clone of this node."""
        raise NotImplementedError


class Load(AccessNode):
    """A node that loads a value from a storage location."""

    __slots__ = ()

    def __init__(self, storage: Storage, location: InputLocation | None = None):
        expr = LoadedValue(self, mask_for_width(storage.width))
        AccessNode.__init__(self, expr, storage, location)

    def __repr__(self) -> str:
        return f"Load({self._storage!r}, {self._location!r})"

    def __str__(self) -> str:
        return f"load from {self._storage}"

    @property  # type: ignore[override]  # https://github.com/python/mypy/issues/14301
    def expr(self) -> LoadedValue:
        return cast(LoadedValue, self._expr)

    @expr.setter
    def expr(self, expr: Expression) -> None:
        raise AttributeError("Cannot change expression for Load nodes")

    def clone(self) -> Load:
        return Load(self._storage, self._location)


class Store(AccessNode):
    """A node that stores a value into a storage location."""

    __slots__ = ()

    def __init__(
        self, expr: Expression, storage: Storage, location: InputLocation | None = None
    ):
        AccessNode.__init__(self, expr, storage, location)

    def __repr__(self) -> str:
        return f"Store({self._expr!r}, {self._storage!r}, {self._location!r})"

    def __str__(self) -> str:
        return f"store {self._expr} in {self._storage}"

    def clone(self) -> Store:
        return Store(self._expr, self._storage, self._location)


class LoadedValue(Expression):
    """A value loaded from a storage location."""

    __slots__ = ("_load", "_mask")

    @property
    def load(self) -> Load:
        return self._load

    @property
    def mask(self) -> int:
        return self._mask

    def __init__(self, load: Load, mask: int):
        Expression.__init__(self)
        self._load = load
        self._mask = mask

    def _ctorargs(self) -> tuple[Load, int]:
        return self._load, self._mask

    def __repr__(self) -> str:
        return f"LoadedValue({self._load!r}, {self._mask:d})"

    def __str__(self) -> str:
        return f"load({self._load.storage})"

    def _equals(self, other: LoadedValue) -> bool:
        # pylint: disable=protected-access
        return self._load is other._load

    @property
    def complexity(self) -> int:
        # Since loaded values are only available at runtime, they are not
        # desirable in analysis, so assign a high cost to them.
        return 8


def verify_loads(
    nodes: Iterable[AccessNode], returned: Iterable[BitString] = ()
) -> bool:
    """
    Performs consistency checks on the LoadedValues in the given nodes and
    returned bit strings.
    Raises AssertionError if an inconsistency is found.
    Returns True on success, never returns False.
    """

    # Check that every LoadedValue has an associated Load node, which must
    # execute before the LoadedValue is used.
    loads: set[Load] = set()
    for node in nodes:
        # Check that all expected loads have occurred.
        match node:
            case Store(expr=expr):
                for value in expr.iter_instances(LoadedValue):
                    assert value.load in loads, value
        for expr in node.storage.iter_expressions():
            for value in expr.iter_instances(LoadedValue):
                assert value.load in loads, value
        # Remember this load.
        match node:
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

    __slots__ = ("_nodes", "_storages", "_arguments", "_value_mapping")

    def __init__(self, nodes: Iterable[AccessNode]):
        cloned_nodes = []
        value_mapping: dict[Expression, Expression] = {}
        for node in nodes:
            clone = node.clone()
            cloned_nodes.append(clone)
            if isinstance(node, Load):
                value_mapping[node.expr] = clone.expr

        # TODO: I don't think the value mapping should be here, but for now
        #       it's the easiest way to pass it to FunctionBody.
        self._value_mapping = value_mapping

        update_expressions_in_nodes(cloned_nodes, value_mapping.get)
        assert verify_loads(cloned_nodes)
        self._nodes = cloned_nodes

        # Reject multiple arguments with the same name.
        self.arguments  # pylint: disable=pointless-statement

    def dump(self) -> None:
        """Print this basic block on stdout."""
        for node in self._nodes:
            node.dump()

    @property
    def nodes(self) -> Sequence[AccessNode]:
        return self._nodes

    @const_property
    def storages(self) -> Set[Storage]:
        """A set of all storages that are accessed by this block."""
        return {node.storage for node in self._nodes}

    @const_property
    def arguments(self) -> Mapping[str, ArgStorage]:
        """
        A mapping containing all arguments that occur in this basic block.
        ValueError is raised if the same name is used for multiple arguments.
        """
        args: dict[str, ArgStorage] = {}
        for storage in self.storages:
            match storage:
                case ArgStorage(name=name) as arg:
                    if args.setdefault(name, arg) is not arg:
                        raise ValueError(f'multiple arguments named "{name}"')
        return args


class FunctionBody:
    """
    A code block with returned bit strings.
    """

    __slots__ = ("_block", "_returned", "_storages")

    def __init__(self, nodes: Iterable[AccessNode], returned: Iterable[BitString]):
        self._block = BasicBlock(nodes)
        self._returned = list(returned)
        value_mapping = self._block._value_mapping
        update_expressions_in_bitstrings(self._returned, value_mapping.get)
        assert verify_loads(self.nodes, self._returned)

    def dump(self) -> None:
        """Print this function body on stdout."""
        self._block.dump()
        for ret_bits in self._returned:
            print(f"    return {ret_bits}")

    @property
    def block(self) -> BasicBlock:
        return self._block

    @property
    def nodes(self) -> Sequence[AccessNode]:
        return self._block.nodes

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

    @property
    def arguments(self) -> Mapping[str, ArgStorage]:
        return self._block.arguments


def update_expressions_in_nodes(
    nodes: list[AccessNode],
    subst_func: Callable[[Expression], Expression | None],
) -> None:
    """
    Calls the given substitution function with each expression in the given nodes.
    If the substitution function returns an expression, that expression replaces
    the original expression. If the substitution function returns None, the original
    expression is kept.
    """
    for node in nodes:
        # Update indices for I/O storages.
        storage = node.storage
        new_storage = storage.substitute_expressions(subst_func)
        if new_storage is not storage:
            node.storage = new_storage

        # Update node with new expression.
        match node:
            case Store(expr=expr) as store:
                new_expr = expr.substitute(subst_func)
                if new_expr is not expr:
                    store.expr = new_expr


def update_expressions_in_bitstrings(
    returned: list[BitString],
    subst_func: Callable[[Expression], Expression | None],
) -> None:
    """
    Calls the given substitution function with each expression in the given bit strings.
    If the substitution function returns an expression, that expression replaces
    the original expression. If the substitution function returns None, the original
    expression is kept.
    """
    for i, ret_bits in enumerate(returned):
        new_bits = ret_bits.substitute(expression_func=subst_func)
        if new_bits is not ret_bits:
            returned[i] = new_bits
