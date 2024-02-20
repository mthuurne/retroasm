from __future__ import annotations

from collections.abc import Callable, Iterable, Mapping, Set
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
) -> None:
    """
    Performs consistency checks on the LoadedValues in the given nodes and
    returned bit strings.
    Raises AssertionError if an inconsistency is found.
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


class BasicBlock:
    """
    A sequence of load/store operations without any branches.
    """

    def __init__(self, nodes: Iterable[AccessNode], returned: Iterable[BitString]):
        cloned_nodes = []
        value_mapping: dict[Expression, Expression] = {}
        for node in nodes:
            clone = node.clone()
            cloned_nodes.append(clone)
            if isinstance(node, Load):
                value_mapping[node.expr] = clone.expr
        self.nodes = cloned_nodes
        self.returned = list(returned)
        self._update_expressions(value_mapping.get)
        assert self.verify()

    def verify(self) -> bool:
        """
        Performs consistency checks on this basic block.
        Raises AssertionError if an inconsistency is found.
        Returns True on success, never returns False.
        """
        verify_loads(self.nodes, self.returned)
        return True

    def dump(self) -> None:
        """Prints this basic block on stdout."""
        for node in self.nodes:
            node.dump()
        for ret_bits in self.returned:
            print(f"    return {ret_bits}")

    @const_property
    def expressions(self) -> Set[Expression]:
        """
        A set of all expressions that are contained in this block.
        Only top-level expressions are included, not all subexpressions of
        those top-level expressions.
        """
        expressions = set()
        for node in self.nodes:
            if isinstance(node, Store):
                expressions.add(node.expr)
            expressions.update(node.storage.iter_expressions())
        for ret_bits in self.returned:
            expressions.update(ret_bits.iter_expressions())
        return expressions

    @const_property
    def storages(self) -> Set[Storage]:
        """A set of all storages that are accessed or referenced by this block."""
        storages = set()
        for node in self.nodes:
            storages.add(node.storage)
        for ret_bits in self.returned:
            storages.update(ret_bits.iter_storages())
        return storages

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

    def _update_expressions(
        self, subst_func: Callable[[Expression], Expression | None]
    ) -> None:
        """
        Calls the given substitution function with each expression in this
        basic block. If the substitution function returns an expression, that
        expression replaces the original expression. If the substitution
        function returns None, the original expression is kept.
        """
        for node in self.nodes:
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

        # Update returned bit string.
        returned = self.returned
        for i, ret_bits in enumerate(returned):
            new_bits = ret_bits.substitute(expression_func=subst_func)
            if new_bits is not ret_bits:
                returned[i] = new_bits
