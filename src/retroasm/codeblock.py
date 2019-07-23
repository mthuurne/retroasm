from __future__ import annotations

from typing import (
    AbstractSet, Callable, Dict, Iterable, Mapping, Optional, Set, Tuple,
    TypeVar, cast
)

from .expression import Expression
from .linereader import InputLocation
from .reference import BitString
from .storage import ArgStorage, Storage
from .types import maskForWidth
from .utils import const_property


class Node:
    '''Base class for nodes.
    '''
    __slots__ = ()

    def dump(self) -> None:
        raise NotImplementedError

class AccessNode(Node):
    '''Base class for Load and Store.
    '''
    __slots__ = ('_expr', '_storage', '_location')

    def __init__(self,
                 expr: Expression,
                 storage: Storage,
                 location: Optional[InputLocation]
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
    def location(self) -> Optional[InputLocation]:
        return self._location

    def dump(self) -> None:
        print(f'    {self} ({self._storage.width}-bit)')

    def clone(self) -> AccessNode:
        '''Create a clone of this node.
        '''
        raise NotImplementedError

class Load(AccessNode):
    '''A node that loads a value from a storage location.
    '''
    __slots__ = ()

    def __init__(self,
                 storage: Storage,
                 location: Optional[InputLocation] = None
                 ):
        expr = LoadedValue(self, maskForWidth(storage.width))
        AccessNode.__init__(self, expr, storage, location)

    def __repr__(self) -> str:
        return f'Load({self._storage!r}, {self._location!r})'

    def __str__(self) -> str:
        return f'load from {self._storage}'

    @property
    def expr(self) -> LoadedValue:
        return cast(LoadedValue, self._expr)

    @expr.setter
    def expr(self, expr: Expression) -> None:
        raise AttributeError('Cannot change expression for Load nodes')

    def clone(self) -> Load:
        return Load(self._storage, self._location)

class Store(AccessNode):
    '''A node that stores a value into a storage location.
    '''
    __slots__ = ()

    def __init__(self,
                 expr: Expression,
                 storage: Storage,
                 location: Optional[InputLocation] = None
                 ):
        AccessNode.__init__(self, expr, storage, location)

    def __repr__(self) -> str:
        return f'Store({self._expr!r}, {self._storage!r}, {self._location!r})'

    def __str__(self) -> str:
        return f'store {self._expr} in {self._storage}'

    def clone(self) -> Store:
        return Store(self._expr, self._storage, self._location)

class LoadedValue(Expression):
    '''A value loaded from a storage location.
    '''
    __slots__ = ('_load', '_mask')

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

    def _ctorargs(self) -> Tuple[Load, int]:
        return self._load, self._mask

    def __repr__(self) -> str:
        return f'LoadedValue({self._load!r}, {self._mask:d})'

    def __str__(self) -> str:
        return f'load({self._load.storage})'

    def _equals(self, other: LoadedValue) -> bool:
        # pylint: disable=protected-access
        return self._load is other._load

    @property
    def complexity(self) -> int:
        # Since loaded values are only available at runtime, they are not
        # desirable in analysis, so assign a high cost to them.
        return 8

def verifyLoads(nodes: Iterable[AccessNode],
                returned: Iterable[BitString] = ()
                ) -> None:
    '''Performs consistency checks on the LoadedValues in the given nodes and
    returned bit strings.
    Raises AssertionError if an inconsistency is found.
    '''
    # Check that every LoadedValue has an associated Load node, which must
    # execute before the LoadedValue is used.
    loads: Set[Load] = set()
    for node in nodes:
        # Check that all expected loads have occurred.
        if isinstance(node, Store):
            for value in node.expr.iterInstances(LoadedValue):
                assert value.load in loads, value
        for expr in node.storage.iterExpressions():
            for value in expr.iterInstances(LoadedValue):
                assert value.load in loads, value
        # Remember this load.
        if isinstance(node, Load):
            loads.add(node)
    # Check I/O indices in the returned bit string.
    for retBits in returned:
        for storage in retBits.iterStorages():
            for expr in storage.iterExpressions():
                for value in expr.iterInstances(LoadedValue):
                    assert value.load in loads, value

class CodeBlock:

    def __init__(self,
                 nodes: Iterable[AccessNode],
                 returned: Iterable[BitString]
                 ):
        clonedNodes = []
        valueMapping: Dict[Expression, Expression] = {}
        for node in nodes:
            clone = node.clone()
            clonedNodes.append(clone)
            if isinstance(node, Load):
                valueMapping[node.expr] = clone.expr
        self.nodes = clonedNodes
        self.returned = list(returned)
        self._updateExpressions(valueMapping.get)
        assert self.verify()

    def verify(self) -> bool:
        '''Performs consistency checks on this code block.
        Raises AssertionError if an inconsistency is found.
        Returns True on success, never returns False.
        '''
        verifyLoads(self.nodes, self.returned)
        return True

    def dump(self) -> None:
        '''Prints this code block on stdout.
        '''
        for node in self.nodes:
            node.dump()
        for retBits in self.returned:
            print(f'    return {retBits}')

    def _gatherExpressions(self) -> Set[Expression]:
        expressions = set()
        for node in self.nodes:
            if isinstance(node, Store):
                expressions.add(node.expr)
            expressions.update(node.storage.iterExpressions())
        for retBits in self.returned:
            expressions.update(retBits.iterExpressions())
        return expressions

    @const_property
    def expressions(self) -> AbstractSet[Expression]:
        '''A set of all expressions that are contained in this block.
        Only top-level expressions are included, not all subexpressions of
        those top-level expressions.
        '''
        return self._gatherExpressions()

    def _gatherStorages(self) -> Set[Storage]:
        storages = set()
        for node in self.nodes:
            storages.add(node.storage)
        for retBits in self.returned:
            storages.update(retBits.iterStorages())
        return storages

    @property
    def storages(self) -> AbstractSet[Storage]:
        '''A set of all storages that are accessed or referenced by this block.
        '''
        return self._gatherStorages()

    def _gatherArguments(self) -> Mapping[str, ArgStorage]:
        args: Dict[str, ArgStorage] = {}
        for storage in self.storages:
            if isinstance(storage, ArgStorage):
                name = storage.name
                prev = args.get(name)
                if prev is None:
                    args[name] = storage
                elif prev is not storage:
                    raise ValueError(f'multiple arguments named "{name}"')
        return args

    @const_property
    def arguments(self) -> Mapping[str, ArgStorage]:
        '''A mapping containing all arguments that occur in this code block.
        ValueError is raised if the same name is used for multiple arguments.
        '''
        return self._gatherArguments()

    def _updateExpressions(self,
                           substFunc: Callable[[Expression],
                                               Optional[Expression]]
                           ) -> None:
        '''Calls the given substitution function with each expression in this
        code block. If the substitution function returns an expression, that
        expression replaces the original expression. If the substitution
        function returns None, the original expression is kept.
        '''
        for node in self.nodes:
            # Update indices for I/O storages.
            storage = node.storage
            newStorage = storage.substituteExpressions(substFunc)
            if newStorage is not storage:
                node.storage = newStorage

            # Update node with new expression.
            if isinstance(node, Store):
                expr = node.expr
                newExpr = expr.substitute(substFunc)
                if newExpr is not expr:
                    node.expr = newExpr

        # Update returned bit string.
        returned = self.returned
        for i, retBits in enumerate(returned):
            newBits = retBits.substitute(expressionFunc=substFunc)
            if newBits is not retBits:
                returned[i] = newBits
