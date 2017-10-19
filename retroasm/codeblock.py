from .expression import Expression
from .linereader import InputLocation
from .reference import BitString
from .storage import ArgStorage, Storage
from .types import maskForWidth
from .utils import checkType, const_property

class Node:
    '''Base class for nodes.
    '''
    __slots__ = ()

class AccessNode(Node):
    '''Base class for Load and Store.
    '''
    __slots__ = ('_expr', '_storage', '_location')

    expr = property(lambda self: self._expr)
    storage = property(lambda self: self._storage)
    location = property(lambda self: self._location)

    def __init__(self, expr, storage, location):
        self._expr = expr
        self._storage = storage
        self._location = None if location is None else checkType(
            location, InputLocation, 'location'
            )

    @expr.setter
    def expr(self, expr):
        self._expr = checkType(expr, Expression, 'value')

    @storage.setter
    def storage(self, storage):
        self._storage = checkType(storage, Storage, 'storage')

    def clone(self):
        '''Create a clone of this node.
        '''
        raise NotImplementedError

class Load(AccessNode):
    '''A node that loads a value from a storage location.
    '''
    __slots__ = ()

    def __init__(self, storage, location=None):
        checkType(storage, Storage, 'storage')
        expr = LoadedValue(self, maskForWidth(storage.width))
        AccessNode.__init__(self, expr, storage, location)

    def __repr__(self):
        return 'Load(%r, %r)' % (self._storage, self._location)

    def __str__(self):
        return 'load from %s' % self._storage

    @AccessNode.expr.setter
    def expr(self, expr):
        raise AttributeError('Cannot change expression for Load nodes')

    def clone(self):
        return Load(self._storage, self._location)

class Store(AccessNode):
    '''A node that stores a value into a storage location.
    '''
    __slots__ = ()

    def __init__(self, expr, storage, location=None):
        AccessNode.__init__(
            self,
            checkType(expr, Expression, 'value'),
            checkType(storage, Storage, 'storage'),
            location
            )

    def __repr__(self):
        return 'Store(%r, %r, %r)' % (self._expr, self._storage, self._location)

    def __str__(self):
        return 'store %s in %s' % (self._expr, self._storage)

    def clone(self):
        return Store(self._expr, self._storage, self._location)

class LoadedValue(Expression):
    '''A value loaded from a storage location.
    '''
    __slots__ = ('_load', '_mask')

    load = property(lambda self: self._load)
    mask = property(lambda self: self._mask)

    def __init__(self, load, mask):
        Expression.__init__(self)
        self._load = checkType(load, Load, 'load node')
        self._mask = checkType(mask, int, 'mask')

    def _ctorargs(self):
        return self._load, self._mask

    def __repr__(self):
        return 'LoadedValue(%r, %d)' % (self._load, self._mask)

    def __str__(self):
        return 'load(%s)' % self._load.storage

    def _equals(self, other):
        # pylint: disable=protected-access
        return self._load is other._load

    @property
    def complexity(self):
        # Since loaded values are only available at runtime, they are not
        # desirable in analysis, so assign a high cost to them.
        return 8

def verifyLoads(nodes, returned=()):
    '''Performs consistency checks on the LoadedValues in the given nodes and
    returned bit strings.
    Raises AssertionError if an inconsistency is found.
    '''
    # Check that every LoadedValue has an associated Load node, which must
    # execute before the LoadedValue is used.
    loads = set()
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

    def __init__(self, nodes, returned):
        clonedNodes = []
        valueMapping = {}
        for node in nodes:
            clone = node.clone()
            clonedNodes.append(clone)
            if isinstance(node, Load):
                valueMapping[node.expr] = clone.expr
        self.nodes = clonedNodes
        self.returned = list(returned)
        assert all(isinstance(ret, BitString) for ret in self.returned), \
            self.returned
        self._updateExpressions(valueMapping.get)
        assert self.verify() is None

    def verify(self):
        '''Performs consistency checks on this code block.
        Raises AssertionError if an inconsistency is found.
        '''
        verifyLoads(self.nodes, self.returned)

    def dump(self):
        '''Prints this code block on stdout.
        '''
        for node in self.nodes:
            print('    %s (%s-bit)' % (node, node.storage.width))
        for retBits in self.returned:
            print('    return %s' % retBits)

    def _gatherExpressions(self):
        '''A set of all expressions that are contained in this block.
        Only top-level expressions are included, not all subexpressions of
        those top-level expressions.
        '''
        expressions = set()
        for node in self.nodes:
            if isinstance(node, Store):
                expressions.add(node.expr)
            expressions.update(node.storage.iterExpressions())
        for retBits in self.returned:
            expressions.update(retBits.iterExpressions())
        return expressions

    expressions = const_property(_gatherExpressions)

    def _gatherStorages(self):
        '''A set of all storages that are accessed or referenced by this block.
        '''
        storages = set()
        for node in self.nodes:
            storages.add(node.storage)
        for retBits in self.returned:
            storages.update(retBits.iterStorages())
        return storages

    storages = const_property(_gatherStorages)

    def _gatherArguments(self):
        '''A dictionary containing all arguments that occur in this code block.
        The dictionary keys are the argument names and the dictionary values
        are ArgStorage instances.
        ValueError is raised if the same name is used for multiple arguments.
        '''
        args = {}
        for storage in self.storages:
            if isinstance(storage, ArgStorage):
                name = storage.name
                prev = args.get(name)
                if prev is None:
                    args[name] = storage
                elif prev is not storage:
                    raise ValueError('multiple arguments named "%s"' % name)
        return args

    arguments = const_property(_gatherArguments)

    def _updateExpressions(self, substFunc):
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
