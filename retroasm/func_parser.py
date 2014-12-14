from .expression import (
    Concatenation, Expression, IOReference, IntLiteral, LocalReference,
    NamedValue, Register, Slice, Storage
    )

from itertools import count

class ConstantDef:
    '''Definition of a local constant value.
    '''
    __slots__ = ('_cid', '_expr')

    cid = property(lambda self: self._cid)
    expr = property(lambda self: self._expr)
    type = property(lambda self: self._expr._type)

    def __init__(self, cid, expr):
        self._expr = Expression.checkInstance(expr)
        if not isinstance(cid, int):
            raise TypeError('constant ID must be int, got %s' % type(cid))
        self._cid = cid

    def __repr__(self):
        return 'ConstantDef(%d, %s)' % (self._cid, repr(self._expr))

    def __str__(self):
        return 'C%d = %s' % (self._cid, self._expr)

class Node:
    '''Base class for nodes.
    '''
    __slots__ = ()

class AccessNode(Node):
    '''Base class for Load and Store.
    '''
    __slots__ = ('_cid', '_rid')

    cid = property(lambda self: self._cid)
    rid = property(lambda self: self._rid)

    def __init__(self, cid, rid):
        if not isinstance(cid, int):
            raise TypeError('constant ID must be int, got %s' % type(cid))
        if not isinstance(rid, int):
            raise TypeError('reference ID must be int, got %s' % type(rid))
        self._cid = cid
        self._rid = rid

    def __repr__(self):
        return '%s(%d, %d)' % (self.__class__.__name__, self._cid, self._rid)

class Load(AccessNode):
    '''A node that loads a value from a storage location.
    '''
    __slots__ = ()

    def __str__(self):
        return 'load C%d from R%d' % (self._cid, self._rid)

class Store(AccessNode):
    '''A node that stores a value into a storage location.
    '''
    __slots__ = ()

    def __str__(self):
        return 'store C%d in R%d' % (self._cid, self._rid)

class Constant(Expression):
    '''A synthetic constant containing an intermediate value.
    '''
    __slots__ = ('_cid',)

    cid = property(lambda self: self._cid)

    def __init__(self, cid, intType):
        if not isinstance(cid, int):
            raise TypeError('constant ID must be int, got %s' % type(cid))
        Expression.__init__(self, intType)
        self._cid = cid

    def __repr__(self):
        return 'Constant(%d, %s)' % (self._cid, repr(self._type))

    def __str__(self):
        return 'C%d' % self._cid

    def _equals(self, other):
        return self._cid is other._cid

    def _complexity(self):
        return 2

def createFunc(log, assignments):
    '''Creates a function body from the given assignments.
    Returns a tuple of three lists: constants, references and nodes.
    '''
    nodes = []
    constants = []
    references = []
    nameToReference = {}

    def getReferenceID(namedValue):
        name = namedValue.name
        rid = nameToReference.get(name)
        if rid is None:
            rid = len(references)
            references.append(namedValue)
            nameToReference[name] = rid
        return rid

    def getReferenceType(rid):
        return references[rid].type

    constantCounter = count()
    def emitConstant(expr):
        constantDef = ConstantDef(next(constantCounter), expr)
        constants.append(constantDef)
        return Constant(constantDef.cid, expr.type)

    def emitLoad(rid):
        load = Load(next(constantCounter), rid)
        nodes.append(load)
        return Constant(load.cid, getReferenceType(rid))

    def emitStore(rid, value):
        const = emitConstant(value)
        nodes.append(Store(const.cid, rid))

    def constifyIOIndex(ref):
        indexConst = emitConstant(ref.index.substitute(substituteConstants))
        rid = len(references)
        references.append(IOReference(ref.channel, indexConst))
        return rid

    def substituteConstants(expr):
        if not isinstance(expr, Storage):
            return None
        elif isinstance(expr, IOReference):
            return emitLoad(constifyIOIndex(expr))
        else:
            return emitLoad(getReferenceID(expr))

    def decomposeLHS(storage, top=True):
        '''Iterates through the storage locations inside a concatenation.
        Each element is a pair of resource ID and offset.
        '''
        if isinstance(storage, IntLiteral):
            # Assigning to a literal as part of a concatenation can be useful,
            # but assigning to only a literal is probably a mistake.
            if top:
                log.warning('assigning to literal has no effect')
        elif isinstance(storage, Storage):
            if isinstance(storage, IOReference):
                rid = constifyIOIndex(storage)
            else:
                rid = getReferenceID(storage)
            yield rid, 0
        elif isinstance(storage, Concatenation):
            for concatTerm, concatOffset in storage.iterWithOffset():
                for rid, offset in decomposeLHS(concatTerm, False):
                    yield rid, concatOffset + offset
        else:
            log.error('cannot assign to an arithmetical expression: %s', lhs)

    for lhs, rhs in assignments:
        rhsConst = emitConstant(rhs.substitute(substituteConstants))

        # Create a list since we want to force emission of all loads before
        # we emit any stores.
        lhsDecomposed = list(decomposeLHS(lhs))

        for rid, offset in lhsDecomposed:
            width = getReferenceType(rid).width
            emitStore(rid, Slice(rhsConst, offset, width))

    return constants, references, nodes
