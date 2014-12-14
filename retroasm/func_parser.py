from .expression import (
    Concatenation, Expression, IOReference, IntLiteral, IntType, LocalReference,
    NamedValue, Register, Slice, Storage
    )

from inspect import signature
from itertools import count

class Constant:
    '''Definition of a local constant value.
    '''
    __slots__ = ('_cid', '_type')

    cid = property(lambda self: self._cid)
    type = property(lambda self: self._type)

    def __init__(self, cid, intType):
        if not isinstance(cid, int):
            raise TypeError('constant ID must be int, got %s' % type(cid))
        if not isinstance(intType, IntType):
            raise TypeError('type must be IntType, got %s' % type(intType))
        self._cid = cid
        self._type = intType

    def __repr__(self):
        return 'Constant(%d, %s)' % (self._cid, repr(self._type))

    def __str__(self):
        return '%s C%d' % (self._type, self._cid)

class ComputedConstant(Constant):
    '''A constant defined by evaluating an expression.
    '''
    __slots__ = ('_expr',)

    expr = property(lambda self: self._expr)

    def __init__(self, cid, expr):
        self._expr = Expression.checkInstance(expr)
        Constant.__init__(self, cid, expr.type)

    def __repr__(self):
        return 'ComputedConstant(%d, %s)' % (self._cid, repr(self._expr))

    def __str__(self):
        return '%s = %s' % (super().__str__(), self._expr)

class LoadedConstant(Constant):
    '''A constant defined by loading a value from a storage location.
    '''
    __slots__ = ('_load',)

    load = property(lambda self: self._load)
    rid = property(lambda self: self._load.rid)

    def __init__(self, load, refType):
        if not isinstance(load, Load):
            raise TypeError('expected Load node, got %s' % type(load))
        Constant.__init__(self, load.cid, refType)
        self._load = load

    def __repr__(self):
        return 'LoadedConstant(%s, %s)' % (repr(self._load), repr(self.type))

    def __str__(self):
        return '%s <- R%s' % (super().__str__(), self._load.rid)

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

class ConstantValue(Expression):
    '''A synthetic constant containing an intermediate value.
    '''
    __slots__ = ('_constant',)

    constant = property(lambda self: self._constant)
    cid = property(lambda self: self._constant.cid)

    def __init__(self, constant):
        if not isinstance(constant, Constant):
            raise TypeError('expected Constant object, got %s' % type(constant))
        Expression.__init__(self, constant.type)
        self._constant = constant

    def _ctorargs(self, *exprs, **kwargs):
        cls = self.__class__
        if exprs:
            raise ValueError('%s does not take expression args' % cls.__name__)
        kwargs.setdefault('constant', self._constant)
        return signature(cls).bind(**kwargs)

    def __str__(self):
        return 'C%d' % self.cid

    def _equals(self, other):
        return self._constant is other._constant

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
        constant = ComputedConstant(next(constantCounter), expr)
        constants.append(constant)
        return ConstantValue(constant)

    def emitLoad(rid):
        load = Load(next(constantCounter), rid)
        nodes.append(load)
        refType = getReferenceType(rid)
        constant = LoadedConstant(load, refType)
        constants.append(constant)
        return ConstantValue(constant)

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
