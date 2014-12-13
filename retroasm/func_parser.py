from .expression import (
    Concatenation, Expression, IOReference, IntLiteral, IntType, LocalReference,
    NamedValue, Reference, Register, Slice
    )
from .expression_parser import parseExpr

from itertools import count

def decomposeConcatenation(log, lhs, top=True):
    '''Iterates through the storage locations inside a concatenation.
    Each element is a pair of Reference and offset.
    '''
    if isinstance(lhs, IntLiteral):
        # Assigning to a literal as part of a concatenation can be useful,
        # but assigning to only a literal is probably a mistake.
        if top:
            log.warning('assigning to literal has no effect')
    elif isinstance(lhs, Reference):
        yield lhs, 0
    elif isinstance(lhs, Concatenation):
        for concatTerm, concatOffset in lhs.iterWithOffset():
            for expr, offset in decomposeConcatenation(log, concatTerm, False):
                yield expr, concatOffset + offset
    else:
        log.error('cannot assign to an arithmetical expression: %s', lhs)

class Node:
    '''Base class for nodes.
    '''
    __slots__ = ()

class ConstantDef(Node):
    '''A node that defines a local constant.
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
        return '{C%d} = %s' % (self._cid, self._expr)

class Load(Node):
    '''A node that loads a value from a storage location.
    '''
    __slots__ = ('_cid', '_storage')

    cid = property(lambda self: self._cid)
    storage = property(lambda self: self._storage)
    type = property(lambda self: self._storage._type)

    def __init__(self, cid, storage):
        if not isinstance(cid, int):
            raise TypeError('constant ID must be int, got %s' % type(cid))
        if not isinstance(storage, (IOReference, NamedValue)):
            raise TypeError('not a storage location: %s' % type(storage))
        if isinstance(storage, IOReference):
            if not isinstance(storage.index, Constant):
                raise ValueError(
                    'load needs a constant index, got %s' % type(storage.index)
                    )
        self._cid = cid
        self._storage = storage

    def __repr__(self):
        return 'Load(%d, %s)' % (self._cid, repr(self._storage))

    def __str__(self):
        return 'load {C%d}, %s' % (self._cid, self._storage)

class Store(Node):
    '''A node that stores a value into a storage location.
    '''
    __slots__ = ('_storage', '_cid')

    storage = property(lambda self: self._storage)
    cid = property(lambda self: self._cid)

    def __init__(self, storage, cid):
        if not isinstance(storage, (IOReference, NamedValue)):
            raise TypeError('not a storage location: %s' % type(storage))
        if not isinstance(cid, int):
            raise TypeError('constant ID must be int, got %s' % type(cid))
        self._storage = storage
        self._cid = cid

    def __repr__(self):
        return 'Store(%s, %d)' % (repr(self._storage), self._cid)

    def __str__(self):
        return 'store %s, {C%d}' % (self._storage, self._cid)

class Constant(Expression):
    '''A synthetic constant containing an intermediate value.
    '''
    __slots__ = ('_constDef',)

    cid = property(lambda self: self._constDef.cid)

    def __init__(self, constDef):
        if not isinstance(constDef, (ConstantDef, Load)):
            raise TypeError('not a constant definition: %s' % type(constDef))
        self._constDef = constDef
        Expression.__init__(self, constDef.type)

    def __repr__(self):
        return 'Constant(%s)' % repr(self._constDef)

    def __str__(self):
        return '{C%d}' % self._constDef.cid

    def _equals(self, other):
        return self._constDef is other._constDef

    def _complexity(self):
        return 1 + self._constDef.expr._complexity()

    def simplify(self):
        return self._constDef.expr.simplify()

def parseFuncBody(log, lines, context):
    ok = True
    def error(msg, *args):
        nonlocal ok
        ok = False
        log.error(msg, *args)

    nodes = []

    constantCounter = count()
    def emitConstant(expr):
        constantDef = ConstantDef(next(constantCounter), expr)
        nodes.append(constantDef)
        return Constant(constantDef)

    def emitLoad(storage):
        load = Load(next(constantCounter), storage)
        nodes.append(load)
        return Constant(load)

    def emitStore(storage, value):
        const = emitConstant(value)
        nodes.append(Store(storage, const.cid))

    # TODO: We cannot perform substitution on LocalReferences until we know
    #       the arguments passed to the function. So we have to limit the
    #       kind of processing we do when parsing the function.

    def constifyIOIndex(ref):
        indexConst = emitConstant(ref.index.substitute(substituteConstants))
        return IOReference(ref.channel, indexConst)

    def substituteConstants(expr):
        if not isinstance(expr, Reference):
            return None
        elif isinstance(expr, Register):
            return emitLoad(expr)
        elif isinstance(expr, IOReference):
            return emitLoad(constifyIOIndex(expr))
        elif isinstance(storage, LocalReference):
            return None
        else:
            # Unknown Reference subclass.
            assert False, storage

    def substituteIndices(storage):
        assert isinstance(storage, Reference), storage
        if isinstance(storage, Register):
            return storage
        elif isinstance(storage, IOReference):
            return constifyIOIndex(storage)
        elif isinstance(storage, LocalReference):
            return storage
        else:
            # Unknown Reference subclass.
            assert False, storage

    for line in lines:
        parts = line.split(':=')
        if len(parts) < 2:
            error('no assignment in line')
        elif len(parts) > 2:
            error('multiple assignments in a single line')
        else:
            lhsStr, rhsStr = parts
            try:
                lhs = parseExpr(lhsStr, context)
            except ValueError as ex:
                error('error in left hand side of assignment: %s', str(ex))
                continue
            try:
                rhs = parseExpr(rhsStr, context)
            except ValueError as ex:
                error('error in right hand side of assignment: %s', str(ex))
                continue

            rhsConst = emitConstant(rhs.substitute(substituteConstants))

            errorsBefore = log.errors
            lhsConcats = [
                (substituteIndices(storage), offset)
                for storage, offset in decomposeConcatenation(log, lhs)
                ]
            ok &= errorsBefore == log.errors

            for storage, offset in lhsConcats:
                emitStore(storage, Slice(rhsConst, offset, storage.width))

    return nodes if ok else None
