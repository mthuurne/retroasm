from .expression import (
    Concatenation, Expression, IOReference, IntLiteral, IntType, NamedValue,
    Reference, Slice
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

    def __init__(self, cid, expr):
        self._expr = Expression.checkInstance(expr)
        if not isinstance(cid, int):
            raise TypeError('constant ID must be int, got %s' % type(cid))
        self._cid = cid

    def __repr__(self):
        return 'ConstantDef(%d, %s)' % (self._cid, repr(self._expr))

    def __str__(self):
        return '{C%d} = %s' % (self._cid, self._expr)

class Store(Node):
    '''A node that stores a value into a storage location.
    '''
    __slots__ = ('_storage', '_value')

    storage = property(lambda self: self._storage)
    value = property(lambda self: self._value)

    def __init__(self, storage, value):
        if not isinstance(storage, (IOReference, NamedValue)):
            raise TypeError('not a storage location: %s' % type(storage))
        self._storage = storage
        self._value = Expression.checkInstance(value)

    def __repr__(self):
        return 'Store(%s, %s)' % (repr(self._storage), repr(self._value))

    def __str__(self):
        return '%s := %s' % (self._storage, self._value)

class Constant(Expression):
    '''A synthetic constant containing an intermediate value.
    '''
    __slots__ = ('_constDef',)

    def __init__(self, constDef):
        if not isinstance(constDef, ConstantDef):
            raise TypeError('not a constant definition: %s' % type(constDef))
        self._constDef = constDef
        Expression.__init__(self, IntType(None))

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

    def emitStore(storage, value):
        nodes.append(Store(storage, value))

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

            # TODO: Isolate I/O and dereferencing from both rhs and
            #       the inner exprs of lhs.

            rhsConst = emitConstant(rhs)

            errorsBefore = log.errors
            for storage, offset in decomposeConcatenation(log, lhs):
                emitStore(storage, Slice(rhsConst, offset, storage.width))
            ok &= errorsBefore == log.errors

    return nodes if ok else None
