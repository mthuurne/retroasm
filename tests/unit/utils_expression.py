from retroasm.expression import (
    AndOperator, Expression, IntLiteral, LShift, RShift, OrOperator, truncate
    )
from retroasm.expression_simplifier import simplifyExpression
from retroasm.types import IntType, unlimited


def makeConcat(exprH, exprL, widthL):
    return OrOperator(exprL, LShift(exprH, widthL))

def makeSlice(expr, index, width):
    return truncate(RShift(expr, index), width)

class TestValue(Expression):
    __slots__ = ('_name', '_type')

    name = property(lambda self: self._name)
    mask = property(lambda self: self._type.mask)

    def __init__(self, name, typ):
        if not isinstance(name, str):
            raise TypeError('name must be string, got %s' % type(name).__name__)
        if not isinstance(typ, IntType):
            raise TypeError('typ must be IntType, got %s' % type(type).__name__)
        Expression.__init__(self)
        self._name = name
        self._type = typ

    def _ctorargs(self):
        return self._name, self._type

    def __str__(self):
        return self._name

    def _equals(self, other):
        return self is other

    @property
    def complexity(self):
        return 3

def assertIntLiteral(expr, value):
    '''Asserts that the given expression is an unlimited-width int literal
    with the given value.
    '''
    assert isinstance(expr, IntLiteral)
    assert expr.value == value

def assertAnd(expr, *args):
    assert isinstance(expr, AndOperator)
    exprs = expr.exprs
    assert len(exprs) == len(args)
    found = [False] * len(exprs)
    missing = []
    for arg in args:
        try:
            found[exprs.index(arg)] = True
        except ValueError:
            missing.append(arg)
    if missing:
        raise AssertionError(
            'mismatch on AND arguments: expected %s, got %s' % (
                ', '.join("'%s'" % e for e in missing),
                ', '.join("'%s'" % e for f, e in zip(found, exprs) if not f)
                )
            )

def assertOr(expr, *args):
    assert isinstance(expr, OrOperator)
    exprs = expr.exprs
    assert len(exprs) == len(args)
    found = [False] * len(exprs)
    missing = []
    for arg in args:
        try:
            found[exprs.index(arg)] = True
        except ValueError:
            missing.append(arg)
    if missing:
        raise AssertionError(
            'mismatch on OR arguments: expected %s, got %s' % (
                ', '.join("'%s'" % e for e in missing),
                ', '.join("'%s'" % e for f, e in zip(found, exprs) if not f)
                )
            )

def assertConcat(expr, subExprs):
    compExprs = []
    offset = 0
    for term, width in reversed(subExprs):
        shifted = simplifyExpression(LShift(term, offset))
        if not (isinstance(shifted, IntLiteral) and shifted.value == 0):
            compExprs.append(shifted)
        if width is unlimited:
            offset = None
        else:
            offset += width
    assertOr(expr, *compExprs)

def assertSlice(expr, subExpr, subWidth, index, width):
    needsShift = index != 0
    shift = RShift(subExpr, index) if needsShift else subExpr
    needsTrunc = subWidth > index + width
    trunc = truncate(shift, width) if needsTrunc else shift
    assert str(expr) == str(trunc)
    assert expr == trunc
    assert isinstance(expr, type(trunc))
    shiftExpr = expr.exprs[0] if needsTrunc else expr
    if needsShift:
        assert str(shiftExpr) == str(shift)
        assert shiftExpr == shift
        assert isinstance(shiftExpr, RShift)
        assert shiftExpr.offset == index
        assert shiftExpr.expr == subExpr
    else:
        assert shiftExpr == subExpr

def assertTrunc(expr, subExpr, subWidth, width):
    assertSlice(expr, subExpr, subWidth, 0, width)
