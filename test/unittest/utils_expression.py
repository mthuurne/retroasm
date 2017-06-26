from retroasm.expression import (
    AndOperator, Expression, IntLiteral, LShift, RShift, OrOperator, truncate
    )
from retroasm.expression_simplifier import simplifyExpression
from retroasm.types import IntType, unlimited

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

class TestExprMixin:

    def assertIntLiteral(self, expr, value):
        '''Asserts that the given expression is an unlimited-width int literal
        with the given value.
        '''
        comparison = IntLiteral(value)
        self.assertIsInstance(expr, IntLiteral)
        self.assertEqual(expr.value, value)

    def assertAnd(self, expr, *args):
        self.assertIsInstance(expr, AndOperator)
        exprs = expr.exprs
        self.assertEqual(len(exprs), len(args))
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

    def assertOr(self, expr, *args):
        self.assertIsInstance(expr, OrOperator)
        exprs = expr.exprs
        self.assertEqual(len(exprs), len(args))
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

    def assertConcat(self, expr, subExprs):
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
        self.assertOr(expr, *compExprs)

    def assertSlice(self, expr, subExpr, subWidth, index, width):
        needsShift = index != 0
        shift = RShift(subExpr, index) if needsShift else subExpr
        needsTrunc = subWidth > index + width
        trunc = truncate(shift, width) if needsTrunc else shift
        self.assertEqual(str(expr), str(trunc))
        self.assertEqual(expr, trunc)
        self.assertIsInstance(expr, type(trunc))
        shiftExpr = expr.exprs[0] if needsTrunc else expr
        if needsShift:
            self.assertEqual(str(shiftExpr), str(shift))
            self.assertEqual(shiftExpr, shift)
            self.assertIsInstance(shiftExpr, RShift)
            self.assertEqual(shiftExpr.offset, index)
            self.assertEqual(shiftExpr.expr, subExpr)
        else:
            self.assertEqual(shiftExpr, subExpr)

    def assertTrunc(self, expr, subExpr, subWidth, width):
        self.assertSlice(expr, subExpr, subWidth, 0, width)
