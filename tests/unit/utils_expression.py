from __future__ import annotations

from typing import Sequence, cast

from retroasm.expression import (
    AndOperator,
    Expression,
    IntLiteral,
    LShift,
    MultiExpression,
    OrOperator,
    RShift,
    truncate,
)
from retroasm.expression_simplifier import simplifyExpression
from retroasm.types import IntType, Width


def makeConcat(exprH: Expression, exprL: Expression, widthL: int) -> Expression:
    return OrOperator(exprL, LShift(exprH, widthL))


def makeSlice(expr: Expression, index: int, width: Width) -> Expression:
    return truncate(RShift(expr, index), width)


class TestValue(Expression):
    __slots__ = ("_name", "_type")

    @property
    def name(self) -> str:
        return self._name

    @property
    def mask(self) -> int:
        return self._type.mask

    def __init__(self, name: str, typ: IntType):
        super().__init__()
        self._name = name
        self._type = typ

    def _ctorargs(self) -> tuple[str, IntType]:
        return self._name, self._type

    def __str__(self) -> str:
        return self._name

    def _equals(self, other: object) -> bool:
        return self is other

    @property
    def complexity(self) -> int:
        return 3


def assertIntLiteral(expr: Expression, value: int) -> None:
    """Assert that the given expression is an int literal with the given value."""
    assert isinstance(expr, IntLiteral)
    assert expr.value == value


def assertAnd(expr: Expression, *args: Expression) -> None:
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
            "mismatch on AND arguments: expected %s, got %s"
            % (
                ", ".join("'%s'" % e for e in missing),
                ", ".join("'%s'" % e for f, e in zip(found, exprs) if not f),
            )
        )


def assertOr(expr: Expression, *args: Expression) -> None:
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
            "mismatch on OR arguments: expected %s, got %s"
            % (
                ", ".join("'%s'" % e for e in missing),
                ", ".join("'%s'" % e for f, e in zip(found, exprs) if not f),
            )
        )


def assertConcat(
    expr: Expression, subExprs: Sequence[tuple[Expression, Width]]
) -> None:
    compExprs = []
    offset = 0
    for term, width in reversed(subExprs):
        shifted = simplifyExpression(LShift(term, offset))
        if not (isinstance(shifted, IntLiteral) and shifted.value == 0):
            compExprs.append(shifted)
        offset += cast(int, width)
    assertOr(expr, *compExprs)


def assertSlice(
    expr: Expression, subExpr: Expression, subWidth: Width, index: int, width: Width
) -> None:
    needsShift = index != 0
    shift = RShift(subExpr, index) if needsShift else subExpr
    needsTrunc = subWidth > index + width
    trunc = truncate(shift, width) if needsTrunc else shift
    assert str(expr) == str(trunc)
    assert expr == trunc
    assert isinstance(expr, type(trunc))
    if needsTrunc:
        assert isinstance(expr, MultiExpression), expr
        shiftExpr = expr.exprs[0]
    else:
        shiftExpr = expr
    if needsShift:
        assert str(shiftExpr) == str(shift)
        assert shiftExpr == shift
        assert isinstance(shiftExpr, RShift)
        assert shiftExpr.offset == index
        assert shiftExpr.expr == subExpr
    else:
        assert shiftExpr == subExpr


def assertTrunc(
    expr: Expression, subExpr: Expression, subWidth: Width, width: Width
) -> None:
    assertSlice(expr, subExpr, subWidth, 0, width)
