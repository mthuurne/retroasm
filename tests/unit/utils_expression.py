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
from retroasm.expression_simplifier import simplify_expression
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


def assert_int_literal(expr: Expression, value: int) -> None:
    """Assert that the given expression is an int literal with the given value."""
    assert isinstance(expr, IntLiteral)
    assert expr.value == value


def assert_and(expr: Expression, *args: Expression) -> None:
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


def assert_or(expr: Expression, *args: Expression) -> None:
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


def assert_concat(
    expr: Expression, subExprs: Sequence[tuple[Expression, Width]]
) -> None:
    compExprs = []
    offset = 0
    for term, width in reversed(subExprs):
        shifted = simplify_expression(LShift(term, offset))
        if not (isinstance(shifted, IntLiteral) and shifted.value == 0):
            compExprs.append(shifted)
        offset += cast(int, width)
    assert_or(expr, *compExprs)


def assert_slice(
    expr: Expression, subExpr: Expression, subWidth: Width, index: int, width: Width
) -> None:
    needs_shift = index != 0
    shift = RShift(subExpr, index) if needs_shift else subExpr
    needs_trunc = subWidth > index + width
    trunc = truncate(shift, width) if needs_trunc else shift
    assert str(expr) == str(trunc)
    assert expr == trunc
    assert isinstance(expr, type(trunc))
    if needs_trunc:
        assert isinstance(expr, MultiExpression), expr
        shift_expr = expr.exprs[0]
    else:
        shift_expr = expr
    if needs_shift:
        assert str(shift_expr) == str(shift)
        assert shift_expr == shift
        assert isinstance(shift_expr, RShift)
        assert shift_expr.offset == index
        assert shift_expr.expr == subExpr
    else:
        assert shift_expr == subExpr


def assert_trunc(
    expr: Expression, subExpr: Expression, subWidth: Width, width: Width
) -> None:
    assert_slice(expr, subExpr, subWidth, 0, width)
