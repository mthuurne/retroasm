from __future__ import annotations

from retroasm.expression import (
    Expression,
    IntLiteral,
    LShift,
    MultiExpression,
    OrOperator,
    RShift,
    truncate,
)
from retroasm.types import IntType, Width


def make_concat(expr_h: Expression, expr_l: Expression, width_l: int) -> Expression:
    return OrOperator(expr_l, LShift(expr_h, width_l))


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


def assert_slice(
    expr: Expression, sub_expr: Expression, sub_width: Width, index: int, width: Width
) -> None:
    needs_shift = index != 0
    shift = RShift(sub_expr, index) if needs_shift else sub_expr
    needs_trunc = sub_width > index + width
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
        assert shift_expr.expr == sub_expr
    else:
        assert shift_expr == sub_expr


def assert_trunc(
    expr: Expression, sub_expr: Expression, sub_width: Width, width: Width
) -> None:
    assert_slice(expr, sub_expr, sub_width, 0, width)
