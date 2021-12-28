from __future__ import annotations

from retroasm.expression import Expression, IntLiteral, truncate
from retroasm.types import IntType

from .utils_expression import TestValue, makeConcat


def assertExprEqual(expr1: Expression, expr2: Expression) -> None:
    assert expr1 == expr2
    assert expr2 == expr1
    assert (expr1 == expr2) is True
    assert (expr2 == expr1) is True
    assert (expr1 != expr2) is False
    assert (expr2 != expr1) is False


def assertExprNotEqual(expr1: Expression, expr2: Expression) -> None:
    assert expr1 != expr2
    assert expr2 != expr1
    assert (expr1 == expr2) is False
    assert (expr2 == expr1) is False
    assert (expr1 != expr2) is True
    assert (expr2 != expr1) is True


def test_int() -> None:
    """Checks integer literals for equality."""
    arg1a = IntLiteral(1)
    arg1b = IntLiteral(1)
    arg2 = IntLiteral(2)
    assertExprEqual(arg1a, arg1b)
    assertExprNotEqual(arg1a, arg2)
    assertExprEqual(arg2, arg2)


def test_type_mismatch() -> None:
    """Checks equality checks between mismatching types."""
    zero = IntLiteral(0)
    addr = TestValue("A", IntType.u(16))
    assertExprNotEqual(zero, 0)  # type: ignore[arg-type]
    assertExprNotEqual(addr, "A")  # type: ignore[arg-type]
    assertExprNotEqual(zero, addr)


def test_concat_internal() -> None:
    """Checks equality between different concatenations of equal width."""
    four = IntLiteral(4)
    cat_u4_u8 = makeConcat(four, four, 8)
    cat_u8_u4 = makeConcat(four, four, 4)
    # Test expression being equal to itself.
    assertExprEqual(cat_u4_u8, cat_u4_u8)
    assertExprEqual(cat_u8_u4, cat_u8_u4)
    # Test that position of integer within concatenation is considered.
    assertExprNotEqual(cat_u4_u8, cat_u8_u4)


def test_truncate_subexpr() -> None:
    """Checks equality between truncations with differing subexpressions."""
    trunc1 = truncate(IntLiteral(0x1234), 8)
    trunc2 = truncate(IntLiteral(0x5678), 8)
    assertExprEqual(trunc1, trunc1)
    assertExprEqual(trunc2, trunc2)
    assertExprNotEqual(trunc1, trunc2)


def test_truncate_width() -> None:
    """Checks equality between truncations with differing widths."""
    addr = IntLiteral(0x456)
    trunc1 = truncate(addr, 8)  # $56
    trunc2 = truncate(addr, 12)  # $456
    trunc3 = truncate(addr, 16)  # $0456
    assertExprEqual(trunc1, trunc1)
    assertExprEqual(trunc2, trunc2)
    assertExprEqual(trunc3, trunc3)
    assertExprNotEqual(trunc1, trunc2)
    assertExprNotEqual(trunc1, trunc3)
    assertExprNotEqual(trunc2, trunc3)
