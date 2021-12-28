from __future__ import annotations

from pytest import raises

from retroasm.types import IntType, unlimited


def test_inttype_range_check_unlimited() -> None:
    """Any integer value fits in the unlimited-width 'int' type."""

    # Near zero.
    IntType.int.check_range(0)
    IntType.int.check_range(1)
    IntType.int.check_range(-1)

    # Big numbers.
    IntType.int.check_range(1 << 100)
    IntType.int.check_range(-1 << 100)


def test_inttype_range_check_unsigned_unlimited() -> None:
    """
    Any non-negative integer value fits in the unlimited-width 'uint' type.

    While this type does not exist in the instruction set definition language,
    the API does support it.
    """

    uint = IntType(unlimited, False)

    # Near zero.
    uint.check_range(0)
    uint.check_range(1)
    with raises(ValueError, match="^value -1 does not fit in type uint$"):
        uint.check_range(-1)

    # Big numbers.
    uint.check_range(1 << 100)
    with raises(ValueError, match=r"^value -1267\d* does not fit in type uint$"):
        uint.check_range(-1 << 100)


def test_inttype_range_check_unsigned() -> None:
    """Unsigned integer types support values in the range [0..2**N)."""

    # Near zero.
    IntType.u(8).check_range(0)
    IntType.u(8).check_range(1)
    with raises(ValueError, match="^value -1 does not fit in type u123$"):
        IntType.u(123).check_range(-1)

    # Near upper bound.
    IntType.u(8).check_range(255)
    with raises(ValueError, match="^value 256 does not fit in type u8$"):
        IntType.u(8).check_range(256)
    IntType.u(9).check_range(256)

    # The type u0 only contains the value zero.
    IntType.u(0).check_range(0)
    with raises(ValueError, match="^value 1 does not fit in type u0$"):
        IntType.u(0).check_range(1)
    with raises(ValueError, match="^value -1 does not fit in type u0$"):
        IntType.u(0).check_range(-1)


def test_inttype_range_check_signed() -> None:
    """Signed integer types support values in the range [-2**(N-1)..2**(N-1))."""

    # Near zero.
    IntType.s(8).check_range(0)
    IntType.s(8).check_range(1)
    IntType.s(8).check_range(-1)

    # Near upper bound.
    IntType.s(8).check_range(127)
    with raises(ValueError, match="^value 128 does not fit in type s8$"):
        IntType.s(8).check_range(128)
    IntType.s(9).check_range(128)

    # Near lower bound.
    IntType.s(8).check_range(-128)
    with raises(ValueError, match="^value -129 does not fit in type s8$"):
        IntType.s(8).check_range(-129)
    IntType.s(9).check_range(-129)

    # The type s0 only contains the value 0.
    IntType.s(0).check_range(0)
    with raises(ValueError, match="^value 1 does not fit in type s0$"):
        IntType.s(0).check_range(1)
    with raises(ValueError, match="^value -1 does not fit in type s0$"):
        IntType.s(0).check_range(-1)

    # The type s1 contains the values 0 and -1.
    IntType.s(1).check_range(0)
    IntType.s(1).check_range(-1)
    with raises(ValueError, match="^value 1 does not fit in type s1$"):
        IntType.s(1).check_range(1)
    with raises(ValueError, match="^value -2 does not fit in type s1$"):
        IntType.s(1).check_range(-2)
