from pytest import raises

from retroasm.types import IntType


def test_inttype_range_check_unlimited() -> None:
    """Any integer value fits in the unlimited-width 'int' type."""

    # Near zero.
    IntType.int.checkRange(0)
    IntType.int.checkRange(1)
    IntType.int.checkRange(-1)

    # Big numbers.
    IntType.int.checkRange(1 << 100)
    IntType.int.checkRange(-1 << 100)


def test_inttype_range_check_unsigned() -> None:
    """Unsigned integer types support values in the range [0..2**N)."""

    # Near zero.
    IntType.u(8).checkRange(0)
    IntType.u(8).checkRange(1)
    with raises(ValueError, match="^value -1 does not fit in type u123$"):
        IntType.u(123).checkRange(-1)

    # Near upper bound.
    IntType.u(8).checkRange(255)
    with raises(ValueError, match="^value 256 does not fit in type u8$"):
        IntType.u(8).checkRange(256)
    IntType.u(9).checkRange(256)

    # The type u0 only contains the value zero.
    IntType.u(0).checkRange(0)
    with raises(ValueError, match="^value 1 does not fit in type u0$"):
        IntType.u(0).checkRange(1)
    with raises(ValueError, match="^value -1 does not fit in type u0$"):
        IntType.u(0).checkRange(-1)


def test_inttype_range_check_signed() -> None:
    """Signed integer types support values in the range [-2**(N-1)..2**(N-1))."""

    # Near zero.
    IntType.s(8).checkRange(0)
    IntType.s(8).checkRange(1)
    IntType.s(8).checkRange(-1)

    # Near upper bound.
    IntType.s(8).checkRange(127)
    with raises(ValueError, match="^value 128 does not fit in type s8$"):
        IntType.s(8).checkRange(128)
    IntType.s(9).checkRange(128)

    # Near lower bound.
    IntType.s(8).checkRange(-128)
    with raises(ValueError, match="^value -129 does not fit in type s8$"):
        IntType.s(8).checkRange(-129)
    IntType.s(9).checkRange(-129)

    # The type s0 only contains the value 0.
    IntType.s(0).checkRange(0)
    with raises(ValueError, match="^value 1 does not fit in type s0$"):
        IntType.s(0).checkRange(1)
    with raises(ValueError, match="^value -1 does not fit in type s0$"):
        IntType.s(0).checkRange(-1)

    # The type s1 contains the values 0 and -1.
    IntType.s(1).checkRange(0)
    IntType.s(1).checkRange(-1)
    with raises(ValueError, match="^value 1 does not fit in type s1$"):
        IntType.s(1).checkRange(1)
    with raises(ValueError, match="^value -2 does not fit in type s1$"):
        IntType.s(1).checkRange(-2)
