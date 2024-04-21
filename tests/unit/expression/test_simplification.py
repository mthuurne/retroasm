from __future__ import annotations

import pytest

from retroasm.expression import (
    BadValue,
    Expression,
    IntLiteral,
    OrOperator,
    SignExtension,
    SignTest,
)
from retroasm.expression_simplifier import simplify_expression
from retroasm.types import IntType, unlimited

from .conftest import Equation
from .utils import (
    TestValue,
    assert_int_literal,
    assert_slice,
    make_concat,
)


def test_zero_literal() -> None:
    """Verify that the same object is returned."""
    zero = IntLiteral(0)
    assert simplify_expression(zero) is zero


def test_zeromask_variable(equation: Equation) -> None:
    """
    A value with width 0 is simplified to the literal 0.

    .. code-block:: expr

        Z = 0
        Z;Z = 0
        A[3:3] = 0
    """
    equation.check_simplify()


def test_and_literals(equation: Equation) -> None:
    """
    Apply logical AND to integer literals.

    .. code-block:: expr

        $E3 & $7A = $62
        $FF00 & $123456 = $3400
    """
    equation.check_simplify()


def test_and_identity(equation: Equation) -> None:
    """
    Simplify logical AND expressions containing -1.

    .. code-block:: expr

        -1 & A = A
        A & -1 = A
        -1 & A & -1 = A
        -1 & -1 & -1 = -1
    """
    equation.check_simplify()


def test_and_absorbtion(equation: Equation) -> None:
    """
    Simplify logical AND expressions containing 0.

    .. code-block:: expr

        0 & A = 0
        A & 0 = 0
        0 & A & 0 = 0
        0 & 0 & 0 = 0
    """
    equation.check_simplify()


def test_and_idempotence(equation: Equation) -> None:
    """
    Simplify logical AND expressions containing duplicates.

    .. code-block:: expr

        A & A = A
        A & A & A = A
        A & B & A = A & B
    """
    equation.check_simplify()


def test_and_width(equation: Equation) -> None:
    """
    Simplify logical AND expressions using the subexpression widths.

    .. code-block:: expr

        H;L & $00FF = L
        H;L & $00F0 = L & $00F0
        (H;L)[:6] = L & $003F
        H;L & L = L
        H;L & H = L & H
        $F000 & L = 0
    """
    equation.check_simplify()


def test_and_mask_literal(equation: Equation) -> None:
    """
    Simplify logical AND expressions that mask part of an expression.

    .. code-block:: expr

        H;L & $FF00 = H << 8
        -(A & $3FFF) & $3FF0 = -A & $3FF0
    """
    equation.check_simplify()


def test_or_literals(equation: Equation) -> None:
    """
    Apply logical OR to integer literals.

    .. code-block:: expr

        $4C | $91 = $DD
        $00FF | $120021 = $1200FF
    """
    equation.check_simplify()


def test_or_identity(equation: Equation) -> None:
    """
    Simplify logical OR expressions containing 0.

    .. code-block:: expr

        A | 0 = A
        0 | A = A
        0 | A | 0 = A
        0 | 0 | 0 = 0
    """
    equation.check_simplify()


def test_or_absorbtion(equation: Equation) -> None:
    """
    Simplify logical OR expressions containing -1.

    .. code-block:: expr

        -1 | A = -1
        A | -1 = -1
        -1 | A | -1 = -1
        -1 | -1 | -1 = -1
    """
    equation.check_simplify()


def test_or_idempotence(equation: Equation) -> None:
    """
    Simplify logical OR expressions containing duplicates.

    .. code-block:: expr

        A | A = A
        A | A | A = A
        A | B | A = A | B
    """
    equation.check_simplify()


def test_or_and(equation: Equation) -> None:
    """
    Simplify expressions containing both OR and AND.

    .. code-block:: expr

        (L | $5500) & $AAFF = L
        (L & $55) | $AA = L | $AA
        ((L << 4) | $FF) & L = L
    """
    equation.check_simplify()


def test_xor_literals(equation: Equation) -> None:
    """
    Apply logical XOR to integer literals.

    .. code-block:: expr

        $DC ^ $58 = $84
        $F00F ^ $123456 = $12C459
    """
    equation.check_simplify()


def test_xor_identity(equation: Equation) -> None:
    """
    Simplify logical XOR expressions containing 0.

    .. code-block:: expr

        A ^ 0 = A
        0 ^ A = A
        0 ^ A ^ 0 = A
        0 ^ 0 ^ 0 = 0
    """
    equation.check_simplify()


def test_xor_deduplication(equation: Equation) -> None:
    """
    Simplify logical XOR expressions containing duplicates.

    .. code-block:: expr

        A ^ A = 0
        A ^ A ^ A = A
        A ^ A ^ A ^ A = 0
        A ^ B ^ A = B
        A ^ B ^ B ^ A = 0
    """
    equation.check_simplify()


def test_xor_bitwise_complement(equation: Equation) -> None:
    """
    Simplify XOR expressions used for bitwise complement.

    .. code-block:: expr

        -1 ^ (-1 ^ A) = A
    """
    equation.check_simplify()


def test_complement_literal(equation: Equation) -> None:
    """
    Take the complement of an integer literal.

    .. code-block:: expr

        -(4) = -4
        -0 = 0
    """
    equation.check_simplify()


def test_complement_twice(equation: Equation) -> None:
    """
    Take the complement of a complement.

    .. code-block:: expr

        --A = A
        -(-(-A)) = -A
    """
    equation.check_simplify()


def test_complement_subexpr(equation: Equation) -> None:
    """
    Take the complement of a simplifiable subexpression.

    .. code-block:: expr

        -(A & 0) = 0
        -(A | B | A) = -(A | B)
        -($C0;$DE;H;L) = -($C0DE;H;L)
    """
    equation.check_simplify()


def test_negation_literal(equation: Equation) -> None:
    """
    Negate an integer literal.

    .. code-block:: expr

        !-2 = 0
        !-1 = 0
        !0 = 1
        !1 = 0
        !2 = 0
    """
    equation.check_simplify()


def test_negation_subexpr(equation: Equation) -> None:
    """
    Negate a subexpression that simplifies to a literal.

    .. code-block:: expr

        !($0;$00) = 1
        !($B;$00) = 0
        !($0;$07) = 0
        !(A & 0) = 1
        !(A | -1) = 0
        !(L >> 8) = 1
    """
    equation.check_simplify()


def test_negation_nested(equation: Equation) -> None:
    """
    Negate a subexpression that doesn't simplify to a literal.

    When the subexpression can never be zero, the negation is zero.
    Otherwise, it is just the negation of the simplified subexpression.

    .. code-block:: expr

        !(A ^ A ^ A) = !A
        !(A | $76) = 0
        !(-A | $76) = 0
        !(-(A | $76)) = 0
        !(($60 | A) & $F0) = 0
        !(L ^ -1) = 0
        !(A ^ -1) = !(A ^ -1)
        !(L + 1) = 0
        !(A + 1) = !(A + 1)
        !((L + 1) << 8) = 0
        !((A | $345) >> 8) = 0
    """
    equation.check_simplify()


def test_negation_twice(equation: Equation) -> None:
    """
    Negate a negation.

    .. code-block:: expr

        !!A = !!A
        !!F = F
        !!!A = !A
        !!!F = !F
        !!(A & F) = A & F
    """
    equation.check_simplify()


def test_comparison_literals(equation: Equation) -> None:
    """
    Tests sign of several integer literals.

    .. code-block:: expr

        0 < 0 = 0
        0 > 0 = 0
        0 <= 0 = 1
        0 >= 0 = 1
        1 < 0 = 0
        1 > 0 = 1
        1 <= 0 = 0
        1 >= 0 = 1
        -1 < 0 = 1
        -1 > 0 = 0
        -1 <= 0 = 1
        -1 >= 0 = 0
        123 < 0 = 0
        123 > 0 = 1
        123 <= 0 = 0
        123 >= 0 = 1
        -123 < 0 = 1
        -123 > 0 = 0
        -123 <= 0 = 1
        -123 >= 0 = 0
    """
    equation.check_simplify()


def test_sign_types(equation: Equation) -> None:
    """
    Signed values like A can be negative, but unsigned values like L cannot.

    .. code-block:: expr

        L < 0 = 0
        L > 0 = L > 0
        L >= 0 = 1
        L <= 0 = L <= 0
        A < 0 = A < 0
        A > 0 = A > 0
        A >= 0 = A >= 0
        A <= 0 = A <= 0
    """
    equation.check_simplify()


def test_sign_extended() -> None:
    """Test sign of sign extended values."""
    v = TestValue("V", IntType.int)
    assert_slice(simplify_expression(SignTest(SignExtension(v, 8))), v, unlimited, 7, 1)
    # A zero-width value has no sign bit; sign test should return 0.
    z = TestValue("Z", IntType.u(0))
    assert_int_literal(simplify_expression(SignTest(SignExtension(z, 0))), 0)


sign_extension_data = [
    (123, 8, 123),
    (-123, 8, -123),
    (-123 & 0xFF, 8, -123),
    (0x123456, 8, 0x56),
    (0x89ABCD, 8, 0xCD - 0x100),
    (0, 0, 0),
    (0, 1, 0),
    (1, 1, -1),
]


@pytest.mark.parametrize(
    "value,width,expected",
    sign_extension_data,
    ids=[f"${value:X} {width}-bit" for value, width, _expected in sign_extension_data],
)
def test_sign_extend_int(value: int, width: int, expected: int) -> None:
    """Apply sign extension to several integer literals."""
    assert_int_literal(
        simplify_expression(SignExtension(IntLiteral(value), width)), expected
    )


def test_sign_extend_mask_concat() -> None:
    """Applies sign extension to concatenated values."""
    h = TestValue("H", IntType.u(8))
    l = TestValue("L", IntType.u(8))
    hl = make_concat(h, l, 8)
    assert simplify_expression(SignExtension(hl, 8)) == SignExtension(l, 8)


def test_sign_extend_clear() -> None:
    """Removes sign extension when sign bit is known to be zero."""
    h = TestValue("H", IntType.u(8))
    l = TestValue("L", IntType.u(8))
    hgapl = make_concat(h, l, 9)
    assert simplify_expression(SignExtension(hgapl, 9)) is l


def test_sign_extend_set() -> None:
    """Removes sign extension when sign bit is known to be one."""
    a = TestValue("A", IntType.u(8))
    b = IntLiteral(0x80)
    combi = OrOperator(a, b)
    assert simplify_expression(SignExtension(combi, 8)) == OrOperator(
        a, IntLiteral(~0x7F)
    )


def test_add_literals(equation: Equation) -> None:
    """
    Add integer literals.

    .. code-block:: expr

        3 + 20 = 23
        8 + 127 = 135
        -5 + 2 = -3
        (1 + 2) + (3 + 4) = 10
        (0 + -39) + -(101 + -1001) = 861
    """
    equation.check_simplify()


def test_add_zero(equation: Equation) -> None:
    """
    Remove literal zero terms.

    .. code-block:: expr

        A + 0 = A
        0 + A = A
        0 + A + 0 = A
        0 + 0 = 0
    """
    equation.check_simplify()


def test_add_associative(equation: Equation) -> None:
    """
    Simplify using the associativity of addition.

    Note that subtraction is converted into addition of the complement.

    .. code-block:: expr

        (A + 1) + (2 + -3) = A
        (A - 1) - (2 + -3) = A
    """
    equation.check_simplify()


def test_add_commutative(equation: Equation) -> None:
    """
    Simplify using the commutativity of addition.

    Note that subtraction is converted into addition of the complement.

    .. code-block:: expr

        1 + 2 + A + -3 = A
        1 + 2 - (-A + 3) = A
    """
    equation.check_simplify()


def test_add_complement(equation: Equation) -> None:
    """
    Simplify subtracting an expression from itself.

    .. code-block:: expr

        A - A = 0
        -A + A = 0
        -A - -A = 0
        A + B - A = B
        (A + B) - (A + B) = 0
        (A + B) - (B + A) = 0
        H;L - H;L = 0
    """
    equation.check_simplify()


def test_add_truncate(equation: Equation) -> None:
    """
    Simplify addition results that are truncated.

    This happens a lot, as expressions have unlimited width, but registers
    and I/O channels do not.

    .. code-block:: expr

        ((L + 1)[:8] - 1)[:8] = L
        (H;L + 1)[:8] = (L + 1)[:8]
        (H;L + $10001)[:16] = (H;L + 1)[:16]
    """
    equation.check_simplify()


def test_shift_literals(equation: Equation) -> None:
    """
    Shift integer literals left and right.

    .. code-block:: expr

        $1234 << 4 = $12340
        $1234 >> 4 = $123
        $DA << 16 = $DA0000
        $DA >> 16 = 0
        -1 << 4 = -16
        -16 >> 2 = -4
        -1 >> 4 = -1
    """
    equation.check_simplify()


def test_shift_symbols(equation: Equation) -> None:
    """
    Shift symbols one or multiple times.

    .. code-block:: expr

        A << 0 = A
        A >> 0 = A
        (A << 5) << 3 = A << 8
        (A << 5) >> 3 = A << 2
        (A << 3) >> 5 = A >> 2
        (A << 4) >> 4 = A
        (A >> 3) << 5 = (A << 2) & -$20
    """
    equation.check_simplify()


def test_shift_truncate(equation: Equation) -> None:
    """
    Truncate a shifted expression.

    .. code-block:: expr

        L >> 8 = 0
        H;L >> 8 = H
        H;L >> 16 = 0
        (H;L << 8)[:8] = 0
        (H;L << 8)[:16] = L << 8
        (H;L << 8)[:32] = H;L << 8
        H >> (L >> 8) = H
    """
    equation.check_simplify()


def test_shift_negative(expression: Expression) -> None:
    """
    Shifting by a negative amount is an error.

    .. code-block:: expr

        A << -1
        A >> -1
        A << (2 - 345)
        A >> (2 - 345)
    """
    assert isinstance(simplify_expression(expression), BadValue)


def test_concat_literals(equation: Equation) -> None:
    """
    Concatenate integer literals.

    .. code-block:: expr

        4;$D = $4D
        4;$29 = $429
        -4;$D = -$33
        -4;$29 = -$3D7
        $D;$D = $DD
        $D;$29 = $D29
        $29;$D = $29D
        $29;$29 = $2929
    """
    equation.check_simplify()


def test_concat_identity(equation: Equation) -> None:
    """
    Simplify concatenations containing identity values.

    .. code-block:: expr

        0;L = L
        Z;L = L
        H;Z = H
        Z;H;L = H;L
        H;Z;L = H;L
        H;L;Z = H;L
        Z;Z;H;Z;Z;L;Z;Z = H;L
        H;$0;L = H;$0;L
    """
    equation.check_simplify()


def test_concat_associative(equation: Equation) -> None:
    """
    Simplify using the associativity of concatenation.

    .. code-block:: expr

        (H;L) ; H ; (L;H;L) = H;L;H;L;H;L
        (A;$9) ; ($63;H; L) = A;$963;H;L
    """
    equation.check_simplify()


def test_slice_literals(equation: Equation) -> None:
    """
    Slice integer literals.

    .. code-block:: expr

        $FD56[8:] = $FD
        $FD56[:8] = $56
        $FD56[0:16] = $FD56
        $FD56[4:12] = $D5
        $FD56[8:20] = $FD
        (-$1995)[:16] = $E66B
        (-$1995)[4:12] = $66
        (-$1995)[8:20] = $FE6
        (-$1995)[8:] = -$1A
    """
    equation.check_simplify()


def test_slice_range(equation: Equation) -> None:
    """
    Slice various ranges from a fixed-width symbol.

    .. code-block:: expr

        L[:8] = L
        L[:12] = L
        L[8:] = 0
        L[4:12] = L[4:]
    """
    equation.check_simplify()


def test_slice_of_slice(equation: Equation) -> None:
    """
    Slice a range from another slice.

    .. code-block:: expr

        A[3:13][2:8] = A[5:11]
        (7 ; (H;L)[8:20])[8:16] = $70
    """
    equation.check_simplify()


def test_slice_concat(equation: Equation) -> None:
    """
    Slice a range from a concatenation.

    .. code-block:: expr

        (A;H;L)[:8] = L
        (A;H;L)[8:16] = H
        (A;H;L)[16:] = A
        (A;H;L)[8:] = A;H
        (A;H;L)[:16] = H;L
        (H;L)[2:5] = L[2:5]
        (H;L)[11:14] = H[3:6]
        (H;L)[:5] = L[:5]
        (H;L)[8:13] = H[:5]
        (H;L)[5:8] = L[5:]
        (H;L)[13:] = H[5:]
    """
    # TODO: Also test slices that don't end at term boundaries.
    #       However, these currently don't simplify consistently.
    equation.check_simplify()


def test_slice_and(equation: Equation) -> None:
    """
    Simplify slicing a logical AND.

    .. code-block:: expr

        (H;L & $BFFF)[8:14] = H[:6]
        (H & L)[:8] = H & L
    """
    equation.check_simplify()


def test_slice_add(equation: Equation) -> None:
    """
    Simplify slicing an addition.

    .. code-block:: expr

        (H;L + 2)[:6] = (L + 2)[:6]
        (H;L + 2)[:8] = (L + 2)[:8]
        (H;L + 2)[:10] = (H;L + 2)[:10]
        (H;L + 2)[8:] = (H;L + 2)[8:]
    """
    equation.check_simplify()


def test_slice_complement(equation: Equation) -> None:
    """
    Simplify slicing a complement.

    .. code-block:: expr

        (-(H;L))[:6] = (-L)[:6]
        (-(H;L))[:8] = (-L)[:8]
        (-(H;L))[:10] = (-(H;L))[:10]
        (-(H;L))[8:] = (-(H;L))[8:]
    """
    equation.check_simplify()
