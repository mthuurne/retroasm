from __future__ import annotations

from retroasm.expression import (
    AddOperator,
    AndOperator,
    Complement,
    Expression,
    IntLiteral,
    LShift,
    LVShift,
    Negation,
    OrOperator,
    RShift,
    RVShift,
    SignExtension,
    SignTest,
    XorOperator,
    truncate,
)
from retroasm.expression_simplifier import simplify_expression
from retroasm.types import IntType, unlimited

from .conftest import Equation
from .utils import (
    TestValue,
    assert_and,
    assert_concat,
    assert_int_literal,
    assert_slice,
    make_concat,
    make_slice,
)


def test_zero_literal() -> None:
    """Verify that the same object is returned."""
    zero = IntLiteral(0)
    assert simplify_expression(zero) is zero


def test_zeromask_variable() -> None:
    """Verify that a value with width 0 is simplified to the literal 0."""
    zvar = TestValue("Z", IntType.u(0))
    assert_int_literal(simplify_expression(zvar), 0)


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


def test_add_literals(equation: Equation) -> None:
    """
    Add integer literals.

    .. code-block:: expr

        3 + 20 = 23
        8 + 127 = 135
        -5 + 2 = -3
        (1 + 2) + (3 + 4) = 10
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

    .. code-block:: expr

        (A + 1) + (2 + -3) = A
    """
    equation.check_simplify()


def test_add_commutative(equation: Equation) -> None:
    """
    Simplify using the commutativity of addition.

    .. code-block:: expr

        1 + 2 + A + -3 = A
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


def test_negation_int() -> None:
    """Negates an integer literal."""
    assert_int_literal(simplify_expression(Negation(IntLiteral(-1))), 0)
    assert_int_literal(simplify_expression(Negation(IntLiteral(0))), 1)
    assert_int_literal(simplify_expression(Negation(IntLiteral(1))), 0)
    assert_int_literal(simplify_expression(Negation(IntLiteral(2))), 0)
    assert_int_literal(simplify_expression(Negation(IntLiteral(3))), 0)


def test_negation_subexpr() -> None:
    """Negates a simplifiable subexpression."""
    assert_int_literal(
        simplify_expression(
            Negation(make_concat(IntLiteral(0x0), IntLiteral(0x00), 8))
        ),
        1,
    )
    assert_int_literal(
        simplify_expression(
            Negation(make_concat(IntLiteral(0xB), IntLiteral(0x00), 8))
        ),
        0,
    )
    assert_int_literal(
        simplify_expression(
            Negation(make_concat(IntLiteral(0x0), IntLiteral(0x07), 8))
        ),
        0,
    )


def test_negation_or() -> None:
    """Negates an OR expression."""
    addr = TestValue("A", IntType.u(16))
    assert_int_literal(
        simplify_expression(Negation(OrOperator(addr, IntLiteral(0x76)))), 0
    )


def test_negation_and() -> None:
    """Negates an AND expression."""
    addr = TestValue("A", IntType.u(16))
    assert_int_literal(
        simplify_expression(
            Negation(AndOperator(OrOperator(IntLiteral(0x60), addr), IntLiteral(0xF0)))
        ),
        0,
    )


def test_negation_xor() -> None:
    """Negates a XOR expression."""
    addr = TestValue("A", IntType.u(16))
    assert_int_literal(
        simplify_expression(Negation(XorOperator(addr, IntLiteral(-1)))), 0
    )


def test_negation_add() -> None:
    """Negates an addition."""
    addr = TestValue("A", IntType.u(16))
    assert_int_literal(
        simplify_expression(Negation(AddOperator(addr, IntLiteral(1)))), 0
    )


def test_negation_complement() -> None:
    """Negates a complement."""
    addr = TestValue("A", IntType.u(16))
    assert_int_literal(
        simplify_expression(Negation(Complement(OrOperator(addr, IntLiteral(0x76))))), 0
    )


def test_negation_twice() -> None:
    """Negates a negation."""
    bool_val = TestValue("B", IntType.u(1))
    int_val = TestValue("I", IntType.u(16))
    assert simplify_expression(Negation(Negation(bool_val))) is bool_val
    not_not_int = Negation(Negation(int_val))
    assert simplify_expression(not_not_int) is not_not_int
    combi = AndOperator(bool_val, int_val)
    assert simplify_expression(Negation(Negation(combi))) is combi


def test_negation_lshift() -> None:
    """Negates a left-shifted expression."""
    addr = TestValue("A", IntType.u(16))
    assert_int_literal(
        simplify_expression(Negation(LShift(AddOperator(addr, IntLiteral(1)), 8))), 0
    )


def test_negation_rshift() -> None:
    """Negates a right-shifted expression."""
    addr = TestValue("A", IntType.u(16))
    assert_int_literal(
        simplify_expression(Negation(RShift(OrOperator(addr, IntLiteral(0x345)), 8))), 0
    )


def test_sign_int() -> None:
    """Tests sign of several integer literals."""

    def check(value: int, result: int) -> None:
        assert_int_literal(simplify_expression(SignTest(IntLiteral(value))), result)

    check(0, 0)
    check(1, 0)
    check(-1, 1)
    check(123, 0)
    check(-123, 1)


def test_sign_types() -> None:
    """Test sign of values of signed and unsigned types."""
    u = SignTest(TestValue("U", IntType.u(8)))
    assert_int_literal(simplify_expression(u), 0)
    s = SignTest(TestValue("S", IntType.s(8)))
    assert simplify_expression(s) is s


def test_sign_extended() -> None:
    """Test sign of sign extended values."""
    v = TestValue("V", IntType.int)
    assert_slice(simplify_expression(SignTest(SignExtension(v, 8))), v, unlimited, 7, 1)
    # A zero-width value has no sign bit; sign test should return 0.
    z = TestValue("Z", IntType.u(0))
    assert_int_literal(simplify_expression(SignTest(SignExtension(z, 0))), 0)


def test_sign_extend_int() -> None:
    """Applies sign extension to several integer literals."""

    def check(value: int, width: int, result: int) -> None:
        assert_int_literal(
            simplify_expression(SignExtension(IntLiteral(value), width)), result
        )

    check(123, 8, 123)
    check(-123, 8, -123)
    check(-123 & 0xFF, 8, -123)
    check(0x123456, 8, 0x56)
    check(0x89ABCD, 8, 0xCD - 0x100)
    check(0, 0, 0)
    check(0, 1, 0)
    check(1, 1, -1)


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


def test_arithmetic_int() -> None:
    """Uses add/complement on several integer literals."""
    expr = AddOperator(
        AddOperator(IntLiteral(0), Complement(IntLiteral(39))),
        Complement(AddOperator(IntLiteral(101), IntLiteral(-1001))),
    )
    assert_int_literal(simplify_expression(expr), 861)


def test_arithmetic_associative() -> None:
    """Test simplification using the associativity of addition.
    Note that that associativity cannot be exploited unless the subtraction
    is converted into an addition first.
    """
    addr = TestValue("A", IntType.u(16))
    arg1 = AddOperator(addr, Complement(IntLiteral(1)))
    arg2 = AddOperator(IntLiteral(-2), IntLiteral(3))
    assert simplify_expression(AddOperator(arg1, arg2)) is addr


def test_arithmetic_commutative() -> None:
    """Test simplification using the commutativity of addition.
    Note that that commutativity cannot be exploited unless the subtraction
    is converted into an addition first.
    """
    addr = TestValue("A", IntType.u(16))
    arg1 = AddOperator(IntLiteral(1), IntLiteral(2))
    arg2 = AddOperator(Complement(addr), IntLiteral(3))
    assert simplify_expression(AddOperator(arg1, Complement(arg2))) is addr


def test_arithmetic_add_complement() -> None:
    """Test simplification of subtracting an expression from itself."""
    a = TestValue("A", IntType.u(16))
    b = TestValue("B", IntType.u(8))
    assert_int_literal(simplify_expression(AddOperator(a, Complement(a))), 0)
    c = AddOperator(a, b)
    d = AddOperator(b, a)
    assert_int_literal(simplify_expression(AddOperator(c, Complement(c))), 0)
    assert_int_literal(simplify_expression(AddOperator(c, Complement(d))), 0)
    e = make_concat(a, b, 8)
    assert_int_literal(simplify_expression(AddOperator(e, Complement(e))), 0)


def test_arithmetic_add_truncate() -> None:
    """Test simplification of truncation of adding truncated expressions."""
    a = TestValue("A", IntType.u(16))
    expr = truncate(
        AddOperator(truncate(AddOperator(a, IntLiteral(1)), 16), IntLiteral(-1)), 16
    )
    assert str(simplify_expression(expr)) is str(a)
    assert simplify_expression(expr) is a


def test_arithmetic_add_truncate_literal() -> None:
    """Test simplification of truncation of added literal."""
    a = TestValue("A", IntType.u(16))
    expr = truncate(AddOperator(a, IntLiteral(0x10001)), 16)
    expected = truncate(AddOperator(a, IntLiteral(1)), 16)
    assert simplify_expression(expr) == expected


def test_lshift_literals() -> None:
    """Shifts an integer literal to the left."""
    assert_int_literal(simplify_expression(LShift(IntLiteral(0x1234), 8)), 0x123400)
    assert_int_literal(simplify_expression(LShift(IntLiteral(0xDA), 16)), 0xDA0000)


def test_lshift_twice() -> None:
    """Shifts a value to the left twice."""
    addr = TestValue("A", IntType.u(16))
    expr = simplify_expression(LShift(LShift(addr, 3), 5))
    assert isinstance(expr, LShift)
    assert expr.expr is addr
    assert expr.offset == 8


def test_lshift_rshift() -> None:
    """Tests left-shifting after right-shifting."""
    addr = TestValue("A", IntType.u(16))
    # Shift more to the right than to the left.
    rwin = simplify_expression(LShift(RShift(addr, 5), 3))
    assert_slice(rwin, AndOperator(addr, IntLiteral(0xFFE0)), 16, 2, 14)
    # Shift equal amounts to the right and to the left.
    draw = simplify_expression(LShift(RShift(addr, 4), 4))
    assert_and(draw, addr, IntLiteral(0xFFF0))
    # Shift less to the right than to the left.
    lwin = simplify_expression(LShift(RShift(addr, 3), 5))
    assert_and(lwin, LShift(addr, 2), IntLiteral(0x3FFE0))


def test_lshift_truncate() -> None:
    """Tests truncation of a left-shifted expression."""
    h = TestValue("H", IntType.u(8))
    l = TestValue("L", IntType.u(8))
    hl = make_concat(h, l, 8)
    # Shift H and L out of the truncation range.
    expr1 = simplify_expression(truncate(LShift(hl, 8), 8))
    assert_int_literal(expr1, 0)
    # Shift only H out of the truncation range.
    expr2 = simplify_expression(truncate(LShift(hl, 8), 16))
    assert isinstance(expr2, LShift)
    assert expr2.expr is l
    assert expr2.offset == 8


def test_rshift_lshift() -> None:
    """Tests right-shifting after left-shifting."""
    addr = TestValue("A", IntType.u(16))
    # Shift less to the left than to the right.
    rwin = simplify_expression(RShift(LShift(addr, 3), 5))
    assert isinstance(rwin, RShift)
    assert rwin.offset == 2
    assert rwin.expr is addr
    # Shift equal amounts to the left and to the right.
    draw = simplify_expression(RShift(LShift(addr, 4), 4))
    assert draw is addr
    # Shift more to the left than to the right.
    lwin = simplify_expression(RShift(LShift(addr, 5), 3))
    assert isinstance(lwin, LShift)
    assert lwin.offset == 2
    assert lwin.expr is addr


def test_lvshift_constant() -> None:
    """Shifts a constant number of positions to the left."""
    assert_int_literal(
        simplify_expression(LVShift(IntLiteral(0x1234), IntLiteral(8))), 0x123400
    )
    v8 = TestValue("A", IntType.u(8))
    assert simplify_expression(LVShift(v8, IntLiteral(3))) == LShift(v8, 3)
    assert simplify_expression(LVShift(v8, TestValue("Z", IntType.u(0)))) == v8


def test_rvshift_constant() -> None:
    """Shifts a constant number of positions to the left."""
    assert_int_literal(
        simplify_expression(RVShift(IntLiteral(0x1234), IntLiteral(8))), 0x12
    )
    v8 = TestValue("A", IntType.u(8))
    assert simplify_expression(RVShift(v8, IntLiteral(3))) == RShift(v8, 3)
    assert simplify_expression(RVShift(v8, TestValue("Z", IntType.u(0)))) == v8


def test_concat_literals() -> None:
    """Concatenates integer literals."""
    ip = IntLiteral(4)
    im = Complement(ip)
    u4 = IntLiteral(0xD)
    u8 = IntLiteral(0x29)
    cat_ip_u4 = simplify_expression(make_concat(ip, u4, 4))
    assert_int_literal(cat_ip_u4, 0x40 + 0xD)
    cat_ip_u8 = simplify_expression(make_concat(ip, u8, 8))
    assert_int_literal(cat_ip_u8, 0x400 + 0x29)
    cat_im_u4 = simplify_expression(make_concat(im, u4, 4))
    assert_int_literal(cat_im_u4, -0x40 + 0xD)
    cat_im_u8 = simplify_expression(make_concat(im, u8, 8))
    assert_int_literal(cat_im_u8, -0x400 + 0x29)
    cat_u4_u4 = simplify_expression(make_concat(u4, u4, 4))
    assert_int_literal(cat_u4_u4, 0xDD)
    cat_u4_u8 = simplify_expression(make_concat(u4, u8, 8))
    assert_int_literal(cat_u4_u8, 0xD29)
    cat_u8_u4 = simplify_expression(make_concat(u8, u4, 4))
    assert_int_literal(cat_u8_u4, 0x29D)
    cat_u8_u8 = simplify_expression(make_concat(u8, u8, 8))
    assert_int_literal(cat_u8_u8, 0x2929)


def test_concat_identity() -> None:
    """Simplifies concatenations containing identity values."""
    addr = TestValue("A", IntType.u(16))
    # Check whether empty bitstrings are filtered out.
    empty = IntLiteral(0)
    head = make_concat(make_concat(empty, addr, 16), addr, 16)
    assert_concat(simplify_expression(head), ((addr, 16), (addr, 16)))
    mid = make_concat(make_concat(addr, empty, 0), addr, 16)
    assert_concat(simplify_expression(mid), ((addr, 16), (addr, 16)))
    tail = make_concat(make_concat(addr, addr, 16), empty, 0)
    assert_concat(simplify_expression(tail), ((addr, 16), (addr, 16)))
    many = make_concat(
        make_concat(
            make_concat(
                make_concat(
                    make_concat(
                        make_concat(make_concat(empty, empty, 0), addr, 16), empty, 0
                    ),
                    empty,
                    0,
                ),
                addr,
                16,
            ),
            empty,
            0,
        ),
        empty,
        0,
    )
    assert_concat(simplify_expression(many), ((addr, 16), (addr, 16)))
    # Check graceful handling when zero subexpressions remain.
    only = make_concat(make_concat(empty, empty, 0), empty, 0)
    assert_int_literal(simplify_expression(only), 0)
    # Check whether non-empty fixed-width zero-valued bitstrings are kept.
    zero_u8 = IntLiteral(0)
    mid_u8 = make_concat(make_concat(addr, zero_u8, 8), addr, 16)
    assert_concat(simplify_expression(mid_u8), ((addr, 16), (zero_u8, 8), (addr, 16)))
    tail_u8 = make_concat(make_concat(addr, addr, 16), zero_u8, 8)
    assert_concat(simplify_expression(tail_u8), ((addr, 16), (addr, 16), (zero_u8, 8)))
    # Check whether unlimited-width zero-valued bitstrings are kept.
    zero_int = IntLiteral(0)
    head_int = make_concat(make_concat(zero_int, addr, 16), addr, 16)
    assert_concat(
        simplify_expression(head_int), ((zero_int, unlimited), (addr, 16), (addr, 16))
    )


def test_concat_associative() -> None:
    """Test simplification using the associativity of concatenation."""
    addr = TestValue("A", IntType.u(16))
    arg1 = make_concat(addr, addr, 16)  # (A ; A)
    arg2 = make_concat(arg1, arg1, 32)  # ((A ; A) ; (A ; A))
    arg3 = make_concat(arg1, arg2, 64)  # ((A ; A) ; ((A ; A) ; (A ; A)))
    assert_concat(simplify_expression(arg3), ((addr, 16),) * 6)


def test_concat_associative2() -> None:
    """Test simplification using the associativity of concatenation."""
    addr = TestValue("A", IntType.u(16))
    arg1 = make_concat(addr, IntLiteral(0x9), 4)  # (A ; $9)
    arg2 = make_concat(IntLiteral(0x63), addr, 16)  # ($63 ; A)
    arg3 = make_concat(arg1, arg2, 24)  # ((A ; $9) ; ($63 ; A))
    assert_concat(
        simplify_expression(arg3), ((addr, 16), (IntLiteral(0x963), 12), (addr, 16))
    )


def simplify_slice(expr: Expression, index: int, width: int) -> Expression:
    return simplify_expression(make_slice(expr, index, width))


def test_slice_literals() -> None:
    """Slices integer literals."""
    addr = IntLiteral(0xFD56)
    assert_int_literal(simplify_slice(addr, 0, 16), 0xFD56)
    assert_int_literal(simplify_slice(addr, 4, 8), 0xD5)
    assert_int_literal(simplify_slice(addr, 8, 12), 0x0FD)
    signed = IntLiteral(-0x1995)
    assert_int_literal(simplify_slice(signed, 0, 16), 0xE66B)
    assert_int_literal(simplify_slice(signed, 4, 8), 0x66)
    assert_int_literal(simplify_slice(signed, 8, 12), 0xFE6)


def test_slice_zero_width() -> None:
    """Takes a slices of width 0."""
    addr = TestValue("A", IntType.u(16))
    assert_int_literal(simplify_slice(addr, 8, 0), 0)


def test_slice_full_range() -> None:
    """Slices a range that exactly matches a value's type."""
    addr = TestValue("A", IntType.u(16))
    assert simplify_slice(addr, 0, 16) is addr


def test_slice_out_of_range() -> None:
    """Slices a range that is fully outside a value's type."""
    addr = TestValue("A", IntType.u(16))
    assert_int_literal(simplify_slice(addr, 16, 8), 0)


def test_slice_leading_zeroes() -> None:
    """Slices a range that is partially outside a value's type."""
    addr = TestValue("A", IntType.u(16))
    expr = simplify_slice(addr, 0, 20)  # $0xxxx
    assert expr is addr
    expr = simplify_slice(addr, 8, 12)  # $0xx
    assert_slice(expr, addr, 16, 8, 8)


def test_slice_of_slice() -> None:
    """Slices a range from another slice."""
    addr = TestValue("A", IntType.u(16))
    expr = simplify_slice(make_slice(addr, 3, 10), 2, 6)
    assert_slice(expr, addr, 16, 5, 6)


def test_slice_concat() -> None:
    """Slices a range from a concatenation."""
    a = TestValue("A", IntType.u(8))
    b = TestValue("B", IntType.u(8))
    c = TestValue("C", IntType.u(8))
    d = TestValue("D", IntType.u(8))
    abcd = make_concat(make_concat(make_concat(a, b, 8), c, 8), d, 8)
    # Test slicing out individual values.
    assert simplify_slice(abcd, 0, 8) is d
    assert simplify_slice(abcd, 8, 8) is c
    assert simplify_slice(abcd, 16, 8) is b
    assert simplify_slice(abcd, 24, 8) is a
    # Test slice edges at subexpression boundaries.
    bc = simplify_slice(abcd, 8, 16)
    assert_concat(bc, ((b, 8), (c, 8)))
    # Test one slice edge at subexpression boundaries.
    assert_slice(simplify_slice(abcd, 0, 5), d, 8, 0, 5)
    assert_slice(simplify_slice(abcd, 8, 5), c, 8, 0, 5)
    assert_slice(simplify_slice(abcd, 19, 5), b, 8, 3, 5)
    assert_slice(simplify_slice(abcd, 27, 5), a, 8, 3, 5)
    # Test slice entirely inside one subexpression.
    assert_slice(simplify_slice(abcd, 10, 4), c, 8, 2, 4)
    # Test slice across subexpression boundaries.
    assert_slice(simplify_slice(abcd, 10, 9), make_concat(b, RShift(c, 2), 6), 14, 0, 9)
    # Note: Earlier code produced b[:3] ; c[2:] instead of (b ; c[2:])[:9].
    #       The complexity() function considers them equally complex,
    #       although I prefer the former in readability.
    # assertConcat(
    # simplifySlice(abcd, 10, 9),
    # ((truncate(b, 3), 3), (RShift(c, 2), 6))
    # )


def test_slice_and() -> None:
    """Tests simplification of slicing a logical AND."""
    h = TestValue("H", IntType.u(8))
    l = TestValue("L", IntType.u(8))
    hl = make_concat(h, l, 8)
    # Test whether slicing cuts off L.
    expr1 = AndOperator(hl, IntLiteral(0xBFFF))
    assert_slice(simplify_slice(expr1, 8, 6), h, 8, 0, 6)
    # Test whether redundant slicing can be eliminated.
    assert_and(simplify_slice(AndOperator(h, l), 0, 8), h, l)


def test_slice_add() -> None:
    """Tests simplification of slicing an addition."""
    h = TestValue("H", IntType.u(8))
    l = TestValue("L", IntType.u(8))
    hl = make_concat(h, l, 8)
    expr = AddOperator(hl, IntLiteral(2))
    # Simplifcation fails because index is not 0.
    up8 = simplify_slice(expr, 8, 8)
    assert_slice(up8, simplify_expression(expr), unlimited, 8, 8)
    # Successful simplification: slice lowest 8 bits.
    low8 = simplify_slice(expr, 0, 8)
    add8 = truncate(AddOperator(l, IntLiteral(2)), 8)
    assert low8 == add8
    # Successful simplification: slice lowest 6 bits.
    low6 = simplify_slice(expr, 0, 6)
    add6 = truncate(AddOperator(l, IntLiteral(2)), 6)
    assert low6 == add6
    # Simplification fails because expression becomes more complex.
    low12 = truncate(simplify_expression(expr), 12)
    low12s = simplify_expression(low12)
    assert str(low12s) == str(low12)
    assert low12s == low12


def test_slice_complement() -> None:
    """Tests simplification of slicing a complement."""
    h = TestValue("H", IntType.u(8))
    l = TestValue("L", IntType.u(8))
    hl = make_concat(h, l, 8)
    expr = Complement(hl)
    # Simplifcation fails because index is not 0.
    up8 = make_slice(expr, 8, 8)
    assert_slice(simplify_expression(up8), simplify_expression(expr), unlimited, 8, 8)
    # Successful simplification: slice lowest 8 bits.
    low8 = simplify_slice(expr, 0, 8)
    cpl8 = truncate(Complement(l), 8)
    assert str(low8) == str(cpl8)
    assert low8 == cpl8
    # Successful simplification: slice lowest 6 bits.
    low6 = simplify_slice(expr, 0, 6)
    cpl6 = truncate(Complement(l), 6)
    assert str(low6) == str(cpl6)
    assert low6 == cpl6
    # Simplification fails because expression becomes more complex.
    low12 = simplify_expression(truncate(expr, 12))
    assert_slice(low12, simplify_expression(expr), unlimited, 0, 12)


def test_slice_mixed() -> None:
    """Tests a mixture of slicing, concatenation and leading zeroes."""
    addr = TestValue("A", IntType.u(16))
    expr_int = make_slice(make_concat(IntLiteral(7), make_slice(addr, 8, 12), 12), 8, 8)
    assert_int_literal(simplify_expression(expr_int), 0x70)
    expr_u8 = make_slice(make_concat(IntLiteral(7), make_slice(addr, 8, 12), 12), 8, 8)
    assert_int_literal(simplify_expression(expr_u8), 0x70)
