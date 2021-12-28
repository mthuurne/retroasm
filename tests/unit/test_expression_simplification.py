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
from retroasm.expression_simplifier import simplifyExpression
from retroasm.types import IntType, unlimited

from .utils_expression import (
    TestValue,
    assertAnd,
    assertConcat,
    assertIntLiteral,
    assertOr,
    assertSlice,
    makeConcat,
    makeSlice,
)


def test_zero_literal() -> None:
    """Verify that the same object is returned."""
    zero = IntLiteral(0)
    assert simplifyExpression(zero) is zero


def test_zeromask_variable() -> None:
    """Verify that a value with width 0 is simplified to the literal 0."""
    zvar = TestValue("Z", IntType.u(0))
    assertIntLiteral(simplifyExpression(zvar), 0)


def test_and_literals() -> None:
    """Applies logical AND to integer literals."""
    a = IntLiteral(0xE3)
    b = IntLiteral(0x7A)
    assertIntLiteral(simplifyExpression(AndOperator(a, b)), 0x62)
    c = IntLiteral(0xFF00)
    d = IntLiteral(0x123456)
    assertIntLiteral(simplifyExpression(AndOperator(c, d)), 0x3400)


def test_and_identity() -> None:
    """Simplifies logical AND expressions containing -1."""
    addr = TestValue("A", IntType.u(16))
    ones = IntLiteral(-1)
    # Check whether identity values are filtered out.
    assert simplifyExpression(AndOperator(ones, addr)) is addr
    assert simplifyExpression(AndOperator(addr, ones)) is addr
    assert simplifyExpression(AndOperator(ones, addr, ones)) is addr
    # Check graceful handling when zero subexpressions remain.
    assertIntLiteral(simplifyExpression(AndOperator(ones, ones, ones)), -1)


def test_and_absorbtion() -> None:
    """Simplifies logical AND expressions containing 0."""
    addr = TestValue("A", IntType.u(16))
    zero = IntLiteral(0)
    assertIntLiteral(simplifyExpression(AndOperator(zero, addr)), 0)
    assertIntLiteral(simplifyExpression(AndOperator(addr, zero)), 0)
    assertIntLiteral(simplifyExpression(AndOperator(addr, zero, addr)), 0)


def test_and_idempotence() -> None:
    """Simplifies logical AND expressions containing duplicates."""
    addr = TestValue("A", IntType.u(16))
    assert simplifyExpression(AndOperator(addr, addr)) is addr
    assert simplifyExpression(AndOperator(addr, addr, addr)) is addr
    mask = TestValue("M", IntType.u(16))
    assertAnd(simplifyExpression(AndOperator(mask, addr, mask)), addr, mask)


def test_and_or() -> None:
    """Simplifies expressions containing AND and OR."""
    a = TestValue("A", IntType.u(8))
    b = TestValue("B", IntType.u(8))
    # Test literal merging.
    expr1 = OrOperator(a, IntLiteral(0x5500))
    expr2 = AndOperator(expr1, IntLiteral(0xAAFF))
    expr3 = simplifyExpression(expr2)
    assert str(expr3) == str(a)
    assert expr3 is a


def test_and_width() -> None:
    """Simplifies logical AND expressions using the subexpression widths."""
    h = TestValue("H", IntType.u(8))
    l = TestValue("L", IntType.u(8))
    hl = makeConcat(h, l, 8)
    maskLo = IntLiteral(0x00F0)
    maskHi = IntLiteral(0xF000)
    # Test whether (HL & $00F0) cuts off H.
    assertAnd(simplifyExpression(AndOperator(hl, maskLo)), maskLo, l)
    # Test whether (HL & H) cuts off H.
    assertAnd(simplifyExpression(AndOperator(hl, h)), h, l)
    # Test whether (HL & L) simplifies to L.
    assert simplifyExpression(AndOperator(hl, l)) is l
    # Test whether ($F000 & L) simplifies to 0.
    assertIntLiteral(simplifyExpression(AndOperator(maskHi, l)), 0)


def test_and_mask_to_slice() -> None:
    """Simplifies logical AND expressions that are essentially slicing."""
    h = TestValue("H", IntType.u(8))
    l = TestValue("L", IntType.u(8))
    hl = makeConcat(h, l, 8)
    # Test whether (HL & $003F) simplifies to L[0:6].
    mask6 = IntLiteral(0x003F)
    assertSlice(simplifyExpression(AndOperator(hl, mask6)), l, 8, 0, 6)
    # Test whether (HL & $00FF) simplifies to L.
    mask8 = IntLiteral(0x00FF)
    assert simplifyExpression(AndOperator(hl, mask8)) is l


def test_and_mask_concat() -> None:
    """Simplifies logical AND expressions that mask concatenated terms."""
    h = TestValue("H", IntType.u(8))
    l = TestValue("L", IntType.u(8))
    hl = makeConcat(h, l, 8)
    # Test whether (HL & $FF00) simplifies to H;$00.
    expr = simplifyExpression(AndOperator(hl, IntLiteral(0xFF00)))
    assert isinstance(expr, LShift)
    assert expr.expr is h
    assert expr.offset == 8


def test_and_mask_literal() -> None:
    """Tests elimination of redundant literals from AND expressions."""
    addr = TestValue("A", IntType.u(16))
    assertAnd(
        simplifyExpression(
            AndOperator(
                Complement(AndOperator(addr, IntLiteral(0x3FFF))), IntLiteral(0x3FF0)
            )
        ),
        Complement(addr),
        IntLiteral(0x3FF0),
    )


def test_or_literals() -> None:
    """Applies logical OR to integer literals."""
    a = IntLiteral(0x4C)
    b = IntLiteral(0x91)
    assertIntLiteral(simplifyExpression(OrOperator(a, b)), 0xDD)
    c = IntLiteral(0x00FF)
    d = IntLiteral(0x120021)
    assertIntLiteral(simplifyExpression(OrOperator(c, d)), 0x1200FF)


def test_or_identity() -> None:
    """Simplifies logical OR expressions containing 0."""
    addr = TestValue("A", IntType.u(16))
    zero = IntLiteral(0)
    # Check whether identity values are filtered out.
    assert simplifyExpression(OrOperator(zero, addr)) is addr
    assert simplifyExpression(OrOperator(addr, zero)) is addr
    assert simplifyExpression(OrOperator(zero, addr, zero)) is addr
    # Check graceful handling when zero subexpressions remain.
    assertIntLiteral(simplifyExpression(OrOperator(zero, zero, zero)), 0)


def test_or_absorbtion() -> None:
    """Simplifies logical OR expressions containing -1."""
    addr = TestValue("A", IntType.u(16))
    ones = IntLiteral(-1)
    assertIntLiteral(simplifyExpression(OrOperator(ones, addr)), -1)
    assertIntLiteral(simplifyExpression(OrOperator(addr, ones)), -1)
    assertIntLiteral(simplifyExpression(OrOperator(addr, ones, addr)), -1)


def test_or_idempotence() -> None:
    """Simplifies logical OR expressions containing duplicates."""
    addr = TestValue("A", IntType.u(16))
    assert simplifyExpression(OrOperator(addr, addr)) is addr
    assert simplifyExpression(OrOperator(addr, addr, addr)) is addr
    mask = TestValue("M", IntType.u(16))
    assertOr(simplifyExpression(OrOperator(mask, addr, mask)), addr, mask)


def test_or_and() -> None:
    """Simplifies expressions containing OR and AND."""
    x = TestValue("X", IntType.u(8))
    # (X & $55) | $AA  ==  (X | $AA) & ($55 | $AA)  ==  (X | $AA)
    mask1 = IntLiteral(0x55)
    mask2 = IntLiteral(0xAA)
    expr1 = AndOperator(x, mask1)
    expr2 = OrOperator(expr1, mask2)
    assertOr(simplifyExpression(expr2), x, mask2)


def test_or_mask_literal() -> None:
    """Tests elimination of masked OR expressions."""
    addr = TestValue("A", IntType.u(16))
    assert (
        simplifyExpression(
            AndOperator(OrOperator(LShift(addr, 8), IntLiteral(0xFFFF)), addr)
        )
        is addr
    )


def test_xor_literals() -> None:
    """Applies logical XOR to integer literals."""
    a = IntLiteral(0xDC)
    b = IntLiteral(0x58)
    assertIntLiteral(simplifyExpression(XorOperator(a, b)), 0x84)
    c = IntLiteral(0xF00F)
    d = IntLiteral(0x123456)
    assertIntLiteral(simplifyExpression(XorOperator(c, d)), 0x12C459)


def test_xor_identity() -> None:
    """Simplifies logical XOR expressions containing 0."""
    addr = TestValue("A", IntType.u(16))
    zero = IntLiteral(0)
    # Check whether identity values are filtered out.
    assert simplifyExpression(XorOperator(zero, addr)) is addr
    assert simplifyExpression(XorOperator(addr, zero)) is addr
    assert simplifyExpression(XorOperator(zero, addr, zero)) is addr
    # Check graceful handling when zero subexpressions remain.
    assertIntLiteral(simplifyExpression(XorOperator(zero, zero, zero)), 0)


def test_xor_deduplication() -> None:
    """Simplifies logical XOR expressions containing duplicates."""
    a = TestValue("A", IntType.u(8))
    b = TestValue("B", IntType.u(8))
    zero = IntLiteral(0)
    # Check that duplicate values are filtered out.
    assert simplifyExpression(XorOperator(a)) is a
    assert simplifyExpression(XorOperator(a, a)) == zero
    assert simplifyExpression(XorOperator(a, a, a)) is a
    assert simplifyExpression(XorOperator(a, a, a, a)) == zero
    # Check with different subexpressions.
    assert simplifyExpression(XorOperator(b, a, b)) is a
    assert simplifyExpression(XorOperator(a, b, b, a)) == zero


def test_xor_bitwise_complement() -> None:
    """Simplifies XOR expressions used for bitwise complement."""
    a = TestValue("A", IntType.u(8))
    compl1 = XorOperator(IntLiteral(-1), a)
    compl2 = XorOperator(IntLiteral(-1), compl1)
    assert simplifyExpression(compl2) is a


def test_add_int() -> None:
    """Adds two unlimited width integer literals."""
    arg1 = IntLiteral(3)
    arg2 = IntLiteral(20)
    expr = simplifyExpression(AddOperator(arg1, arg2))
    assertIntLiteral(expr, 23)


def test_add_fixed_width() -> None:
    """Adds two fixed width integer literals."""
    arg1 = IntLiteral(8)
    arg2 = IntLiteral(127)
    expr = simplifyExpression(AddOperator(arg1, arg2))
    assertIntLiteral(expr, 135)


def test_add_nested() -> None:
    """Adds several integers in an expression tree."""
    arg1 = AddOperator(IntLiteral(1), IntLiteral(2))
    arg2 = AddOperator(IntLiteral(3), IntLiteral(4))
    expr = simplifyExpression(AddOperator(arg1, arg2))
    assertIntLiteral(expr, 10)


def test_add_zero() -> None:
    """Test simplification of zero literal terms."""
    zero = IntLiteral(0)
    addr = TestValue("A", IntType.u(16))
    assert simplifyExpression(AddOperator(zero, addr)) is addr
    assert simplifyExpression(AddOperator(addr, zero)) is addr
    assertIntLiteral(simplifyExpression(AddOperator(zero, zero)), 0)


def test_add_associative() -> None:
    """Test simplification using the associativity of addition."""
    addr = TestValue("A", IntType.u(16))
    arg1 = AddOperator(addr, IntLiteral(1))
    arg2 = AddOperator(IntLiteral(2), IntLiteral(-3))
    assert simplifyExpression(AddOperator(arg1, arg2)) is addr


def test_add_commutative() -> None:
    """Test simplification using the commutativity of addition."""
    addr = TestValue("A", IntType.u(16))
    arg1 = AddOperator(IntLiteral(1), IntLiteral(2))
    arg2 = AddOperator(addr, IntLiteral(-3))
    assert simplifyExpression(AddOperator(arg1, arg2)) is addr


def test_complement_int() -> None:
    """Takes the complement of an integer literal."""
    expr = Complement(IntLiteral(4))
    assertIntLiteral(simplifyExpression(expr), -4)


def test_complement_twice() -> None:
    """Takes the complement of a complement."""
    addr = TestValue("A", IntType.u(16))
    assert simplifyExpression(Complement(Complement(addr))) is addr


def test_complement_subexpr() -> None:
    """Takes the complement of a simplifiable subexpression."""
    addr = TestValue("A", IntType.u(16))
    expr = simplifyExpression(
        Complement(
            makeConcat(makeConcat(IntLiteral(0xC0), IntLiteral(0xDE), 8), addr, 16)
        )
    )
    assert isinstance(expr, Complement)
    assertConcat(expr.expr, ((IntLiteral(0xC0DE), 16), (addr, 16)))


def test_negation_int() -> None:
    """Negates an integer literal."""
    assertIntLiteral(simplifyExpression(Negation(IntLiteral(-1))), 0)
    assertIntLiteral(simplifyExpression(Negation(IntLiteral(0))), 1)
    assertIntLiteral(simplifyExpression(Negation(IntLiteral(1))), 0)
    assertIntLiteral(simplifyExpression(Negation(IntLiteral(2))), 0)
    assertIntLiteral(simplifyExpression(Negation(IntLiteral(3))), 0)


def test_negation_subexpr() -> None:
    """Negates a simplifiable subexpression."""
    assertIntLiteral(
        simplifyExpression(Negation(makeConcat(IntLiteral(0x0), IntLiteral(0x00), 8))),
        1,
    )
    assertIntLiteral(
        simplifyExpression(Negation(makeConcat(IntLiteral(0xB), IntLiteral(0x00), 8))),
        0,
    )
    assertIntLiteral(
        simplifyExpression(Negation(makeConcat(IntLiteral(0x0), IntLiteral(0x07), 8))),
        0,
    )


def test_negation_or() -> None:
    """Negates an OR expression."""
    addr = TestValue("A", IntType.u(16))
    assertIntLiteral(
        simplifyExpression(Negation(OrOperator(addr, IntLiteral(0x76)))), 0
    )


def test_negation_and() -> None:
    """Negates an AND expression."""
    addr = TestValue("A", IntType.u(16))
    assertIntLiteral(
        simplifyExpression(
            Negation(AndOperator(OrOperator(IntLiteral(0x60), addr), IntLiteral(0xF0)))
        ),
        0,
    )


def test_negation_xor() -> None:
    """Negates a XOR expression."""
    addr = TestValue("A", IntType.u(16))
    assertIntLiteral(simplifyExpression(Negation(XorOperator(addr, IntLiteral(-1)))), 0)


def test_negation_add() -> None:
    """Negates an addition."""
    addr = TestValue("A", IntType.u(16))
    assertIntLiteral(simplifyExpression(Negation(AddOperator(addr, IntLiteral(1)))), 0)


def test_negation_complement() -> None:
    """Negates a complement."""
    addr = TestValue("A", IntType.u(16))
    assertIntLiteral(
        simplifyExpression(Negation(Complement(OrOperator(addr, IntLiteral(0x76))))), 0
    )


def test_negation_twice() -> None:
    """Negates a negation."""
    boolVal = TestValue("B", IntType.u(1))
    intVal = TestValue("I", IntType.u(16))
    assert simplifyExpression(Negation(Negation(boolVal))) is boolVal
    notNotInt = Negation(Negation(intVal))
    assert simplifyExpression(notNotInt) is notNotInt
    combi = AndOperator(boolVal, intVal)
    assert simplifyExpression(Negation(Negation(combi))) is combi


def test_negation_lshift() -> None:
    """Negates a left-shifted expression."""
    addr = TestValue("A", IntType.u(16))
    assertIntLiteral(
        simplifyExpression(Negation(LShift(AddOperator(addr, IntLiteral(1)), 8))), 0
    )


def test_negation_rshift() -> None:
    """Negates a right-shifted expression."""
    addr = TestValue("A", IntType.u(16))
    assertIntLiteral(
        simplifyExpression(Negation(RShift(OrOperator(addr, IntLiteral(0x345)), 8))), 0
    )


def test_sign_int() -> None:
    """Tests sign of several integer literals."""

    def check(value: int, result: int) -> None:
        assertIntLiteral(simplifyExpression(SignTest(IntLiteral(value))), result)

    check(0, 0)
    check(1, 0)
    check(-1, 1)
    check(123, 0)
    check(-123, 1)


def test_sign_types() -> None:
    """Test sign of values of signed and unsigned types."""
    u = SignTest(TestValue("U", IntType.u(8)))
    assertIntLiteral(simplifyExpression(u), 0)
    s = SignTest(TestValue("S", IntType.s(8)))
    assert simplifyExpression(s) is s


def test_sign_extended() -> None:
    """Test sign of sign extended values."""
    v = TestValue("V", IntType.int)
    assertSlice(simplifyExpression(SignTest(SignExtension(v, 8))), v, unlimited, 7, 1)
    # A zero-width value has no sign bit; sign test should return 0.
    z = TestValue("Z", IntType.u(0))
    assertIntLiteral(simplifyExpression(SignTest(SignExtension(z, 0))), 0)


def test_sign_extend_int() -> None:
    """Applies sign extension to several integer literals."""

    def check(value: int, width: int, result: int) -> None:
        assertIntLiteral(
            simplifyExpression(SignExtension(IntLiteral(value), width)), result
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
    hl = makeConcat(h, l, 8)
    assert simplifyExpression(SignExtension(hl, 8)) == SignExtension(l, 8)


def test_sign_extend_clear() -> None:
    """Removes sign extension when sign bit is known to be zero."""
    h = TestValue("H", IntType.u(8))
    l = TestValue("L", IntType.u(8))
    hgapl = makeConcat(h, l, 9)
    assert simplifyExpression(SignExtension(hgapl, 9)) is l


def test_sign_extend_set() -> None:
    """Removes sign extension when sign bit is known to be one."""
    a = TestValue("A", IntType.u(8))
    b = IntLiteral(0x80)
    combi = OrOperator(a, b)
    assert simplifyExpression(SignExtension(combi, 8)) == OrOperator(
        a, IntLiteral(~0x7F)
    )


def test_arithmetic_int() -> None:
    """Uses add/complement on several integer literals."""
    expr = AddOperator(
        AddOperator(IntLiteral(0), Complement(IntLiteral(39))),
        Complement(AddOperator(IntLiteral(101), IntLiteral(-1001))),
    )
    assertIntLiteral(simplifyExpression(expr), 861)


def test_arithmetic_associative() -> None:
    """Test simplification using the associativity of addition.
    Note that that associativity cannot be exploited unless the subtraction
    is converted into an addition first.
    """
    addr = TestValue("A", IntType.u(16))
    arg1 = AddOperator(addr, Complement(IntLiteral(1)))
    arg2 = AddOperator(IntLiteral(-2), IntLiteral(3))
    assert simplifyExpression(AddOperator(arg1, arg2)) is addr


def test_arithmetic_commutative() -> None:
    """Test simplification using the commutativity of addition.
    Note that that commutativity cannot be exploited unless the subtraction
    is converted into an addition first.
    """
    addr = TestValue("A", IntType.u(16))
    arg1 = AddOperator(IntLiteral(1), IntLiteral(2))
    arg2 = AddOperator(Complement(addr), IntLiteral(3))
    assert simplifyExpression(AddOperator(arg1, Complement(arg2))) is addr


def test_arithmetic_add_complement() -> None:
    """Test simplification of subtracting an expression from itself."""
    a = TestValue("A", IntType.u(16))
    b = TestValue("B", IntType.u(8))
    assertIntLiteral(simplifyExpression(AddOperator(a, Complement(a))), 0)
    c = AddOperator(a, b)
    d = AddOperator(b, a)
    assertIntLiteral(simplifyExpression(AddOperator(c, Complement(c))), 0)
    assertIntLiteral(simplifyExpression(AddOperator(c, Complement(d))), 0)
    e = makeConcat(a, b, 8)
    assertIntLiteral(simplifyExpression(AddOperator(e, Complement(e))), 0)


def test_arithmetic_add_truncate() -> None:
    """Test simplification of truncation of adding truncated expressions."""
    a = TestValue("A", IntType.u(16))
    expr = truncate(
        AddOperator(truncate(AddOperator(a, IntLiteral(1)), 16), IntLiteral(-1)), 16
    )
    assert str(simplifyExpression(expr)) is str(a)
    assert simplifyExpression(expr) is a


def test_arithmetic_add_truncate_literal() -> None:
    """Test simplification of truncation of added literal."""
    a = TestValue("A", IntType.u(16))
    expr = truncate(AddOperator(a, IntLiteral(0x10001)), 16)
    expected = truncate(AddOperator(a, IntLiteral(1)), 16)
    assert simplifyExpression(expr) == expected


def test_lshift_literals() -> None:
    """Shifts an integer literal to the left."""
    assertIntLiteral(simplifyExpression(LShift(IntLiteral(0x1234), 8)), 0x123400)
    assertIntLiteral(simplifyExpression(LShift(IntLiteral(0xDA), 16)), 0xDA0000)


def test_lshift_twice() -> None:
    """Shifts a value to the left twice."""
    addr = TestValue("A", IntType.u(16))
    expr = simplifyExpression(LShift(LShift(addr, 3), 5))
    assert isinstance(expr, LShift)
    assert expr.expr is addr
    assert expr.offset == 8


def test_lshift_rshift() -> None:
    """Tests left-shifting after right-shifting."""
    addr = TestValue("A", IntType.u(16))
    # Shift more to the right than to the left.
    rwin = simplifyExpression(LShift(RShift(addr, 5), 3))
    assertSlice(rwin, AndOperator(addr, IntLiteral(0xFFE0)), 16, 2, 14)
    # Shift equal amounts to the right and to the left.
    draw = simplifyExpression(LShift(RShift(addr, 4), 4))
    assertAnd(draw, addr, IntLiteral(0xFFF0))
    # Shift less to the right than to the left.
    lwin = simplifyExpression(LShift(RShift(addr, 3), 5))
    assertAnd(lwin, LShift(addr, 2), IntLiteral(0x3FFE0))


def test_lshift_truncate() -> None:
    """Tests truncation of a left-shifted expression."""
    h = TestValue("H", IntType.u(8))
    l = TestValue("L", IntType.u(8))
    hl = makeConcat(h, l, 8)
    # Shift H and L out of the truncation range.
    expr1 = simplifyExpression(truncate(LShift(hl, 8), 8))
    assertIntLiteral(expr1, 0)
    # Shift only H out of the truncation range.
    expr2 = simplifyExpression(truncate(LShift(hl, 8), 16))
    assert isinstance(expr2, LShift)
    assert expr2.expr is l
    assert expr2.offset == 8


def test_rshift_lshift() -> None:
    """Tests right-shifting after left-shifting."""
    addr = TestValue("A", IntType.u(16))
    # Shift less to the left than to the right.
    rwin = simplifyExpression(RShift(LShift(addr, 3), 5))
    assert isinstance(rwin, RShift)
    assert rwin.offset == 2
    assert rwin.expr is addr
    # Shift equal amounts to the left and to the right.
    draw = simplifyExpression(RShift(LShift(addr, 4), 4))
    assert draw is addr
    # Shift more to the left than to the right.
    lwin = simplifyExpression(RShift(LShift(addr, 5), 3))
    assert isinstance(lwin, LShift)
    assert lwin.offset == 2
    assert lwin.expr is addr


def test_lvshift_constant() -> None:
    """Shifts a constant number of positions to the left."""
    assertIntLiteral(
        simplifyExpression(LVShift(IntLiteral(0x1234), IntLiteral(8))), 0x123400
    )
    v8 = TestValue("A", IntType.u(8))
    assert simplifyExpression(LVShift(v8, IntLiteral(3))) == LShift(v8, 3)
    assert simplifyExpression(LVShift(v8, TestValue("Z", IntType.u(0)))) == v8


def test_rvshift_constant() -> None:
    """Shifts a constant number of positions to the left."""
    assertIntLiteral(
        simplifyExpression(RVShift(IntLiteral(0x1234), IntLiteral(8))), 0x12
    )
    v8 = TestValue("A", IntType.u(8))
    assert simplifyExpression(RVShift(v8, IntLiteral(3))) == RShift(v8, 3)
    assert simplifyExpression(RVShift(v8, TestValue("Z", IntType.u(0)))) == v8


def test_concat_literals() -> None:
    """Concatenates integer literals."""
    ip = IntLiteral(4)
    im = Complement(ip)
    u4 = IntLiteral(0xD)
    u8 = IntLiteral(0x29)
    cat_ip_u4 = simplifyExpression(makeConcat(ip, u4, 4))
    assertIntLiteral(cat_ip_u4, 0x40 + 0xD)
    cat_ip_u8 = simplifyExpression(makeConcat(ip, u8, 8))
    assertIntLiteral(cat_ip_u8, 0x400 + 0x29)
    cat_im_u4 = simplifyExpression(makeConcat(im, u4, 4))
    assertIntLiteral(cat_im_u4, -0x40 + 0xD)
    cat_im_u8 = simplifyExpression(makeConcat(im, u8, 8))
    assertIntLiteral(cat_im_u8, -0x400 + 0x29)
    cat_u4_u4 = simplifyExpression(makeConcat(u4, u4, 4))
    assertIntLiteral(cat_u4_u4, 0xDD)
    cat_u4_u8 = simplifyExpression(makeConcat(u4, u8, 8))
    assertIntLiteral(cat_u4_u8, 0xD29)
    cat_u8_u4 = simplifyExpression(makeConcat(u8, u4, 4))
    assertIntLiteral(cat_u8_u4, 0x29D)
    cat_u8_u8 = simplifyExpression(makeConcat(u8, u8, 8))
    assertIntLiteral(cat_u8_u8, 0x2929)


def test_concat_identity() -> None:
    """Simplifies concatenations containing identity values."""
    addr = TestValue("A", IntType.u(16))
    # Check whether empty bitstrings are filtered out.
    empty = IntLiteral(0)
    head = makeConcat(makeConcat(empty, addr, 16), addr, 16)
    assertConcat(simplifyExpression(head), ((addr, 16), (addr, 16)))
    mid = makeConcat(makeConcat(addr, empty, 0), addr, 16)
    assertConcat(simplifyExpression(mid), ((addr, 16), (addr, 16)))
    tail = makeConcat(makeConcat(addr, addr, 16), empty, 0)
    assertConcat(simplifyExpression(tail), ((addr, 16), (addr, 16)))
    many = makeConcat(
        makeConcat(
            makeConcat(
                makeConcat(
                    makeConcat(
                        makeConcat(makeConcat(empty, empty, 0), addr, 16), empty, 0
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
    assertConcat(simplifyExpression(many), ((addr, 16), (addr, 16)))
    # Check graceful handling when zero subexpressions remain.
    only = makeConcat(makeConcat(empty, empty, 0), empty, 0)
    assertIntLiteral(simplifyExpression(only), 0)
    # Check whether non-empty fixed-width zero-valued bitstrings are kept.
    zero_u8 = IntLiteral(0)
    mid_u8 = makeConcat(makeConcat(addr, zero_u8, 8), addr, 16)
    assertConcat(simplifyExpression(mid_u8), ((addr, 16), (zero_u8, 8), (addr, 16)))
    tail_u8 = makeConcat(makeConcat(addr, addr, 16), zero_u8, 8)
    assertConcat(simplifyExpression(tail_u8), ((addr, 16), (addr, 16), (zero_u8, 8)))
    # Check whether unlimited-width zero-valued bitstrings are kept.
    zero_int = IntLiteral(0)
    head_int = makeConcat(makeConcat(zero_int, addr, 16), addr, 16)
    assertConcat(
        simplifyExpression(head_int), ((zero_int, unlimited), (addr, 16), (addr, 16))
    )


def test_concat_associative() -> None:
    """Test simplification using the associativity of concatenation."""
    addr = TestValue("A", IntType.u(16))
    arg1 = makeConcat(addr, addr, 16)  # (A ; A)
    arg2 = makeConcat(arg1, arg1, 32)  # ((A ; A) ; (A ; A))
    arg3 = makeConcat(arg1, arg2, 64)  # ((A ; A) ; ((A ; A) ; (A ; A)))
    assertConcat(simplifyExpression(arg3), ((addr, 16),) * 6)


def test_concat_associative2() -> None:
    """Test simplification using the associativity of concatenation."""
    addr = TestValue("A", IntType.u(16))
    arg1 = makeConcat(addr, IntLiteral(0x9), 4)  # (A ; $9)
    arg2 = makeConcat(IntLiteral(0x63), addr, 16)  # ($63 ; A)
    arg3 = makeConcat(arg1, arg2, 24)  # ((A ; $9) ; ($63 ; A))
    assertConcat(
        simplifyExpression(arg3), ((addr, 16), (IntLiteral(0x963), 12), (addr, 16))
    )


def simplifySlice(expr: Expression, index: int, width: int) -> Expression:
    return simplifyExpression(makeSlice(expr, index, width))


def test_slice_literals() -> None:
    """Slices integer literals."""
    addr = IntLiteral(0xFD56)
    assertIntLiteral(simplifySlice(addr, 0, 16), 0xFD56)
    assertIntLiteral(simplifySlice(addr, 4, 8), 0xD5)
    assertIntLiteral(simplifySlice(addr, 8, 12), 0x0FD)
    signed = IntLiteral(-0x1995)
    assertIntLiteral(simplifySlice(signed, 0, 16), 0xE66B)
    assertIntLiteral(simplifySlice(signed, 4, 8), 0x66)
    assertIntLiteral(simplifySlice(signed, 8, 12), 0xFE6)


def test_slice_zero_width() -> None:
    """Takes a slices of width 0."""
    addr = TestValue("A", IntType.u(16))
    assertIntLiteral(simplifySlice(addr, 8, 0), 0)


def test_slice_full_range() -> None:
    """Slices a range that exactly matches a value's type."""
    addr = TestValue("A", IntType.u(16))
    assert simplifySlice(addr, 0, 16) is addr


def test_slice_out_of_range() -> None:
    """Slices a range that is fully outside a value's type."""
    addr = TestValue("A", IntType.u(16))
    assertIntLiteral(simplifySlice(addr, 16, 8), 0)


def test_slice_leading_zeroes() -> None:
    """Slices a range that is partially outside a value's type."""
    addr = TestValue("A", IntType.u(16))
    expr = simplifySlice(addr, 0, 20)  # $0xxxx
    assert expr is addr
    expr = simplifySlice(addr, 8, 12)  # $0xx
    assertSlice(expr, addr, 16, 8, 8)


def test_slice_of_slice() -> None:
    """Slices a range from another slice."""
    addr = TestValue("A", IntType.u(16))
    expr = simplifySlice(makeSlice(addr, 3, 10), 2, 6)
    assertSlice(expr, addr, 16, 5, 6)


def test_slice_concat() -> None:
    """Slices a range from a concatenation."""
    a = TestValue("A", IntType.u(8))
    b = TestValue("B", IntType.u(8))
    c = TestValue("C", IntType.u(8))
    d = TestValue("D", IntType.u(8))
    abcd = makeConcat(makeConcat(makeConcat(a, b, 8), c, 8), d, 8)
    # Test slicing out individual values.
    assert simplifySlice(abcd, 0, 8) is d
    assert simplifySlice(abcd, 8, 8) is c
    assert simplifySlice(abcd, 16, 8) is b
    assert simplifySlice(abcd, 24, 8) is a
    # Test slice edges at subexpression boundaries.
    bc = simplifySlice(abcd, 8, 16)
    assertConcat(bc, ((b, 8), (c, 8)))
    # Test one slice edge at subexpression boundaries.
    assertSlice(simplifySlice(abcd, 0, 5), d, 8, 0, 5)
    assertSlice(simplifySlice(abcd, 8, 5), c, 8, 0, 5)
    assertSlice(simplifySlice(abcd, 19, 5), b, 8, 3, 5)
    assertSlice(simplifySlice(abcd, 27, 5), a, 8, 3, 5)
    # Test slice entirely inside one subexpression.
    assertSlice(simplifySlice(abcd, 10, 4), c, 8, 2, 4)
    # Test slice across subexpression boundaries.
    assertSlice(simplifySlice(abcd, 10, 9), makeConcat(b, RShift(c, 2), 6), 14, 0, 9)
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
    hl = makeConcat(h, l, 8)
    # Test whether slicing cuts off L.
    expr1 = AndOperator(hl, IntLiteral(0xBFFF))
    assertSlice(simplifySlice(expr1, 8, 6), h, 8, 0, 6)
    # Test whether redundant slicing can be eliminated.
    assertAnd(simplifySlice(AndOperator(h, l), 0, 8), h, l)


def test_slice_add() -> None:
    """Tests simplification of slicing an addition."""
    h = TestValue("H", IntType.u(8))
    l = TestValue("L", IntType.u(8))
    hl = makeConcat(h, l, 8)
    expr = AddOperator(hl, IntLiteral(2))
    # Simplifcation fails because index is not 0.
    up8 = simplifySlice(expr, 8, 8)
    assertSlice(up8, simplifyExpression(expr), unlimited, 8, 8)
    # Successful simplification: slice lowest 8 bits.
    low8 = simplifySlice(expr, 0, 8)
    add8 = truncate(AddOperator(l, IntLiteral(2)), 8)
    assert low8 == add8
    # Successful simplification: slice lowest 6 bits.
    low6 = simplifySlice(expr, 0, 6)
    add6 = truncate(AddOperator(l, IntLiteral(2)), 6)
    assert low6 == add6
    # Simplification fails because expression becomes more complex.
    low12 = truncate(simplifyExpression(expr), 12)
    low12s = simplifyExpression(low12)
    assert str(low12s) == str(low12)
    assert low12s == low12


def test_slice_complement() -> None:
    """Tests simplification of slicing a complement."""
    h = TestValue("H", IntType.u(8))
    l = TestValue("L", IntType.u(8))
    hl = makeConcat(h, l, 8)
    expr = Complement(hl)
    # Simplifcation fails because index is not 0.
    up8 = makeSlice(expr, 8, 8)
    assertSlice(simplifyExpression(up8), simplifyExpression(expr), unlimited, 8, 8)
    # Successful simplification: slice lowest 8 bits.
    low8 = simplifySlice(expr, 0, 8)
    cpl8 = truncate(Complement(l), 8)
    assert str(low8) == str(cpl8)
    assert low8 == cpl8
    # Successful simplification: slice lowest 6 bits.
    low6 = simplifySlice(expr, 0, 6)
    cpl6 = truncate(Complement(l), 6)
    assert str(low6) == str(cpl6)
    assert low6 == cpl6
    # Simplification fails because expression becomes more complex.
    low12 = simplifyExpression(truncate(expr, 12))
    assertSlice(low12, simplifyExpression(expr), unlimited, 0, 12)


def test_slice_mixed() -> None:
    """Tests a mixture of slicing, concatenation and leading zeroes."""
    addr = TestValue("A", IntType.u(16))
    expr_int = makeSlice(makeConcat(IntLiteral(7), makeSlice(addr, 8, 12), 12), 8, 8)
    assertIntLiteral(simplifyExpression(expr_int), 0x70)
    expr_u8 = makeSlice(makeConcat(IntLiteral(7), makeSlice(addr, 8, 12), 12), 8, 8)
    assertIntLiteral(simplifyExpression(expr_u8), 0x70)
