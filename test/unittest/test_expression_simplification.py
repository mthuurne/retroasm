from utils_expression import TestValue

from retroasm.expression import (
    AddOperator, AndOperator, Complement, IntLiteral, LShift, OrOperator,
    RShift, Truncation, XorOperator
    )
from retroasm.expression_simplifier import simplifyExpression
from retroasm.types import IntType, unlimited

import unittest

class TestUtils(unittest.TestCase):

    def assertIntLiteral(self, expr, value):
        '''Asserts that the given expression is an unlimited-width int literal
        with the given value.
        '''
        comparison = IntLiteral.create(value)
        self.assertIsInstance(expr, IntLiteral)
        self.assertIsInstance(expr.type, IntType)
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
        trunc = Truncation(shift, width) if needsTrunc else shift
        self.assertEqual(str(expr), str(trunc))
        self.assertEqual(expr, trunc)
        self.assertIsInstance(expr, type(trunc))
        self.assertIsInstance(expr.type, IntType)
        if needsShift:
            shiftExpr = expr.expr if needsTrunc else expr
            self.assertEqual(str(shiftExpr), str(shift))
            self.assertEqual(shiftExpr, shift)
            self.assertIsInstance(shiftExpr, RShift)
            self.assertIsInstance(shiftExpr.type, IntType)
            self.assertEqual(shiftExpr.offset, index)
            self.assertEqual(shiftExpr.expr, subExpr)
        else:
            self.assertEqual(expr.expr, subExpr)

def makeSlice(expr, index, width):
    return Truncation(RShift(expr, index), width)

def makeConcat(exprH, exprL, widthL):
    return OrOperator(exprL, LShift(exprH, widthL))

class AndTests(TestUtils):

    def test_literals(self):
        '''Applies logical AND to integer literals.'''
        a = IntLiteral(0xE3, IntType(8))
        b = IntLiteral(0x7A, IntType(8))
        self.assertIntLiteral(simplifyExpression(AndOperator(a, b)), 0x62)
        c = IntLiteral(0xFF00, IntType(16))
        d = IntLiteral.create(0x123456)
        self.assertIntLiteral(simplifyExpression(AndOperator(c, d)), 0x3400)

    def test_identity(self):
        '''Simplifies logical AND expressions containing -1.'''
        addr = TestValue('A', IntType(16))
        ones = IntLiteral.create(-1)
        # Check whether identity values are filtered out.
        self.assertIs(simplifyExpression(AndOperator(ones, addr)), addr)
        self.assertIs(simplifyExpression(AndOperator(addr, ones)), addr)
        self.assertIs(simplifyExpression(AndOperator(ones, addr, ones)), addr)
        # Check graceful handling when zero subexpressions remain.
        self.assertIntLiteral(
            simplifyExpression(AndOperator(ones, ones, ones)), -1
            )

    def test_absorbtion(self):
        '''Simplifies logical AND expressions containing 0.'''
        addr = TestValue('A', IntType(16))
        zero = IntLiteral.create(0)
        self.assertIntLiteral(simplifyExpression(AndOperator(zero, addr)), 0)
        self.assertIntLiteral(simplifyExpression(AndOperator(addr, zero)), 0)
        self.assertIntLiteral(
            simplifyExpression(AndOperator(addr, zero, addr)), 0
            )

    def test_idempotence(self):
        '''Simplifies logical AND expressions containing duplicates.'''
        addr = TestValue('A', IntType(16))
        self.assertIs(simplifyExpression(AndOperator(addr, addr)), addr)
        self.assertIs(simplifyExpression(AndOperator(addr, addr, addr)), addr)
        mask = TestValue('M', IntType(16))
        self.assertAnd(
            simplifyExpression(AndOperator(mask, addr, mask)), addr, mask
            )

    def test_or(self):
        '''Simplifies expressions containing AND and OR.'''
        a = TestValue('A', IntType(8))
        b = TestValue('B', IntType(8))
        # Test literal merging.
        expr1 = OrOperator(a, IntLiteral.create(0x5500))
        expr2 = AndOperator(expr1, IntLiteral.create(0xAAFF))
        expr3 = simplifyExpression(expr2)
        self.assertEqual(str(expr3), str(a))
        self.assertIs(expr3, a)

    def test_width(self):
        '''Simplifies logical AND expressions using the subexpression widths.'''
        h = TestValue('H', IntType(8))
        l = TestValue('L', IntType(8))
        hl = makeConcat(h, l, 8)
        maskLo = IntLiteral(0x00F0, IntType(16))
        maskHi = IntLiteral(0xF000, IntType(16))
        # Test whether (HL & $00F0) cuts off H.
        self.assertAnd(simplifyExpression(AndOperator(hl, maskLo)), maskLo, l)
        # Test whether (HL & H) cuts off H.
        self.assertAnd(simplifyExpression(AndOperator(hl, h)), h, l)
        # Test whether (HL & L) simplifies to L.
        self.assertIs(simplifyExpression(AndOperator(hl, l)), l)
        # Test whether ($F000 & L) simplifies to 0.
        self.assertIntLiteral(simplifyExpression(AndOperator(maskHi, l)), 0)

    def test_mask_to_slice(self):
        '''Simplifies logical AND expressions that are essentially slicing.'''
        h = TestValue('H', IntType(8))
        l = TestValue('L', IntType(8))
        hl = makeConcat(h, l, 8)
        # Test whether (HL & $003F) simplifies to L[0:6].
        mask6 = IntLiteral(0x003F, IntType(16))
        self.assertSlice(simplifyExpression(AndOperator(hl, mask6)), l, 8, 0, 6)
        # Test whether (HL & $00FF) simplifies to L.
        mask8 = IntLiteral(0x00FF, IntType(16))
        self.assertIs(simplifyExpression(AndOperator(hl, mask8)), l)

    def test_mask_concat(self):
        '''Simplifies logical AND expressions that are essentially slicing.'''
        h = TestValue('H', IntType(8))
        l = TestValue('L', IntType(8))
        hl = makeConcat(h, l, 8)
        # Test whether (HL & $FF00) simplifies to H;$00.
        expr = simplifyExpression(
            AndOperator(hl, IntLiteral(0xFF00, IntType(16)))
            )
        self.assertIsInstance(expr, LShift)
        self.assertIs(expr.expr, h)
        self.assertEqual(expr.offset, 8)


class OrTests(TestUtils):

    def test_literals(self):
        '''Applies logical OR to integer literals.'''
        a = IntLiteral(0x4C, IntType(8))
        b = IntLiteral(0x91, IntType(8))
        self.assertIntLiteral(simplifyExpression(OrOperator(a, b)), 0xDD)
        c = IntLiteral(0x00FF, IntType(16))
        d = IntLiteral.create(0x120021)
        self.assertIntLiteral(simplifyExpression(OrOperator(c, d)), 0x1200FF)

    def test_identity(self):
        '''Simplifies logical OR expressions containing 0.'''
        addr = TestValue('A', IntType(16))
        zero = IntLiteral.create(0)
        # Check whether identity values are filtered out.
        self.assertIs(simplifyExpression(OrOperator(zero, addr)), addr)
        self.assertIs(simplifyExpression(OrOperator(addr, zero)), addr)
        self.assertIs(simplifyExpression(OrOperator(zero, addr, zero)), addr)
        # Check graceful handling when zero subexpressions remain.
        self.assertIntLiteral(
            simplifyExpression(OrOperator(zero, zero, zero)), 0
            )

    def test_absorbtion(self):
        '''Simplifies logical OR expressions containing -1.'''
        addr = TestValue('A', IntType(16))
        ones = IntLiteral.create(-1)
        self.assertIntLiteral(simplifyExpression(OrOperator(ones, addr)), -1)
        self.assertIntLiteral(simplifyExpression(OrOperator(addr, ones)), -1)
        self.assertIntLiteral(
            simplifyExpression(OrOperator(addr, ones, addr)), -1
            )

    def test_idempotence(self):
        '''Simplifies logical OR expressions containing duplicates.'''
        addr = TestValue('A', IntType(16))
        self.assertIs(simplifyExpression(OrOperator(addr, addr)), addr)
        self.assertIs(simplifyExpression(OrOperator(addr, addr, addr)), addr)
        mask = TestValue('M', IntType(16))
        self.assertOr(
            simplifyExpression(OrOperator(mask, addr, mask)), addr, mask
            )

    def test_and(self):
        '''Simplifies expressions containing OR and AND.'''
        x = TestValue('X', IntType(8))
        # (X & $55) | $AA  ==  (X | $AA) & ($55 | $AA)  ==  (X | $AA)
        mask1 = IntLiteral.create(0x55)
        mask2 = IntLiteral.create(0xAA)
        expr1 = AndOperator(x, mask1)
        expr2 = OrOperator(expr1, mask2)
        self.assertOr(simplifyExpression(expr2), x, mask2)

class XorTests(TestUtils):

    def test_literals(self):
        '''Applies logical XOR to integer literals.'''
        a = IntLiteral(0xDC, IntType(8))
        b = IntLiteral(0x58, IntType(8))
        self.assertIntLiteral(simplifyExpression(XorOperator(a, b)), 0x84)
        c = IntLiteral(0xF00F, IntType(16))
        d = IntLiteral.create(0x123456)
        self.assertIntLiteral(simplifyExpression(XorOperator(c, d)), 0x12C459)

    def test_identity(self):
        '''Simplifies logical XOR expressions containing 0.'''
        addr = TestValue('A', IntType(16))
        zero = IntLiteral.create(0)
        # Check whether identity values are filtered out.
        self.assertIs(simplifyExpression(XorOperator(zero, addr)), addr)
        self.assertIs(simplifyExpression(XorOperator(addr, zero)), addr)
        self.assertIs(simplifyExpression(XorOperator(zero, addr, zero)), addr)
        # Check graceful handling when zero subexpressions remain.
        self.assertIntLiteral(
            simplifyExpression(XorOperator(zero, zero, zero)), 0
            )

    def test_deduplication(self):
        '''Simplifies logical XOR expressions containing duplicates.'''
        a = TestValue('A', IntType(8))
        b = TestValue('B', IntType(8))
        zero = IntLiteral.create(0)
        # Check that duplicate values are filtered out.
        self.assertIs(simplifyExpression(XorOperator(a)), a)
        self.assertEqual(simplifyExpression(XorOperator(a, a)), zero)
        self.assertIs(simplifyExpression(XorOperator(a, a, a)), a)
        self.assertEqual(simplifyExpression(XorOperator(a, a, a, a)), zero)
        # Check with different subexpressions.
        self.assertIs(simplifyExpression(XorOperator(b, a, b)), a)
        self.assertEqual(simplifyExpression(XorOperator(a, b, b, a)), zero)

class AddTests(TestUtils):

    def test_int(self):
        '''Adds two unlimited width integer literals.'''
        arg1 = IntLiteral.create(3)
        arg2 = IntLiteral.create(20)
        expr = simplifyExpression(AddOperator(arg1, arg2))
        self.assertIntLiteral(expr, 23)

    def test_fixed_width(self):
        '''Adds two fixed width integer literals.'''
        arg1 = IntLiteral(8, IntType(4))
        arg2 = IntLiteral(127, IntType(8))
        expr = simplifyExpression(AddOperator(arg1, arg2))
        self.assertIntLiteral(expr, 135)

    def test_nested(self):
        '''Adds several integers in an expression tree.'''
        arg1 = AddOperator(IntLiteral.create(1), IntLiteral.create(2))
        arg2 = AddOperator(IntLiteral.create(3), IntLiteral.create(4))
        expr = simplifyExpression(AddOperator(arg1, arg2))
        self.assertIntLiteral(expr, 10)

    def test_zero(self):
        '''Test simplification of zero literal terms.'''
        zero = IntLiteral.create(0)
        addr = TestValue('A', IntType(16))
        self.assertIs(simplifyExpression(AddOperator(zero, addr)), addr)
        self.assertIs(simplifyExpression(AddOperator(addr, zero)), addr)
        self.assertIntLiteral(simplifyExpression(AddOperator(zero, zero)), 0)

    def test_associative(self):
        '''Test simplification using the associativity of addition.'''
        addr = TestValue('A', IntType(16))
        arg1 = AddOperator(addr, IntLiteral.create(1))
        arg2 = AddOperator(IntLiteral.create(2), IntLiteral.create(-3))
        self.assertIs(simplifyExpression(AddOperator(arg1, arg2)), addr)

    def test_commutative(self):
        '''Test simplification using the commutativity of addition.'''
        addr = TestValue('A', IntType(16))
        arg1 = AddOperator(IntLiteral.create(1), IntLiteral.create(2))
        arg2 = AddOperator(addr, IntLiteral.create(-3))
        self.assertIs(simplifyExpression(AddOperator(arg1, arg2)), addr)

class ComplementTests(TestUtils):

    def test_int(self):
        '''Takes the complement of an integer literal.'''
        expr = Complement(IntLiteral.create(4))
        self.assertIntLiteral(simplifyExpression(expr), -4)

    def test_twice(self):
        '''Takes the complement of a complement.'''
        addr = TestValue('A', IntType(16))
        self.assertIs(simplifyExpression(Complement(Complement(addr))), addr)

    def test_subexpr(self):
        '''Takes the complement of a simplifiable subexpression.'''
        addr = TestValue('A', IntType(16))
        expr = simplifyExpression(Complement(
            makeConcat(
                makeConcat(
                    IntLiteral(0xC0, IntType(8)),
                    IntLiteral(0xDE, IntType(8)),
                    8
                    ),
                addr, 16
                )
            ))
        self.assertIsInstance(expr, Complement)
        self.assertConcat(expr.expr, (
            (IntLiteral(0xC0DE, IntType(16)), 16),
            (addr, 16)
            ))

class ArithmeticTests(TestUtils):

    def test_int(self):
        '''Uses add/complement on several integer literals.'''
        expr = AddOperator(
            AddOperator(IntLiteral.create(0), Complement(IntLiteral.create(39))),
            Complement(
                AddOperator(IntLiteral.create(101), IntLiteral.create(-1001))
                )
            )
        self.assertIntLiteral(simplifyExpression(expr), 861)

    def test_associative(self):
        '''Test simplification using the associativity of addition.
        Note that that associativity cannot be exploited unless the subtraction
        is converted into an addition first.
        '''
        addr = TestValue('A', IntType(16))
        arg1 = AddOperator(addr, Complement(IntLiteral.create(1)))
        arg2 = AddOperator(IntLiteral.create(-2), IntLiteral.create(3))
        self.assertIs(simplifyExpression(AddOperator(arg1, arg2)), addr)

    def test_commutative(self):
        '''Test simplification using the commutativity of addition.
        Note that that commutativity cannot be exploited unless the subtraction
        is converted into an addition first.
        '''
        addr = TestValue('A', IntType(16))
        arg1 = AddOperator(IntLiteral.create(1), IntLiteral.create(2))
        arg2 = AddOperator(Complement(addr), IntLiteral.create(3))
        self.assertIs(
            simplifyExpression(AddOperator(arg1, Complement(arg2))), addr
            )

    def test_add_complement(self):
        '''Test simplification of subtracting an expression from itself.'''
        a = TestValue('A', IntType(16))
        b = TestValue('B', IntType(8))
        self.assertIntLiteral(
            simplifyExpression(AddOperator(a, Complement(a))), 0
            )
        c = AddOperator(a, b)
        d = AddOperator(b, a)
        self.assertIntLiteral(
            simplifyExpression(AddOperator(c, Complement(c))), 0
            )
        self.assertIntLiteral(
            simplifyExpression(AddOperator(c, Complement(d))), 0
            )
        e = makeConcat(a, b, 8)
        self.assertIntLiteral(
            simplifyExpression(AddOperator(e, Complement(e))), 0
            )

    def test_add_truncate(self):
        '''Test simplification of truncation of adding truncated expressions.'''
        a = TestValue('A', IntType(16))
        expr = Truncation(
            AddOperator(
                Truncation(AddOperator(a, IntLiteral.create(1)), 16),
                IntLiteral.create(-1)
                ),
            16
            )
        self.assertIs(simplifyExpression(expr), a)

    def test_add_truncate_literal(self):
        '''Test simplification of truncation of added literal.'''
        a = TestValue('A', IntType(16))
        expr = Truncation(AddOperator(a, IntLiteral.create(0x10001)), 16)
        expected = Truncation(AddOperator(a, IntLiteral.create(1)), 16)
        self.assertEqual(simplifyExpression(expr), expected)

class LShiftTests(TestUtils):

    def test_literals(self):
        '''Shifts an integer literal to the left.'''
        self.assertIntLiteral(
            simplifyExpression(LShift(IntLiteral.create(0x1234), 8)),
            0x123400
            )
        self.assertIntLiteral(
            simplifyExpression(LShift(IntLiteral(0xDA, IntType(8)), 16)),
            0xDA0000
            )

    def test_twice(self):
        '''Shifts a value to the left twice.'''
        addr = TestValue('A', IntType(16))
        expr = simplifyExpression(LShift(LShift(addr, 3), 5))
        self.assertIsInstance(expr, LShift)
        self.assertIs(expr.expr, addr)
        self.assertEqual(expr.offset, 8)

    def test_rshift(self):
        '''Tests left-shifting after right-shifting.'''
        addr = TestValue('A', IntType(16))
        # Shift more to the right than to the left.
        rwin = simplifyExpression(LShift(RShift(addr, 5), 3))
        self.assertAnd(rwin, RShift(addr, 2), IntLiteral.create(0x3FF8))
        # Shift equal amounts to the right and to the left.
        draw = simplifyExpression(LShift(RShift(addr, 4), 4))
        self.assertAnd(draw, addr, IntLiteral.create(0xFFF0))
        # Shift less to the right than to the left.
        lwin = simplifyExpression(LShift(RShift(addr, 3), 5))
        self.assertAnd(lwin, LShift(addr, 2), IntLiteral.create(0x3FFE0))

    def test_truncate(self):
        '''Tests truncation of a left-shifted expression.'''
        h = TestValue('H', IntType(8))
        l = TestValue('L', IntType(8))
        hl = makeConcat(h, l, 8)
        # Shift H and L out of the truncation range.
        expr1 = simplifyExpression(Truncation(LShift(hl, 8), 8))
        self.assertIntLiteral(expr1, 0)
        # Shift only H out of the truncation range.
        expr2 = simplifyExpression(Truncation(LShift(hl, 8), 16))
        self.assertIsInstance(expr2, LShift)
        self.assertIs(expr2.expr, l)
        self.assertEqual(expr2.offset, 8)

class RShiftTests(TestUtils):

    def test_lshift(self):
        '''Tests right-shifting after left-shifting.'''
        addr = TestValue('A', IntType(16))
        # Shift less to the left than to the right.
        rwin = simplifyExpression(RShift(LShift(addr, 3), 5))
        self.assertIsInstance(rwin, RShift)
        self.assertEqual(rwin.offset, 2)
        self.assertIs(rwin.expr, addr)
        # Shift equal amounts to the left and to the right.
        draw = simplifyExpression(RShift(LShift(addr, 4), 4))
        self.assertIs(draw, addr)
        # Shift more to the left than to the right.
        lwin = simplifyExpression(RShift(LShift(addr, 5), 3))
        self.assertIsInstance(lwin, LShift)
        self.assertEqual(lwin.offset, 2)
        self.assertIs(lwin.expr, addr)

class ConcatTests(TestUtils):

    def test_literals(self):
        '''Concatenates integer literals.'''
        ip = IntLiteral.create(4)
        im = Complement(ip)
        u4 = IntLiteral(0xD, IntType(4))
        u8 = IntLiteral(0x29, IntType(8))
        cat_ip_u4 = simplifyExpression(makeConcat(ip, u4, 4))
        self.assertIntLiteral(cat_ip_u4, 0x40 + 0xD)
        cat_ip_u8 = simplifyExpression(makeConcat(ip, u8, 8))
        self.assertIntLiteral(cat_ip_u8, 0x400 + 0x29)
        cat_im_u4 = simplifyExpression(makeConcat(im, u4, 4))
        self.assertIntLiteral(cat_im_u4, -0x40 + 0xD)
        cat_im_u8 = simplifyExpression(makeConcat(im, u8, 8))
        self.assertIntLiteral(cat_im_u8, -0x400 + 0x29)
        cat_u4_u4 = simplifyExpression(makeConcat(u4, u4, 4))
        self.assertIntLiteral(cat_u4_u4, 0xDD)
        cat_u4_u8 = simplifyExpression(makeConcat(u4, u8, 8))
        self.assertIntLiteral(cat_u4_u8, 0xD29)
        cat_u8_u4 = simplifyExpression(makeConcat(u8, u4, 4))
        self.assertIntLiteral(cat_u8_u4, 0x29D)
        cat_u8_u8 = simplifyExpression(makeConcat(u8, u8, 8))
        self.assertIntLiteral(cat_u8_u8, 0x2929)

    def test_identity(self):
        '''Simplifies concatenations containing identity values.'''
        addr = TestValue('A', IntType(16))
        # Check whether empty bitstrings are filtered out.
        empty = IntLiteral(0, IntType(0))
        head = makeConcat(makeConcat(empty, addr, 16), addr, 16)
        self.assertConcat(simplifyExpression(head), ((addr, 16), (addr, 16)))
        mid = makeConcat(makeConcat(addr, empty, 0), addr, 16)
        self.assertConcat(simplifyExpression(mid), ((addr, 16), (addr, 16)))
        tail = makeConcat(makeConcat(addr, addr, 16), empty, 0)
        self.assertConcat(simplifyExpression(tail), ((addr, 16), (addr, 16)))
        many = makeConcat(
            makeConcat(
                makeConcat(
                    makeConcat(
                        makeConcat(
                            makeConcat(
                                makeConcat(empty, empty, 0),
                                addr, 16
                                ),
                            empty, 0
                            ),
                        empty, 0
                        ),
                    addr, 16
                    ),
                empty, 0
                ),
            empty, 0
            )
        self.assertConcat(simplifyExpression(many), ((addr, 16), (addr, 16)))
        # Check graceful handling when zero subexpressions remain.
        only = makeConcat(makeConcat(empty, empty, 0), empty, 0)
        self.assertIntLiteral(simplifyExpression(only), 0)
        # Check whether non-empty fixed-width zero-valued bitstrings are kept.
        zero_u8 = IntLiteral(0, IntType(8))
        mid_u8 = makeConcat(makeConcat(addr, zero_u8, 8), addr, 16)
        self.assertConcat(simplifyExpression(mid_u8),
            ((addr, 16), (zero_u8, 8), (addr, 16))
            )
        tail_u8 = makeConcat(makeConcat(addr, addr, 16), zero_u8, 8)
        self.assertConcat(simplifyExpression(tail_u8),
            ((addr, 16), (addr, 16), (zero_u8, 8))
            )
        # Check whether unlimited-width zero-valued bitstrings are kept.
        zero_int = IntLiteral.create(0)
        head_int = makeConcat(makeConcat(zero_int, addr, 16), addr, 16)
        self.assertConcat(simplifyExpression(head_int),
            ((zero_int, unlimited), (addr, 16), (addr, 16))
            )

    def test_associative(self):
        '''Test simplification using the associativity of concatenation.
        '''
        addr = TestValue('A', IntType(16))
        arg1 = makeConcat(addr, addr, 16) # (A ; A)
        arg2 = makeConcat(arg1, arg1, 32) # ((A ; A) ; (A ; A))
        arg3 = makeConcat(arg1, arg2, 64) # ((A ; A) ; ((A ; A) ; (A ; A)))
        self.assertConcat(simplifyExpression(arg3), ((addr, 16), ) * 6)

    def test_associative2(self):
        '''Test simplification using the associativity of concatenation.
        '''
        addr = TestValue('A', IntType(16))
        arg1 = makeConcat(addr, IntLiteral(0x9, IntType(4)), 4) # (A ; $9)
        arg2 = makeConcat(IntLiteral(0x63, IntType(8)), addr, 16) # ($63 ; A)
        arg3 = makeConcat(arg1, arg2, 24) # ((A ; $9) ; ($63 ; A))
        self.assertConcat(
            simplifyExpression(arg3),
            ((addr, 16), (IntLiteral(0x963, IntType(12)), 12), (addr, 16))
            )

def simplifySlice(expr, index, width):
    return simplifyExpression(makeSlice(expr, index, width))

class SliceTests(TestUtils):

    def test_literals(self):
        '''Slices integer literals.'''
        addr = IntLiteral(0xFD56, IntType(16))
        self.assertIntLiteral(simplifySlice(addr, 0, 16), 0xFD56)
        self.assertIntLiteral(simplifySlice(addr, 4, 8), 0xD5)
        self.assertIntLiteral(simplifySlice(addr, 8, 12), 0x0FD)
        signed = IntLiteral.create(-0x1995)
        self.assertIntLiteral(simplifySlice(signed, 0, 16), 0xE66B)
        self.assertIntLiteral(simplifySlice(signed, 4, 8), 0x66)
        self.assertIntLiteral(simplifySlice(signed, 8, 12), 0xFE6)

    def test_zero_width(self):
        '''Takes a slices of width 0.'''
        addr = TestValue('A', IntType(16))
        self.assertIntLiteral(simplifySlice(addr, 8, 0), 0)

    def test_full_range(self):
        '''Slices a range that exactly matches a value's type.'''
        addr = TestValue('A', IntType(16))
        self.assertIs(simplifySlice(addr, 0, 16), addr)

    def test_out_of_range(self):
        '''Slices a range that is fully outside a value's type.'''
        addr = TestValue('A', IntType(16))
        self.assertIntLiteral(simplifySlice(addr, 16, 8), 0)

    def test_leading_zeroes(self):
        '''Slices a range that is partially outside a value's type.'''
        addr = TestValue('A', IntType(16))
        expr = simplifySlice(addr, 0, 20) # $0xxxx
        self.assertIs(expr, addr)
        expr = simplifySlice(addr, 8, 12) # $0xx
        self.assertSlice(expr, addr, 16, 8, 8)

    def test_double_slice(self):
        '''Slices a range from another slice.'''
        addr = TestValue('A', IntType(16))
        expr = simplifySlice(makeSlice(addr, 3, 10), 2, 6)
        self.assertSlice(expr, addr, 16, 5, 6)

    def test_concat(self):
        '''Slices a range from a concatenation.'''
        a = TestValue('A', IntType(8))
        b = TestValue('B', IntType(8))
        c = TestValue('C', IntType(8))
        d = TestValue('D', IntType(8))
        abcd = makeConcat(makeConcat(makeConcat(a, b, 8), c, 8), d, 8)
        # Test slicing out individual values.
        self.assertIs(simplifySlice(abcd, 0, 8), d)
        self.assertIs(simplifySlice(abcd, 8, 8), c)
        self.assertIs(simplifySlice(abcd, 16, 8), b)
        self.assertIs(simplifySlice(abcd, 24, 8), a)
        # Test slice edges at subexpression boundaries.
        bc = simplifySlice(abcd, 8, 16)
        self.assertConcat(bc, ((b, 8), (c, 8)))
        # Test one slice edge at subexpression boundaries.
        self.assertSlice(simplifySlice(abcd, 0, 5), d, 8, 0, 5)
        self.assertSlice(simplifySlice(abcd, 8, 5), c, 8, 0, 5)
        self.assertSlice(simplifySlice(abcd, 19, 5), b, 8, 3, 5)
        self.assertSlice(simplifySlice(abcd, 27, 5), a, 8, 3, 5)
        # Test slice entirely inside one subexpression.
        self.assertSlice(simplifySlice(abcd, 10, 4), c, 8, 2, 4)
        # Test slice across subexpression boundaries.
        self.assertConcat(
            simplifySlice(abcd, 10, 9),
            ((Truncation(b, 3), 3), (RShift(c, 2), 6))
            )

    def test_and(self):
        '''Tests simplification of slicing a logical AND.'''
        h = TestValue('H', IntType(8))
        l = TestValue('L', IntType(8))
        hl = makeConcat(h, l, 8)
        # Test whether slicing cuts off L.
        expr1 = AndOperator(hl, IntLiteral.create(0xBFFF))
        self.assertSlice(simplifySlice(expr1, 8, 6), h, 8, 0, 6)
        # Test whether redundant slicing can be eliminated.
        self.assertAnd(simplifySlice(AndOperator(h, l), 0, 8), h, l)

    def test_add(self):
        '''Tests simplification of slicing an addition.'''
        h = TestValue('H', IntType(8))
        l = TestValue('L', IntType(8))
        hl = makeConcat(h, l, 8)
        expr = AddOperator(hl, IntLiteral.create(2))
        # Simplifcation fails because index is not 0.
        up8 = simplifySlice(expr, 8, 8)
        self.assertSlice(up8, simplifyExpression(expr), unlimited, 8, 8)
        # Successful simplification: slice lowest 8 bits.
        low8 = simplifySlice(expr, 0, 8)
        add8 = Truncation(AddOperator(l, IntLiteral.create(2)), 8)
        self.assertEqual(low8, add8)
        # Successful simplification: slice lowest 6 bits.
        low6 = simplifySlice(expr, 0, 6)
        add6 = Truncation(
            AddOperator(Truncation(l, 6), IntLiteral.create(2)), 6
            )
        self.assertEqual(low6, add6)
        # Simplification fails because expression becomes more complex.
        low12 = Truncation(simplifyExpression(expr), 12)
        low12s = simplifyExpression(low12)
        self.assertEqual(str(low12s), str(low12))
        self.assertEqual(low12s, low12)

    def test_complement(self):
        '''Tests simplification of slicing a complement.'''
        h = TestValue('H', IntType(8))
        l = TestValue('L', IntType(8))
        hl = makeConcat(h, l, 8)
        expr = Complement(hl)
        # Simplifcation fails because index is not 0.
        up8 = makeSlice(expr, 8, 8)
        self.assertSlice(
            simplifyExpression(up8), simplifyExpression(expr), unlimited, 8, 8
            )
        # Successful simplification: slice lowest 8 bits.
        low8 = simplifySlice(expr, 0, 8)
        cpl8 = Truncation(Complement(l), 8)
        self.assertEqual(str(low8), str(cpl8))
        self.assertEqual(low8, cpl8)
        # Successful simplification: slice lowest 6 bits.
        low6 = simplifySlice(expr, 0, 6)
        cpl6 = Truncation(Complement(Truncation(l, 6)), 6)
        self.assertEqual(str(low6), str(cpl6))
        self.assertEqual(low6, cpl6)
        # Simplification fails because expression becomes more complex.
        low12 = simplifyExpression(Truncation(expr, 12))
        self.assertSlice(low12, simplifyExpression(expr), unlimited, 0, 12)

    def test_mixed(self):
        '''Tests a mixture of slicing, concatenation and leading zeroes.'''
        addr = TestValue('A', IntType(16))
        expr_int = makeSlice(
            makeConcat(IntLiteral.create(7), makeSlice(addr, 8, 12), 12),
            8, 8
            )
        self.assertIntLiteral(simplifyExpression(expr_int), 0x70)
        expr_u8 = makeSlice(
            makeConcat(IntLiteral(7, IntType(4)), makeSlice(addr, 8, 12), 12),
            8, 8
            )
        self.assertIntLiteral(simplifyExpression(expr_u8), 0x70)

if __name__ == '__main__':
    unittest.main()
