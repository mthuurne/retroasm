from retroasm.expression import (
    AddOperator, AndOperator, Complement, IntLiteral, IntType,
    LShift, LocalValue, OrOperator, RShift, Truncation,
    createConcatenation, createSlice, createSubtraction
    )

import unittest

class TestUtils(unittest.TestCase):

    def assertIntLiteral(self, expr, value):
        '''Asserts that the given expression is an unlimited-width int literal
        with the given value.
        '''
        comparison = IntLiteral.create(value)
        self.assertIs(type(expr), IntLiteral)
        self.assertIs(type(expr.type), IntType)
        self.assertEqual(expr.value, value)

    def assertAnd(self, expr, *args):
        self.assertIs(type(expr), AndOperator)
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
        self.assertIs(type(expr), OrOperator)
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
        for term in reversed(subExprs):
            shifted = LShift(term, offset).simplify()
            if not (isinstance(shifted, IntLiteral) and shifted.value == 0):
                compExprs.append(shifted)
            if term.width is None:
                offset = None
            else:
                offset += term.width
        self.assertOr(expr, *compExprs)

    def assertSlice(self, expr, subExpr, index, width):
        needsShift = index != 0
        shift = RShift(subExpr, index) if needsShift else subExpr
        needsTrunc = subExpr.width != index + width
        trunc = Truncation(shift, width) if needsTrunc else shift
        self.assertEqual(str(expr), str(trunc))
        self.assertEqual(expr, trunc)
        self.assertIs(type(expr), type(trunc))
        self.assertIs(type(expr.type), IntType)
        if needsShift:
            shiftExpr = expr.expr if needsTrunc else expr
            self.assertEqual(str(shiftExpr), str(shift))
            self.assertEqual(shiftExpr, shift)
            self.assertIs(type(shiftExpr), RShift)
            self.assertIs(type(shiftExpr.type), IntType)
            self.assertEqual(shiftExpr.offset, index)
            self.assertEqual(shiftExpr.expr, subExpr)
        else:
            self.assertEqual(expr.expr, subExpr)
        self.assertEqual(expr.width, width)

class AndTests(TestUtils):

    def test_literals(self):
        '''Applies logical AND to integer literals.'''
        a = IntLiteral(0xE3, IntType(8))
        b = IntLiteral(0x7A, IntType(8))
        self.assertIntLiteral(AndOperator(a, b).simplify(), 0x62)
        c = IntLiteral(0xFF00, IntType(16))
        d = IntLiteral.create(0x123456)
        self.assertIntLiteral(AndOperator(c, d).simplify(), 0x3400)

    def test_identity(self):
        '''Simplifies logical AND expressions containing -1.'''
        addr = LocalValue('A', IntType(16))
        ones = IntLiteral.create(-1)
        # Check whether identity values are filtered out.
        self.assertIs(AndOperator(ones, addr).simplify(), addr)
        self.assertIs(AndOperator(addr, ones).simplify(), addr)
        self.assertIs(AndOperator(ones, addr, ones).simplify(), addr)
        # Check graceful handling when zero subexpressions remain.
        self.assertIntLiteral(AndOperator(ones, ones, ones).simplify(), -1)

    def test_absorbtion(self):
        '''Simplifies logical AND expressions containing 0.'''
        addr = LocalValue('A', IntType(16))
        zero = IntLiteral.create(0)
        self.assertIntLiteral(AndOperator(zero, addr).simplify(), 0)
        self.assertIntLiteral(AndOperator(addr, zero).simplify(), 0)
        self.assertIntLiteral(AndOperator(addr, zero, addr).simplify(), 0)

    def test_idempotence(self):
        '''Simplifies logical AND expressions containing duplicates.'''
        addr = LocalValue('A', IntType(16))
        self.assertIs(AndOperator(addr, addr).simplify(), addr)
        self.assertIs(AndOperator(addr, addr, addr).simplify(), addr)
        mask = LocalValue('M', IntType(16))
        self.assertAnd(AndOperator(mask, addr, mask).simplify(), addr, mask)

    def test_width(self):
        '''Simplifies logical AND expressions using the subexpression widths.'''
        h = LocalValue('H', IntType(8))
        l = LocalValue('L', IntType(8))
        hl = createConcatenation(h, l)
        mask = IntLiteral(0x00F0, IntType(16))
        # Test whether (HL & $00F0) cuts off H.
        self.assertAnd(AndOperator(hl, mask).simplify(), mask, l)
        # Test whether (HL & H) cuts off H.
        self.assertAnd(AndOperator(hl, h).simplify(), h, l)
        # Test whether (HL & L) simplifies to L.
        self.assertIs(AndOperator(hl, l).simplify(), l)

    def test_mask_to_slice(self):
        '''Simplifies logical AND expressions that are essentially slicing.'''
        h = LocalValue('H', IntType(8))
        l = LocalValue('L', IntType(8))
        hl = createConcatenation(h, l)
        # Test whether (HL & $003F) simplifies to L[0:6].
        mask6 = IntLiteral(0x003F, IntType(16))
        self.assertSlice(AndOperator(hl, mask6).simplify(), l, 0, 6)
        # Test whether (HL & $00FF) simplifies to L.
        mask8 = IntLiteral(0x00FF, IntType(16))
        self.assertIs(AndOperator(hl, mask8).simplify(), l)

    def test_mask_concat(self):
        '''Simplifies logical AND expressions that are essentially slicing.'''
        h = LocalValue('H', IntType(8))
        l = LocalValue('L', IntType(8))
        hl = createConcatenation(h, l)
        # Test whether (HL & $FF00) simplifies to H;$00.
        self.assertConcat(
            AndOperator(hl, IntLiteral(0xFF00, IntType(16))).simplify(),
            (h, IntLiteral(0, IntType(8)))
            )

class OrTests(TestUtils):

    def test_literals(self):
        '''Applies logical OR to integer literals.'''
        a = IntLiteral(0x4C, IntType(8))
        b = IntLiteral(0x91, IntType(8))
        self.assertIntLiteral(OrOperator(a, b).simplify(), 0xDD)
        c = IntLiteral(0x00FF, IntType(16))
        d = IntLiteral.create(0x120021)
        self.assertIntLiteral(OrOperator(c, d).simplify(), 0x1200FF)

    def test_identity(self):
        '''Simplifies logical OR expressions containing 0.'''
        addr = LocalValue('A', IntType(16))
        zero = IntLiteral.create(0)
        # Check whether identity values are filtered out.
        self.assertIs(OrOperator(zero, addr).simplify(), addr)
        self.assertIs(OrOperator(addr, zero).simplify(), addr)
        self.assertIs(OrOperator(zero, addr, zero).simplify(), addr)
        # Check graceful handling when zero subexpressions remain.
        self.assertIntLiteral(OrOperator(zero, zero, zero).simplify(), 0)

    def test_absorbtion(self):
        '''Simplifies logical OR expressions containing -1.'''
        addr = LocalValue('A', IntType(16))
        ones = IntLiteral.create(-1)
        self.assertIntLiteral(OrOperator(ones, addr).simplify(), -1)
        self.assertIntLiteral(OrOperator(addr, ones).simplify(), -1)
        self.assertIntLiteral(OrOperator(addr, ones, addr).simplify(), -1)

    def test_idempotence(self):
        '''Simplifies logical OR expressions containing duplicates.'''
        addr = LocalValue('A', IntType(16))
        self.assertIs(OrOperator(addr, addr).simplify(), addr)
        self.assertIs(OrOperator(addr, addr, addr).simplify(), addr)
        mask = LocalValue('M', IntType(16))
        self.assertOr(OrOperator(mask, addr, mask).simplify(), addr, mask)

class AddTests(TestUtils):

    def test_int(self):
        '''Adds two unlimited width integer literals.'''
        arg1 = IntLiteral.create(3)
        arg2 = IntLiteral.create(20)
        expr = AddOperator(arg1, arg2).simplify()
        self.assertIntLiteral(expr, 23)

    def test_fixed_width(self):
        '''Adds two fixed width integer literals.'''
        arg1 = IntLiteral(8, IntType(4))
        arg2 = IntLiteral(127, IntType(8))
        expr = AddOperator(arg1, arg2).simplify()
        self.assertIntLiteral(expr, 135)

    def test_nested(self):
        '''Adds several integers in an expression tree.'''
        arg1 = AddOperator(IntLiteral.create(1), IntLiteral.create(2))
        arg2 = AddOperator(IntLiteral.create(3), IntLiteral.create(4))
        expr = AddOperator(arg1, arg2).simplify()
        self.assertIntLiteral(expr, 10)

    def test_zero(self):
        '''Test simplification of zero literal terms.'''
        zero = IntLiteral.create(0)
        addr = LocalValue('A', IntType(16))
        self.assertIs(AddOperator(zero, addr).simplify(), addr)
        self.assertIs(AddOperator(addr, zero).simplify(), addr)
        self.assertIntLiteral(AddOperator(zero, zero).simplify(), 0)

    def test_associative(self):
        '''Test simplification using the associativity of addition.'''
        addr = LocalValue('A', IntType(16))
        arg1 = AddOperator(addr, IntLiteral.create(1))
        arg2 = AddOperator(IntLiteral.create(2), IntLiteral.create(-3))
        self.assertIs(AddOperator(arg1, arg2).simplify(), addr)

    def test_commutative(self):
        '''Test simplification using the commutativity of addition.'''
        addr = LocalValue('A', IntType(16))
        arg1 = AddOperator(IntLiteral.create(1), IntLiteral.create(2))
        arg2 = AddOperator(addr, IntLiteral.create(-3))
        self.assertIs(AddOperator(arg1, arg2).simplify(), addr)

class SubTests(TestUtils):

    def test_int(self):
        '''Subtracts two unlimited width integer literals.'''
        arg1 = IntLiteral.create(3)
        arg2 = IntLiteral.create(20)
        expr = createSubtraction(arg1, arg2).simplify()
        self.assertIntLiteral(expr, -17)

    def test_fixed_width(self):
        '''Subtracts two fixed width integer literals.'''
        arg1 = IntLiteral(8, IntType(4))
        arg2 = IntLiteral(127, IntType(8))
        expr = createSubtraction(arg1, arg2).simplify()
        self.assertIntLiteral(expr, -119)

    def test_nested(self):
        '''Subtracts several integers in an expression tree.'''
        arg1 = createSubtraction(IntLiteral.create(234), IntLiteral.create(123))
        arg2 = createSubtraction(IntLiteral.create(31), IntLiteral.create(63))
        expr = createSubtraction(arg1, arg2).simplify()
        self.assertIntLiteral(expr, 143)

    def test_zero(self):
        '''Test simplification of zero literal terms.'''
        zero = IntLiteral.create(0)
        addr = LocalValue('A', IntType(16))
        leftZero = createSubtraction(zero, addr)
        self.assertEqual(type(leftZero.simplify()), Complement)
        self.assertIs(createSubtraction(addr, zero).simplify(), addr)
        self.assertIntLiteral(createSubtraction(zero, zero).simplify(), 0)

    def test_self(self):
        '''Test simplification of subtracting an expression from itself.'''
        a = LocalValue('A', IntType(16))
        b = LocalValue('B', IntType(8))
        self.assertIntLiteral(createSubtraction(a, a).simplify(), 0)
        c = AddOperator(a, b)
        d = AddOperator(b, a)
        self.assertIntLiteral(createSubtraction(c, c).simplify(), 0)
        self.assertIntLiteral(createSubtraction(c, d).simplify(), 0)
        e = createConcatenation(a, b)
        self.assertIntLiteral(createSubtraction(e, e).simplify(), 0)

class ComplementTests(TestUtils):

    def test_int(self):
        '''Takes the complement of an integer literal.'''
        expr = Complement(IntLiteral.create(4))
        self.assertIntLiteral(expr.simplify(), -4)

    def test_twice(self):
        '''Takes the complement of a complement.'''
        addr = LocalValue('A', IntType(16))
        self.assertIs(Complement(Complement(addr)).simplify(), addr)

    def test_subexpr(self):
        '''Takes the complement of a simplifiable subexpression.'''
        addr = LocalValue('A', IntType(16))
        expr = Complement(createConcatenation(
            IntLiteral(0xC0, IntType(8)),
            IntLiteral(0xDE, IntType(8)),
            addr
            )).simplify()
        self.assertIs(type(expr), Complement)
        self.assertConcat(expr.expr, (IntLiteral(0xC0DE, IntType(16)), addr))

class ArithmeticTests(TestUtils):

    def test_int(self):
        '''Uses add/sub/complement on several integer literals.'''
        expr = AddOperator(
            createSubtraction(IntLiteral.create(0), IntLiteral.create(39)),
            Complement(
                createSubtraction(IntLiteral.create(101), IntLiteral.create(1001))
                )
            )
        self.assertIntLiteral(expr.simplify(), 861)

    def test_associative(self):
        '''Test simplification using the associativity of addition.
        Note that that associativity cannot be exploited unless the subtraction
        is converted into an addition first.
        '''
        addr = LocalValue('A', IntType(16))
        arg1 = createSubtraction(addr, IntLiteral.create(1))
        arg2 = AddOperator(IntLiteral.create(-2), IntLiteral.create(3))
        self.assertIs(AddOperator(arg1, arg2).simplify(), addr)

    def test_commutative(self):
        '''Test simplification using the commutativity of addition.
        Note that that commutativity cannot be exploited unless the subtraction
        is converted into an addition first.
        '''
        addr = LocalValue('A', IntType(16))
        arg1 = AddOperator(IntLiteral.create(1), IntLiteral.create(2))
        arg2 = AddOperator(Complement(addr), IntLiteral.create(3))
        self.assertIs(createSubtraction(arg1, arg2).simplify(), addr)

class LShiftTests(TestUtils):

    def test_literals(self):
        '''Shifts an integer literal to the left.'''
        self.assertIntLiteral(
            LShift(IntLiteral.create(0x1234), 8).simplify(),
            0x123400
            )
        self.assertIntLiteral(
            LShift(IntLiteral(0xDA, IntType(8)), 16).simplify(),
            0xDA0000
            )

    def test_twice(self):
        '''Shifts a value to the left twice.'''
        addr = LocalValue('A', IntType(16))
        expr = LShift(LShift(addr, 3), 5).simplify()
        self.assertIs(type(expr), LShift)
        self.assertIs(expr.expr, addr)
        self.assertEqual(expr.offset, 8)

    def test_rshift(self):
        '''Tests left-shifting after right-shifting.'''
        addr = LocalValue('A', IntType(16))
        # Shift more to the right than to the left.
        rwin = LShift(RShift(addr, 5), 3).simplify()
        self.assertAnd(rwin, RShift(addr, 2), IntLiteral.create(0x3FF8))
        # Shift equal amounts to the right and to the left.
        draw = LShift(RShift(addr, 4), 4).simplify()
        self.assertAnd(draw, addr, IntLiteral.create(0xFFF0))
        # Shift less to the right than to the left.
        lwin = LShift(RShift(addr, 3), 5).simplify()
        self.assertAnd(lwin, LShift(addr, 2), IntLiteral.create(0x3FFE0))

    def test_truncate(self):
        '''Tests truncation of a left-shifted expression.'''
        h = LocalValue('H', IntType(8))
        l = LocalValue('L', IntType(8))
        hl = createConcatenation(h, l)
        # Shift H and L out of the truncation range.
        expr1 = Truncation(LShift(hl, 8), 8).simplify()
        self.assertIntLiteral(expr1, 0)
        # Shift only H out of the truncation range.
        expr2 = Truncation(LShift(hl, 8), 16).simplify()
        self.assertIs(type(expr2), LShift)
        self.assertIs(expr2.expr, l)
        self.assertEqual(expr2.offset, 8)

class RShiftTests(TestUtils):

    def test_lshift(self):
        '''Tests right-shifting after left-shifting.'''
        addr = LocalValue('A', IntType(16))
        # Shift less to the left than to the right.
        rwin = RShift(LShift(addr, 3), 5).simplify()
        self.assertIs(type(rwin), RShift)
        self.assertEqual(rwin.offset, 2)
        self.assertIs(rwin.expr, addr)
        # Shift equal amounts to the left and to the right.
        draw = RShift(LShift(addr, 4), 4).simplify()
        self.assertIs(draw, addr)
        # Shift more to the left than to the right.
        lwin = RShift(LShift(addr, 5), 3).simplify()
        self.assertIs(type(lwin), LShift)
        self.assertEqual(lwin.offset, 2)
        self.assertIs(lwin.expr, addr)

class ConcatTests(TestUtils):

    def test_literals(self):
        '''Concatenates integer literals.'''
        ip = IntLiteral.create(4)
        im = Complement(ip)
        u4 = IntLiteral(0xD, IntType(4))
        u8 = IntLiteral(0x29, IntType(8))
        cat_ip_u4 = createConcatenation(ip, u4).simplify()
        self.assertIntLiteral(cat_ip_u4, 0x40 + 0xD)
        cat_ip_u8 = createConcatenation(ip, u8).simplify()
        self.assertIntLiteral(cat_ip_u8, 0x400 + 0x29)
        cat_im_u4 = createConcatenation(im, u4).simplify()
        self.assertIntLiteral(cat_im_u4, -0x40 + 0xD)
        cat_im_u8 = createConcatenation(im, u8).simplify()
        self.assertIntLiteral(cat_im_u8, -0x400 + 0x29)
        cat_u4_u4 = createConcatenation(u4, u4).simplify()
        self.assertIntLiteral(cat_u4_u4, 0xDD)
        cat_u4_u8 = createConcatenation(u4, u8).simplify()
        self.assertIntLiteral(cat_u4_u8, 0xD29)
        cat_u8_u4 = createConcatenation(u8, u4).simplify()
        self.assertIntLiteral(cat_u8_u4, 0x29D)
        cat_u8_u8 = createConcatenation(u8, u8).simplify()
        self.assertIntLiteral(cat_u8_u8, 0x2929)

    def test_identity(self):
        '''Simplifies concatenations containing identity values.'''
        addr = LocalValue('A', IntType(16))
        # Check whether empty bitstrings are filtered out.
        empty = IntLiteral(0, IntType(0))
        head = createConcatenation(empty, addr, addr)
        self.assertConcat(head.simplify(), (addr, addr))
        mid = createConcatenation(addr, empty, addr)
        self.assertConcat(mid.simplify(), (addr, addr))
        tail = createConcatenation(addr, addr, empty)
        self.assertConcat(tail.simplify(), (addr, addr))
        many = createConcatenation(
            empty, empty, addr, empty, empty, addr, empty, empty
            )
        self.assertConcat(many.simplify(), (addr, addr))
        # Check graceful handling when zero subexpressions remain.
        only = createConcatenation(empty, empty, empty)
        self.assertIntLiteral(only.simplify(), 0)
        # Check whether non-empty fixed-width zero-valued bitstrings are kept.
        zero_u8 = IntLiteral(0, IntType(8))
        mid_u8 = createConcatenation(addr, zero_u8, addr)
        self.assertConcat(mid_u8.simplify(), (addr, zero_u8, addr))
        tail_u8 = createConcatenation(addr, addr, zero_u8)
        self.assertConcat(tail_u8.simplify(), (addr, addr, zero_u8))
        # Check whether unlimited-width zero-valued bitstrings are kept.
        zero_int = IntLiteral.create(0)
        head_int = createConcatenation(zero_int, addr, addr)
        self.assertConcat(head_int.simplify(), (zero_int, addr, addr))

    def test_associative(self):
        '''Test simplification using the associativity of concatenation.
        '''
        addr = LocalValue('A', IntType(16))
        arg1 = createConcatenation(addr, addr) # (A ; A)
        arg2 = createConcatenation(arg1, arg1) # ((A ; A) ; (A ; A))
        arg3 = createConcatenation(arg1, arg2) # ((A ; A) ; ((A ; A) ; (A ; A)))
        self.assertConcat(arg3.simplify(), (addr, ) * 6)

    def test_associative2(self):
        '''Test simplification using the associativity of concatenation.
        '''
        addr = LocalValue('A', IntType(16))
        arg1 = createConcatenation(addr, IntLiteral(0x9, IntType(4))) # (A ; $9)
        arg2 = createConcatenation(IntLiteral(0x63, IntType(8)), addr) # ($63 ; A)
        arg3 = createConcatenation(arg1, arg2) # ((A ; $9) ; ($63 ; A))
        self.assertConcat(
            arg3.simplify(),
            (addr, IntLiteral(0x963, IntType(12)), addr)
            )

class SliceTests(TestUtils):

    def test_literals(self):
        '''Slices integer literals.'''
        addr = IntLiteral(0xFD56, IntType(16))
        self.assertIntLiteral(createSlice(addr, 0, 16).simplify(), 0xFD56)
        self.assertIntLiteral(createSlice(addr, 4, 8).simplify(), 0xD5)
        self.assertIntLiteral(createSlice(addr, 8, 12).simplify(), 0x0FD)
        signed = IntLiteral.create(-0x1995)
        self.assertIntLiteral(createSlice(signed, 0, 16).simplify(), 0xE66B)
        self.assertIntLiteral(createSlice(signed, 4, 8).simplify(), 0x66)
        self.assertIntLiteral(createSlice(signed, 8, 12).simplify(), 0xFE6)

    def test_zero_width(self):
        '''Takes a slices of width 0.'''
        addr = LocalValue('A', IntType(16))
        self.assertIntLiteral(createSlice(addr, 8, 0).simplify(), 0)

    def test_full_range(self):
        '''Slices a range that exactly matches a value's type.'''
        addr = LocalValue('A', IntType(16))
        self.assertIs(createSlice(addr, 0, 16).simplify(), addr)

    def test_out_of_range(self):
        '''Slices a range that is fully outside a value's type.'''
        addr = LocalValue('A', IntType(16))
        self.assertIntLiteral(createSlice(addr, 16, 8).simplify(), 0)

    def test_leading_zeroes(self):
        '''Slices a range that is partially outside a value's type.'''
        addr = LocalValue('A', IntType(16))
        expr = createSlice(addr, 0, 20).simplify() # $0xxxx
        self.assertIs(expr, addr)
        expr = createSlice(addr, 8, 12).simplify() # $0xx
        self.assertSlice(expr, addr, 8, 8)

    def test_double_slice(self):
        '''Slices a range from another slice.'''
        addr = LocalValue('A', IntType(16))
        expr = createSlice(createSlice(addr, 3, 10), 2, 6).simplify()
        self.assertSlice(expr, addr, 5, 6)

    def test_concat(self):
        '''Slices a range from a concatenation.'''
        a = LocalValue('A', IntType(8))
        b = LocalValue('B', IntType(8))
        c = LocalValue('C', IntType(8))
        d = LocalValue('D', IntType(8))
        abcd = createConcatenation(a, b, c, d)
        # Test slicing out individual values.
        self.assertIs(createSlice(abcd, 0, 8).simplify(), d)
        self.assertIs(createSlice(abcd, 8, 8).simplify(), c)
        self.assertIs(createSlice(abcd, 16, 8).simplify(), b)
        self.assertIs(createSlice(abcd, 24, 8).simplify(), a)
        # Test slice edges at subexpression boundaries.
        bc = createSlice(abcd, 8, 16).simplify()
        self.assertConcat(bc, (b, c))
        self.assertEqual(bc.width, 16)
        # Test one slice edge at subexpression boundaries.
        self.assertSlice(createSlice(abcd, 0, 5).simplify(), d, 0, 5)
        self.assertSlice(createSlice(abcd, 8, 5).simplify(), c, 0, 5)
        self.assertSlice(createSlice(abcd, 19, 5).simplify(), b, 3, 5)
        self.assertSlice(createSlice(abcd, 27, 5).simplify(), a, 3, 5)
        # Test slice entirely inside one subexpression.
        self.assertSlice(createSlice(abcd, 10, 4).simplify(), c, 2, 4)
        # Test slice across subexpression boundaries.
        self.assertConcat(
            createSlice(abcd, 10, 9).simplify(),
            (Truncation(b, 3), RShift(c, 2))
            )

    def test_and(self):
        '''Tests simplification of slicing a logical AND.'''
        h = LocalValue('H', IntType(8))
        l = LocalValue('L', IntType(8))
        hl = createConcatenation(h, l)
        # Test whether slicing cuts off L.
        expr1 = AndOperator(hl, IntLiteral.create(0xBFFF))
        self.assertSlice(createSlice(expr1, 8, 6).simplify(), h, 0, 6)
        # Test whether redundant slicing can be eliminated.
        self.assertAnd(createSlice(AndOperator(h, l), 0, 8).simplify(), h, l)

    def test_add(self):
        '''Tests simplification of slicing an addition.'''
        h = LocalValue('H', IntType(8))
        l = LocalValue('L', IntType(8))
        hl = createConcatenation(h, l)
        expr = AddOperator(hl, IntLiteral.create(2))
        # Simplifcation fails because index is not 0.
        up8 = createSlice(expr, 8, 8).simplify()
        self.assertSlice(up8, expr.simplify(), 8, 8)
        # Successful simplification: slice lowest 8 bits.
        low8 = createSlice(expr, 0, 8).simplify()
        add8 = Truncation(AddOperator(l, IntLiteral.create(2)), 8)
        self.assertEqual(low8, add8)
        # Successful simplification: slice lowest 6 bits.
        low6 = createSlice(expr, 0, 6).simplify()
        add6 = Truncation(
            AddOperator(Truncation(l, 6), IntLiteral.create(2)), 6
            )
        self.assertEqual(low6, add6)
        # Simplification fails because expression becomes more complex.
        low12 = Truncation(expr.simplify(), 12)
        low12s = low12.simplify()
        self.assertEqual(str(low12s), str(low12))
        self.assertEqual(low12s, low12)

    def test_complement(self):
        '''Tests simplification of slicing a complement.'''
        h = LocalValue('H', IntType(8))
        l = LocalValue('L', IntType(8))
        hl = createConcatenation(h, l)
        expr = Complement(hl)
        # Simplifcation fails because index is not 0.
        up8 = createSlice(expr, 8, 8)
        self.assertSlice(up8.simplify(), expr.simplify(), 8, 8)
        # Successful simplification: slice lowest 8 bits.
        low8 = createSlice(expr, 0, 8).simplify()
        cpl8 = Truncation(Complement(l), 8)
        self.assertEqual(str(low8), str(cpl8))
        self.assertEqual(low8, cpl8)
        # Successful simplification: slice lowest 6 bits.
        low6 = createSlice(expr, 0, 6).simplify()
        cpl6 = Truncation(Complement(Truncation(l, 6)), 6)
        self.assertEqual(str(low6), str(cpl6))
        self.assertEqual(low6, cpl6)
        # Simplification fails because expression becomes more complex.
        low12 = Truncation(expr, 12).simplify()
        self.assertSlice(low12, expr.simplify(), 0, 12)

    def test_mixed(self):
        '''Tests a mixture of slicing, concatenation and leading zeroes.'''
        addr = LocalValue('A', IntType(16))
        expr_int = createSlice(
            createConcatenation(
                IntLiteral.create(7), createSlice(addr, 8, 12)
                ),
            8, 8
            )
        self.assertIntLiteral(expr_int.simplify(), 0x70)
        expr_u8 = createSlice(
            createConcatenation(
                IntLiteral(7, IntType(4)), createSlice(addr, 8, 12)
                ),
            8, 8
            )
        self.assertIntLiteral(expr_u8.simplify(), 0x70)

if __name__ == '__main__':
    unittest.main()
