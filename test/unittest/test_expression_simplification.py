from retroasm.expression import (
    AddOperator, AndOperator, Complement, Concatenation, IntLiteral, IntType,
    LocalValue, Shift, Truncation, createSlice, createSubtraction
    )

import unittest

class TestUtils(unittest.TestCase):

    def assertIntLiteral(self, expr, value):
        '''Asserts that the given expression is an unlimited-width int literal
        with the given value.
        '''
        comparison = IntLiteral.create(value)
        self.assertEqual(str(expr), str(comparison))
        self.assertEqual(expr, comparison)
        self.assertIs(type(expr), IntLiteral)
        self.assertIs(type(expr.type), IntType)
        self.assertIs(expr.width, None)
        self.assertEqual(expr.value, value)

    def assertUnsignedLiteral(self, expr, value, width):
        '''Asserts that the given expression is a fixed-width unsigned literal
        with the given value.
        '''
        comparison = IntLiteral(value, IntType(width))
        self.assertEqual(str(expr), str(comparison))
        self.assertEqual(expr, comparison)
        self.assertIs(type(expr), IntLiteral)
        self.assertIs(type(expr.type), IntType)
        self.assertEqual(expr.width, width)
        self.assertEqual(expr.value, value)

    def assertConcat(self, expr, subExprs):
        comparison = Concatenation(*subExprs)
        self.assertEqual(str(expr), str(comparison))
        self.assertEqual(expr, comparison)
        self.assertIs(type(expr), Concatenation)
        self.assertIs(type(expr.type), IntType)
        self.assertEqual(expr.width, comparison.width)
        self.assertEqual(len(expr.exprs), len(subExprs))
        for actual, expected in zip(expr.exprs, subExprs):
            self.assertEqual(actual, expected)

    def assertSlice(self, expr, subExpr, index, width):
        needsShift = index != 0
        shift = Shift(subExpr, index) if needsShift else subExpr
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
            self.assertIs(type(shiftExpr), Shift)
            self.assertIs(type(shiftExpr.type), IntType)
            self.assertEqual(shiftExpr.offset, index)
            self.assertEqual(shiftExpr.expr, subExpr)
        else:
            self.assertEqual(expr.expr, subExpr)
        self.assertEqual(expr.width, width)

class AndTests(TestUtils):

    def test_literals(self):
        '''Applies logical and to integer literals.'''
        a = IntLiteral(0xE3, IntType(8))
        b = IntLiteral(0x7A, IntType(8))
        self.assertIntLiteral(AndOperator(a, b).simplify(), 0x62)
        c = IntLiteral(0xFF00, IntType(16))
        d = IntLiteral.create(0x123456)
        self.assertIntLiteral(AndOperator(c, d).simplify(), 0x3400)

    def test_identity(self):
        '''Simplifies logical and expressions containing -1.'''
        addr = LocalValue('A', IntType(16))
        ones = IntLiteral.create(-1)
        # Check whether identity values are filtered out.
        self.assertIs(AndOperator(ones, addr).simplify(), addr)
        self.assertIs(AndOperator(addr, ones).simplify(), addr)
        self.assertIs(AndOperator(ones, addr, ones).simplify(), addr)
        # Check graceful handling when zero subexpressions remain.
        self.assertIntLiteral(AndOperator(ones, ones, ones).simplify(), -1)

    def test_absorbtion(self):
        '''Simplifies logical and expressions containing 0.'''
        addr = LocalValue('A', IntType(16))
        zero = IntLiteral.create(0)
        self.assertIntLiteral(AndOperator(zero, addr).simplify(), 0)
        self.assertIntLiteral(AndOperator(addr, zero).simplify(), 0)
        self.assertIntLiteral(AndOperator(addr, zero, addr).simplify(), 0)

    def test_idempotence(self):
        '''Simplifies logical and expressions containing the duplicates.'''
        addr = LocalValue('A', IntType(16))
        self.assertIs(AndOperator(addr, addr).simplify(), addr)
        self.assertIs(AndOperator(addr, addr, addr).simplify(), addr)
        mask = LocalValue('M', IntType(16))
        expr = AndOperator(mask, addr, mask).simplify()
        self.assertIs(type(expr), AndOperator)
        self.assertEqual(len(expr.exprs), 2)
        self.assertTrue(addr in expr.exprs)
        self.assertTrue(mask in expr.exprs)

    def test_width(self):
        '''Simplifies logical and expressions using the subexpression widths.'''
        h = LocalValue('H', IntType(8))
        l = LocalValue('L', IntType(8))
        hl = Concatenation(h, l)
        mask = IntLiteral(0x00F0, IntType(16))
        # Test whether (HL & $00F0) cuts off H.
        expr1 = AndOperator(hl, mask).simplify()
        self.assertIs(type(expr1), AndOperator)
        self.assertEqual(len(expr1.exprs), 2)
        self.assertTrue(mask in expr1.exprs)
        self.assertTrue(l in expr1.exprs)
        # Test whether (HL & H) cuts off H.
        expr2 = AndOperator(hl, h).simplify()
        self.assertIs(type(expr2), AndOperator)
        self.assertEqual(len(expr2.exprs), 2)
        self.assertTrue(h in expr2.exprs)
        self.assertTrue(l in expr2.exprs)
        # Test whether (HL & L) simplifies to L.
        self.assertIs(AndOperator(hl, l).simplify(), l)

    def test_mask_to_slice(self):
        '''Simplifies logical and expressions that are essentially slicing.'''
        h = LocalValue('H', IntType(8))
        l = LocalValue('L', IntType(8))
        hl = Concatenation(h, l)
        # Test whether (HL & $003F) simplifies to L[0:6].
        mask6 = IntLiteral(0x003F, IntType(16))
        self.assertSlice(AndOperator(hl, mask6).simplify(), l, 0, 6)
        # Test whether (HL & $00FF) simplifies to L.
        mask8 = IntLiteral(0x00FF, IntType(16))
        self.assertIs(AndOperator(hl, mask8).simplify(), l)

    def test_mask_concat(self):
        '''Simplifies logical and expressions that are essentially slicing.'''
        h = LocalValue('H', IntType(8))
        l = LocalValue('L', IntType(8))
        hl = Concatenation(h, l)
        # Test whether (HL & $FF00) simplifies to H;$00.
        self.assertConcat(
            AndOperator(hl, IntLiteral(0xFF00, IntType(16))).simplify(),
            (h, IntLiteral(0, IntType(8)))
            )

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
        e = Concatenation(a, b)
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
        expr = Complement(Concatenation(
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

class ConcatTests(TestUtils):

    def test_literals(self):
        '''Concatenates integer literals.'''
        ip = IntLiteral.create(4)
        im = Complement(ip)
        u4 = IntLiteral(0xD, IntType(4))
        u8 = IntLiteral(0x29, IntType(8))
        cat_ip_u4 = Concatenation(ip, u4).simplify()
        self.assertIntLiteral(cat_ip_u4, 0x40 + 0xD)
        cat_ip_u8 = Concatenation(ip, u8).simplify()
        self.assertIntLiteral(cat_ip_u8, 0x400 + 0x29)
        cat_im_u4 = Concatenation(im, u4).simplify()
        self.assertIntLiteral(cat_im_u4, -0x40 + 0xD)
        cat_im_u8 = Concatenation(im, u8).simplify()
        self.assertIntLiteral(cat_im_u8, -0x400 + 0x29)
        cat_u4_u4 = Concatenation(u4, u4).simplify()
        self.assertUnsignedLiteral(cat_u4_u4, 0xDD, 8)
        cat_u4_u8 = Concatenation(u4, u8).simplify()
        self.assertUnsignedLiteral(cat_u4_u8, 0xD29, 12)
        cat_u8_u4 = Concatenation(u8, u4).simplify()
        self.assertUnsignedLiteral(cat_u8_u4, 0x29D, 12)
        cat_u8_u8 = Concatenation(u8, u8).simplify()
        self.assertUnsignedLiteral(cat_u8_u8, 0x2929, 16)

    def test_identity(self):
        '''Simplifies concatenations containing identity values.'''
        addr = LocalValue('A', IntType(16))
        # Check whether empty bitstrings are filtered out.
        empty = IntLiteral(0, IntType(0))
        head = Concatenation(empty, addr, addr)
        self.assertConcat(head.simplify(), (addr, addr))
        mid = Concatenation(addr, empty, addr)
        self.assertConcat(mid.simplify(), (addr, addr))
        tail = Concatenation(addr, addr, empty)
        self.assertConcat(tail.simplify(), (addr, addr))
        many = Concatenation(
            empty, empty, addr, empty, empty, addr, empty, empty
            )
        self.assertConcat(many.simplify(), (addr, addr))
        # Check graceful handling when zero subexpressions remain.
        only = Concatenation(empty, empty, empty)
        self.assertUnsignedLiteral(only.simplify(), 0, 0)
        # Check whether non-empty fixed-width zero-valued bitstrings are kept.
        zero_u8 = IntLiteral(0, IntType(8))
        head_u8 = Concatenation(zero_u8, addr, addr)
        self.assertConcat(head_u8.simplify(), (zero_u8, addr, addr))
        mid_u8 = Concatenation(addr, zero_u8, addr)
        self.assertConcat(mid_u8.simplify(), (addr, zero_u8, addr))
        tail_u8 = Concatenation(addr, addr, zero_u8)
        self.assertConcat(tail_u8.simplify(), (addr, addr, zero_u8))
        # Check whether unlimited-width zero-valued bitstrings are kept.
        zero_int = IntLiteral.create(0)
        head_int = Concatenation(zero_int, addr, addr)
        self.assertConcat(head_int.simplify(), (zero_int, addr, addr))

    def test_associative(self):
        '''Test simplification using the associativity of concatenation.
        '''
        addr = LocalValue('A', IntType(16))
        arg1 = Concatenation(addr, addr) # (A ; A)
        arg2 = Concatenation(arg1, arg1) # ((A ; A) ; (A ; A))
        arg3 = Concatenation(arg1, arg2) # ((A ; A) ; ((A ; A) ; (A ; A)))
        self.assertConcat(arg3.simplify(), (addr, ) * 6)

    def test_associative2(self):
        '''Test simplification using the associativity of concatenation.
        '''
        addr = LocalValue('A', IntType(16))
        arg1 = Concatenation(addr, IntLiteral(0x9, IntType(4))) # (A ; $9)
        arg2 = Concatenation(IntLiteral(0x63, IntType(8)), addr) # ($63 ; A)
        arg3 = Concatenation(arg1, arg2) # ((A ; $9) ; ($63 ; A))
        self.assertConcat(
            arg3.simplify(),
            (addr, IntLiteral(0x963, IntType(12)), addr)
            )

class SliceTests(TestUtils):

    def test_literals(self):
        '''Slices integer literals.'''
        addr = IntLiteral(0xFD56, IntType(16))
        self.assertUnsignedLiteral(createSlice(addr, 0, 16).simplify(), 0xFD56, 16)
        self.assertUnsignedLiteral(createSlice(addr, 4, 8).simplify(), 0xD5, 8)
        self.assertUnsignedLiteral(createSlice(addr, 8, 12).simplify(), 0x0FD, 12)
        signed = IntLiteral.create(-0x1995)
        self.assertUnsignedLiteral(createSlice(signed, 0, 16).simplify(), 0xE66B, 16)
        self.assertUnsignedLiteral(createSlice(signed, 4, 8).simplify(), 0x66, 8)
        self.assertUnsignedLiteral(createSlice(signed, 8, 12).simplify(), 0xFE6, 12)

    def test_zero_width(self):
        '''Takes a slices of width 0.'''
        addr = LocalValue('A', IntType(16))
        self.assertUnsignedLiteral(createSlice(addr, 8, 0).simplify(), 0, 0)

    def test_full_range(self):
        '''Slices a range that exactly matches a value's type.'''
        addr = LocalValue('A', IntType(16))
        self.assertIs(createSlice(addr, 0, 16).simplify(), addr)

    def test_out_of_range(self):
        '''Slices a range that is fully outside a value's type.'''
        addr = LocalValue('A', IntType(16))
        self.assertUnsignedLiteral(createSlice(addr, 16, 8).simplify(), 0, 8)

    def test_leading_zeroes(self):
        '''Slices a range that is partially outside a value's type.'''
        addr = LocalValue('A', IntType(16))
        expr = createSlice(addr, 0, 20).simplify() # $0xxxx
        self.assertConcat(expr, (IntLiteral(0, IntType(4)), addr))
        expr = createSlice(addr, 8, 12).simplify() # $0xx
        self.assertConcat(expr, (IntLiteral(0, IntType(4)), Shift(addr, 8)))

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
        abcd = Concatenation(a, b, c, d)
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
            (Truncation(b, 3), Shift(c, 2))
            )

    def test_and(self):
        '''Tests simplification of slicing a logical and.'''
        h = LocalValue('H', IntType(8))
        l = LocalValue('L', IntType(8))
        hl = Concatenation(h, l)
        # Test whether slicing cuts off L.
        expr1 = AndOperator(hl, IntLiteral.create(0xBFFF))
        self.assertSlice(createSlice(expr1, 8, 6).simplify(), h, 0, 6)
        # Test whether redundant slicing can be eliminated.
        expr2 = createSlice(AndOperator(h, l), 0, 8).simplify()
        self.assertIs(type(expr2), AndOperator)
        self.assertEqual(len(expr2.exprs), 2)
        self.assertTrue(h in expr2.exprs)
        self.assertTrue(l in expr2.exprs)

    def test_add(self):
        '''Tests simplification of slicing an addition.'''
        h = LocalValue('H', IntType(8))
        l = LocalValue('L', IntType(8))
        hl = Concatenation(h, l)
        expr = AddOperator(hl, IntLiteral.create(2))
        # Simplifcation fails because index is not 0.
        self.assertSlice(createSlice(expr, 8, 8).simplify(), expr, 8, 8)
        # Successful simplification: slice lowest 8 bits.
        low8 = createSlice(expr, 0, 8).simplify()
        add8 = Truncation(AddOperator(l, IntLiteral(2, IntType(8))), 8)
        self.assertEqual(str(low8), str(add8))
        self.assertEqual(low8, add8)
        # Successful simplification: slice lowest 6 bits.
        low6 = createSlice(expr, 0, 6).simplify()
        add6 = Truncation(
            AddOperator(Truncation(l, 6), IntLiteral(2, IntType(6))
            ), 6)
        self.assertEqual(str(low6), str(add6))
        self.assertEqual(low6, add6)
        # Simplification fails because expression becomes more complex.
        low12 = Truncation(expr, 12)
        low12s = low12.simplify()
        self.assertEqual(str(low12s), str(low12))
        self.assertEqual(low12s, low12)
        self.assertIs(low12s, low12)

    def test_complement(self):
        '''Tests simplification of slicing a complement.'''
        h = LocalValue('H', IntType(8))
        l = LocalValue('L', IntType(8))
        hl = Concatenation(h, l)
        expr = Complement(hl)
        # Simplifcation fails because index is not 0.
        up8 = createSlice(expr, 8, 8)
        self.assertSlice(up8.simplify(), expr, 8, 8)
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
        low12 = Truncation(expr, 12)
        low12s = low12.simplify()
        self.assertEqual(str(low12s), str(low12))
        self.assertEqual(low12s, low12)
        self.assertIs(low12s, low12)

    def test_mixed(self):
        '''Tests a mixture of slicing, concatenation and leading zeroes.'''
        addr = LocalValue('A', IntType(16))
        expr_int = createSlice(
            Concatenation(IntLiteral.create(7), createSlice(addr, 8, 12)),
            8, 8
            )
        self.assertUnsignedLiteral(expr_int.simplify(), 0x70, 8)
        expr_u8 = createSlice(
            Concatenation(IntLiteral(7, IntType(4)), createSlice(addr, 8, 12)),
            8, 8
            )
        self.assertUnsignedLiteral(expr_u8.simplify(), 0x70, 8)

if __name__ == '__main__':
    unittest.main()
