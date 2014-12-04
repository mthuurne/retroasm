from retroasm.expression import (
    AddOperator, Complement, Concatenation, IntLiteral, IntType, LocalValue,
    SubOperator
    )

import unittest

class TestUtils(unittest.TestCase):

    def assertIntLiteral(self, expr, value):
        '''Asserts that the given expression is an unlimited-width int literal
        with the given value.
        '''
        self.assertIs(type(expr), IntLiteral)
        self.assertIs(type(expr.type), IntType)
        self.assertIs(expr.width, None)
        self.assertEqual(expr.value, value)

    def assertUnsignedLiteral(self, expr, value, width):
        '''Asserts that the given expression is a fixed-width unsigned literal
        with the given value.
        '''
        self.assertIs(type(expr), IntLiteral)
        self.assertIs(type(expr.type), IntType)
        self.assertEqual(expr.width, width)
        self.assertEqual(expr.value, value)

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
        expr = SubOperator(arg1, arg2).simplify()
        self.assertIntLiteral(expr, -17)

    def test_fixed_width(self):
        '''Subtracts two fixed width integer literals.'''
        arg1 = IntLiteral(8, IntType(4))
        arg2 = IntLiteral(127, IntType(8))
        expr = SubOperator(arg1, arg2).simplify()
        self.assertIntLiteral(expr, -119)

    def test_nested(self):
        '''Subtracts several integers in an expression tree.'''
        arg1 = SubOperator(IntLiteral.create(234), IntLiteral.create(123))
        arg2 = SubOperator(IntLiteral.create(31), IntLiteral.create(63))
        expr = SubOperator(arg1, arg2).simplify()
        self.assertIntLiteral(expr, 143)

    def test_zero(self):
        '''Test simplification of zero literal terms.'''
        zero = IntLiteral.create(0)
        addr = LocalValue('A', IntType(16))
        leftZero = SubOperator(zero, addr)
        self.assertEqual(type(leftZero.simplify()), Complement)
        self.assertIs(SubOperator(addr, zero).simplify(), addr)
        self.assertIntLiteral(SubOperator(zero, zero).simplify(), 0)

    def test_self(self):
        '''Test simplification of subtracting an expression from itself.'''
        a = LocalValue('A', IntType(16))
        b = LocalValue('B', IntType(8))
        self.assertIntLiteral(SubOperator(a, a).simplify(), 0)
        c = AddOperator(a, b)
        d = AddOperator(b, a)
        self.assertIntLiteral(SubOperator(c, c).simplify(), 0)
        self.assertIntLiteral(SubOperator(c, d).simplify(), 0)
        e = Concatenation(a, b)
        self.assertIntLiteral(SubOperator(e, e).simplify(), 0)

class ComplementTests(TestUtils):

    def test_int(self):
        '''Takes the complement of an integer literal.'''
        expr = Complement(IntLiteral.create(4))
        self.assertIntLiteral(expr.simplify(), -4)

    def test_twice(self):
        '''Takes the complement of a complement.'''
        addr = LocalValue('A', IntType(16))
        self.assertIs(Complement(Complement(addr)).simplify(), addr)

class ArithmeticTests(TestUtils):

    def test_int(self):
        '''Uses add/sub/complement on several integer literals.'''
        expr = AddOperator(
            SubOperator(IntLiteral.create(0), IntLiteral.create(39)),
            Complement(
                SubOperator(IntLiteral.create(101), IntLiteral.create(1001))
                )
            )
        self.assertIntLiteral(expr.simplify(), 861)

    def test_associative(self):
        '''Test simplification using the associativity of addition.
        Note that that associativity cannot be exploited unless the subtraction
        is converted into an addition first.
        '''
        addr = LocalValue('A', IntType(16))
        arg1 = SubOperator(addr, IntLiteral.create(1))
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
        self.assertIs(SubOperator(arg1, arg2).simplify(), addr)

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

    def test_associative(self):
        '''Test simplification using the associativity of concatenation.
        '''
        addr = LocalValue('A', IntType(16))
        arg1 = Concatenation(addr, addr) # (A ; A)
        arg2 = Concatenation(arg1, arg1) # ((A ; A) ; (A ; A))
        arg3 = Concatenation(arg1, arg2) # ((A ; A) ; ((A ; A) ; (A ; A)))
        expr = arg3.simplify()
        self.assertIs(type(expr), Concatenation)
        self.assertIs(type(expr.type), IntType)
        self.assertEqual(expr.width, 6 * 16)
        self.assertEqual(str(expr), '(A ; A ; A ; A ; A ; A)')

    def test_associative2(self):
        '''Test simplification using the associativity of concatenation.
        '''
        addr = LocalValue('A', IntType(16))
        arg1 = Concatenation(addr, IntLiteral(0x9, IntType(4))) # (A ; $9)
        arg2 = Concatenation(IntLiteral(0x63, IntType(8)), addr) # ($63 ; A)
        arg3 = Concatenation(arg1, arg2) # ((A ; $9) ; ($63 ; A))
        expr = arg3.simplify()
        self.assertIs(type(expr), Concatenation)
        self.assertIs(type(expr.type), IntType)
        self.assertEqual(expr.width, 2 * 16 + 12)
        self.assertEqual(str(expr), '(A ; $963 ; A)')

if __name__ == '__main__':
    unittest.main()
