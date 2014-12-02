from retroasm.expression import (
    AddOperator, Complement, IntLiteral, IntType, LocalValue, SubOperator
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

if __name__ == '__main__':
    unittest.main()
