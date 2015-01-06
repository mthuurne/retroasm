from retroasm.expression import Concatenation, IntLiteral, Slice
from retroasm.storage import Variable
from retroasm.types import IntType, unlimited

import unittest

class EqualsTests(unittest.TestCase):

    def assertExprEqual(self, expr1, expr2):
        self.assertEqual(expr1, expr2)
        self.assertEqual(expr2, expr1)
        self.assertIs(expr1 == expr2, True)
        self.assertIs(expr2 == expr1, True)
        self.assertIs(expr1 != expr2, False)
        self.assertIs(expr2 != expr1, False)

    def assertExprNotEqual(self, expr1, expr2):
        self.assertNotEqual(expr1, expr2)
        self.assertNotEqual(expr2, expr1)
        self.assertIs(expr1 == expr2, False)
        self.assertIs(expr2 == expr1, False)
        self.assertIs(expr1 != expr2, True)
        self.assertIs(expr2 != expr1, True)

    def test_int(self):
        '''Checks unlimited-width integer literals for equality.'''
        arg1a = IntLiteral.create(1)
        arg1b = IntLiteral.create(1)
        arg2 = IntLiteral.create(2)
        self.assertExprEqual(arg1a, arg1b)
        self.assertExprNotEqual(arg1a, arg2)
        self.assertExprEqual(arg2, arg2)

    def test_fixed_width(self):
        '''Checks fixed-width integer literals for equality.'''
        arg1 = IntLiteral(6, IntType(4))
        arg2 = IntLiteral(6, IntType(8))
        arg3 = IntLiteral(6, IntType(unlimited))
        self.assertExprEqual(arg1, arg1)
        self.assertExprEqual(arg2, arg2)
        self.assertExprEqual(arg3, arg3)
        self.assertExprEqual(arg1, arg2)
        self.assertExprEqual(arg2, arg3)
        self.assertExprEqual(arg1, arg3)

    def test_variable(self):
        '''Checks variables for equality.'''
        a = Variable('A', IntType(16))
        b = Variable('B', IntType(8))
        self.assertExprEqual(a, a)
        self.assertExprEqual(b, b)
        self.assertExprNotEqual(a, b)

    def test_type_mismatch(self):
        '''Checks equality checks between mismatching types.'''
        zero = IntLiteral.create(0)
        addr = Variable('A', IntType(16))
        self.assertExprNotEqual(zero, 0)
        self.assertExprNotEqual(addr, 'A')
        self.assertExprNotEqual(zero, addr)

    def test_concat_width(self):
        '''Checks equality between equal concatenations of different width.'''
        four_int = IntLiteral.create(4)
        four_u4 = IntLiteral(4, IntType(4))
        four_u8 = IntLiteral(4, IntType(8))
        addr = IntLiteral(0xACDC, IntType(16))
        cat_int_addr = Concatenation(four_int, addr)
        cat_u4_addr = Concatenation(four_u4, addr)
        cat_u8_addr = Concatenation(four_u8, addr)
        # Test expression being equal to itself.
        self.assertExprEqual(cat_int_addr, cat_int_addr)
        self.assertExprEqual(cat_u4_addr, cat_u4_addr)
        self.assertExprEqual(cat_u8_addr, cat_u8_addr)
        # Test that first term width is ignored.
        self.assertExprEqual(cat_int_addr, cat_u4_addr)
        self.assertExprEqual(cat_int_addr, cat_u8_addr)
        self.assertExprEqual(cat_u4_addr, cat_u8_addr)

    def test_concat_internal(self):
        '''Checks equality between different concatenations of equal width.'''
        four_int = IntLiteral.create(4)
        four_u4 = IntLiteral(4, IntType(4))
        four_u8 = IntLiteral(4, IntType(8))
        cat_u4_u8 = Concatenation(four_u4, four_u8)
        cat_u8_u4 = Concatenation(four_u8, four_u4)
        # Test expression being equal to itself.
        self.assertExprEqual(cat_u4_u8, cat_u4_u8)
        self.assertExprEqual(cat_u8_u4, cat_u8_u4)
        # Test that position of integer within concatenation is considered.
        self.assertExprEqual(cat_u4_u8.width, cat_u8_u4.width)
        self.assertExprNotEqual(cat_u4_u8, cat_u8_u4)

    def test_slice_subexpr(self):
        '''Checks equality between slices with differing subexpressions.'''
        slice1 = Slice(IntLiteral.create(0x1234), 4, 8)
        slice2 = Slice(IntLiteral.create(0x5678), 4, 8)
        self.assertExprEqual(slice1, slice1)
        self.assertExprEqual(slice2, slice2)
        self.assertExprNotEqual(slice1, slice2)

    def test_slice_index(self):
        '''Checks equality between slices with differing indices.'''
        slice1 = Slice(IntLiteral.create(0x333), 0, 8)
        slice2 = Slice(IntLiteral.create(0x333), 4, 8)
        self.assertExprEqual(slice1, slice1)
        self.assertExprEqual(slice2, slice2)
        self.assertExprNotEqual(slice1, slice2)

    def test_slice_width(self):
        '''Checks equality between slices with differing widths.'''
        addr = IntLiteral(0x4567, IntType(16))
        slice1 = Slice(addr, 4, 8)  # $56
        slice2 = Slice(addr, 4, 12) # $456
        slice3 = Slice(addr, 4, 16) # $0456
        self.assertExprEqual(slice1, slice1)
        self.assertExprEqual(slice2, slice2)
        self.assertExprEqual(slice3, slice3)
        self.assertExprNotEqual(slice1, slice2)
        self.assertExprNotEqual(slice1, slice3)
        self.assertExprNotEqual(slice2, slice3)

if __name__ == '__main__':
    unittest.main()
