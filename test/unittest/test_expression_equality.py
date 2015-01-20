from utils_expression import TestValue

from retroasm.expression import IntLiteral, Truncation, concatenate
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

    def test_type_mismatch(self):
        '''Checks equality checks between mismatching types.'''
        zero = IntLiteral.create(0)
        addr = TestValue('A', IntType(16))
        self.assertExprNotEqual(zero, 0)
        self.assertExprNotEqual(addr, 'A')
        self.assertExprNotEqual(zero, addr)

    def test_concat_width(self):
        '''Checks equality between equal concatenations of different width.'''
        four_int = IntLiteral.create(4)
        four_u4 = IntLiteral(4, IntType(4))
        four_u8 = IntLiteral(4, IntType(8))
        addr = IntLiteral(0xACDC, IntType(16))
        cat_int_addr = concatenate(four_int, addr)
        cat_u4_addr = concatenate(four_u4, addr)
        cat_u8_addr = concatenate(four_u8, addr)
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
        cat_u4_u8 = concatenate(four_u4, four_u8)
        cat_u8_u4 = concatenate(four_u8, four_u4)
        # Test expression being equal to itself.
        self.assertExprEqual(cat_u4_u8, cat_u4_u8)
        self.assertExprEqual(cat_u8_u4, cat_u8_u4)
        # Test that position of integer within concatenation is considered.
        self.assertExprEqual(cat_u4_u8.width, cat_u8_u4.width)
        self.assertExprNotEqual(cat_u4_u8, cat_u8_u4)

    def test_truncate_subexpr(self):
        '''Checks equality between truncations with differing subexpressions.'''
        trunc1 = Truncation(IntLiteral.create(0x1234), 8)
        trunc2 = Truncation(IntLiteral.create(0x5678), 8)
        self.assertExprEqual(trunc1, trunc1)
        self.assertExprEqual(trunc2, trunc2)
        self.assertExprNotEqual(trunc1, trunc2)

    def test_truncate_width(self):
        '''Checks equality between truncations with differing widths.'''
        addr = IntLiteral(0x456, IntType(12))
        trunc1 = Truncation(addr, 8)  # $56
        trunc2 = Truncation(addr, 12) # $456
        trunc3 = Truncation(addr, 16) # $0456
        self.assertExprEqual(trunc1, trunc1)
        self.assertExprEqual(trunc2, trunc2)
        self.assertExprEqual(trunc3, trunc3)
        self.assertExprNotEqual(trunc1, trunc2)
        self.assertExprNotEqual(trunc1, trunc3)
        self.assertExprNotEqual(trunc2, trunc3)

if __name__ == '__main__':
    unittest.main()
