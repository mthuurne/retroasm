from utils_expression import TestValue

from retroasm.expression import IntLiteral, LShift, OrOperator, truncate
from retroasm.types import IntType

import unittest

def makeConcat(exprH, exprL, widthL):
    return OrOperator(exprL, LShift(exprH, widthL))

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
        '''Checks integer literals for equality.'''
        arg1a = IntLiteral(1)
        arg1b = IntLiteral(1)
        arg2 = IntLiteral(2)
        self.assertExprEqual(arg1a, arg1b)
        self.assertExprNotEqual(arg1a, arg2)
        self.assertExprEqual(arg2, arg2)

    def test_type_mismatch(self):
        '''Checks equality checks between mismatching types.'''
        zero = IntLiteral(0)
        addr = TestValue('A', IntType.u(16))
        self.assertExprNotEqual(zero, 0)
        self.assertExprNotEqual(addr, 'A')
        self.assertExprNotEqual(zero, addr)

    def test_concat_internal(self):
        '''Checks equality between different concatenations of equal width.'''
        four = IntLiteral(4)
        cat_u4_u8 = makeConcat(four, four, 8)
        cat_u8_u4 = makeConcat(four, four, 4)
        # Test expression being equal to itself.
        self.assertExprEqual(cat_u4_u8, cat_u4_u8)
        self.assertExprEqual(cat_u8_u4, cat_u8_u4)
        # Test that position of integer within concatenation is considered.
        self.assertExprNotEqual(cat_u4_u8, cat_u8_u4)

    def test_truncate_subexpr(self):
        '''Checks equality between truncations with differing subexpressions.'''
        trunc1 = truncate(IntLiteral(0x1234), 8)
        trunc2 = truncate(IntLiteral(0x5678), 8)
        self.assertExprEqual(trunc1, trunc1)
        self.assertExprEqual(trunc2, trunc2)
        self.assertExprNotEqual(trunc1, trunc2)

    def test_truncate_width(self):
        '''Checks equality between truncations with differing widths.'''
        addr = IntLiteral(0x456)
        trunc1 = truncate(addr, 8)  # $56
        trunc2 = truncate(addr, 12) # $456
        trunc3 = truncate(addr, 16) # $0456
        self.assertExprEqual(trunc1, trunc1)
        self.assertExprEqual(trunc2, trunc2)
        self.assertExprEqual(trunc3, trunc3)
        self.assertExprNotEqual(trunc1, trunc2)
        self.assertExprNotEqual(trunc1, trunc3)
        self.assertExprNotEqual(trunc2, trunc3)

if __name__ == '__main__':
    unittest.main()
