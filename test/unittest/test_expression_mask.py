from utils_expression import TestValue

from retroasm.expression import (
    AddOperator, AndOperator, Complement, IntLiteral, LShift, OrOperator,
    RShift, XorOperator
    )
from retroasm.types import IntType, unlimited

import unittest

class MaskTests(unittest.TestCase):

    def test_int(self):
        '''Checks mask for integer literals.'''
        self.assertEqual(IntLiteral(0).mask, 0)
        self.assertEqual(IntLiteral(1).mask, 1)
        self.assertEqual(IntLiteral(123).mask, 123)
        self.assertEqual(IntLiteral(-1).mask, -1)
        self.assertEqual(IntLiteral(-9).mask, -9)

    def test_width(self):
        '''Checks mask computed from value width.'''
        v8 = TestValue('A', IntType(8))
        v16 = TestValue('B', IntType(16))
        self.assertEqual(v8.mask, 0xFF)
        self.assertEqual(v16.mask, 0xFFFF)

    def test_and(self):
        '''Checks mask for bitwise AND.'''
        v8 = TestValue('A', IntType(8))
        v16 = TestValue('B', IntType(16))
        litval = 0x1234
        lit = IntLiteral(litval)
        self.assertEqual(AndOperator(v8, v8).mask, 0xFF)
        self.assertEqual(AndOperator(v16, v16).mask, 0xFFFF)
        self.assertEqual(AndOperator(lit, lit).mask, litval)
        self.assertEqual(AndOperator(v8, v16).mask, 0xFF)
        self.assertEqual(AndOperator(v8, lit).mask, litval & 0xFF)
        self.assertEqual(AndOperator(v16, lit).mask, litval)
        self.assertEqual(AndOperator(v8, v16, lit).mask, litval & 0xFF)

    def test_or(self):
        '''Checks mask for bitwise OR.'''
        v8 = TestValue('A', IntType(8))
        v16 = TestValue('B', IntType(16))
        litval = 0x1234
        lit = IntLiteral(litval)
        self.assertEqual(OrOperator(v8, v8).mask, 0xFF)
        self.assertEqual(OrOperator(v16, v16).mask, 0xFFFF)
        self.assertEqual(OrOperator(lit, lit).mask, litval)
        self.assertEqual(OrOperator(v8, v16).mask, 0xFFFF)
        self.assertEqual(OrOperator(v8, lit).mask, litval | 0xFF)
        self.assertEqual(OrOperator(v16, lit).mask, 0xFFFF)
        self.assertEqual(OrOperator(v8, v16, lit).mask, 0xFFFF)

    def test_xor(self):
        '''Checks mask for bitwise XOR.'''
        v8 = TestValue('A', IntType(8))
        v16 = TestValue('B', IntType(16))
        litval = 0x1234
        lit = IntLiteral(litval)
        self.assertEqual(XorOperator(v8, v8).mask, 0xFF)
        self.assertEqual(XorOperator(v16, v16).mask, 0xFFFF)
        self.assertEqual(XorOperator(v8, v16).mask, 0xFFFF)
        self.assertEqual(XorOperator(v8, lit).mask, litval | 0xFF)
        self.assertEqual(XorOperator(v16, lit).mask, 0xFFFF)
        self.assertEqual(XorOperator(v8, v16, lit).mask, 0xFFFF)

    def test_add(self):
        '''Checks mask for addition.'''
        v8 = TestValue('A', IntType(8))
        v16 = TestValue('B', IntType(16))
        vu = TestValue('C', IntType(unlimited))
        self.assertEqual(AddOperator(v8, v8).mask, 0x1FF)
        self.assertEqual(AddOperator(v16, v16).mask, 0x1FFFF)
        self.assertEqual(AddOperator(v8, v16).mask, 0x1FFFF)
        self.assertEqual(AddOperator(v8, v8, IntLiteral(1)).mask, 0x1FF)
        self.assertEqual(AddOperator(v8, vu).mask, -1)

    def test_complement(self):
        '''Checks mask for complement.'''
        v8 = TestValue('A', IntType(8))
        self.assertEqual(Complement(v8).mask, -1)
        self.assertEqual(Complement(IntLiteral(-123)).mask, -1)
        self.assertEqual(Complement(IntLiteral(0)).mask, 0)
        self.assertEqual(Complement(IntLiteral(68)).mask, -4)
        self.assertEqual(Complement(IntLiteral(-68)).mask, -4)

    def test_lshift(self):
        '''Checks mask for left shift.'''
        v8 = TestValue('A', IntType(8))
        self.assertEqual(LShift(v8, 0).mask, 0xFF)
        self.assertEqual(LShift(v8, 3).mask, 0x7F8)
        self.assertEqual(LShift(v8, 8).mask, 0xFF00)
        self.assertEqual(LShift(IntLiteral(0x1234), 12).mask, 0x1234000)

    def test_rshift(self):
        '''Checks mask for right shift.'''
        v8 = TestValue('A', IntType(8))
        self.assertEqual(RShift(v8, 0).mask, 0xFF)
        self.assertEqual(RShift(v8, 3).mask, 0x1F)
        self.assertEqual(RShift(v8, 8).mask, 0)
        self.assertEqual(RShift(IntLiteral(0x12345678), 12).mask, 0x12345)

if __name__ == '__main__':
    unittest.main()
