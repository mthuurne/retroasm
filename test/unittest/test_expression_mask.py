from utils_expression import TestValue

from retroasm.expression import (
    AddOperator, AndOperator, Complement, IntLiteral, LShift, LVShift,
    OrOperator, RShift, RVShift, XorOperator
    )
from retroasm.types import IntType

import unittest

class ExpressionMaskTests(unittest.TestCase):

    def test_int(self):
        '''Checks mask for integer literals.'''
        self.assertEqual(IntLiteral(0).mask, 0)
        self.assertEqual(IntLiteral(1).mask, 1)
        self.assertEqual(IntLiteral(123).mask, 123)
        self.assertEqual(IntLiteral(-1).mask, -1)
        self.assertEqual(IntLiteral(-9).mask, -9)

    def test_width(self):
        '''Checks mask computed from value width.'''
        v8 = TestValue('A', IntType.u(8))
        v16 = TestValue('B', IntType.u(16))
        self.assertEqual(v8.mask, 0xFF)
        self.assertEqual(v16.mask, 0xFFFF)

    def test_and(self):
        '''Checks mask for bitwise AND.'''
        v8 = TestValue('A', IntType.u(8))
        v16 = TestValue('B', IntType.u(16))
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
        v8 = TestValue('A', IntType.u(8))
        v16 = TestValue('B', IntType.u(16))
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
        v8 = TestValue('A', IntType.u(8))
        v16 = TestValue('B', IntType.u(16))
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
        v8 = TestValue('A', IntType.u(8))
        v16 = TestValue('B', IntType.u(16))
        vu = TestValue('C', IntType.int)
        self.assertEqual(AddOperator(v8, v8).mask, 0x1FF)
        self.assertEqual(AddOperator(v16, v16).mask, 0x1FFFF)
        self.assertEqual(AddOperator(v8, v16).mask, 0x1FFFF)
        self.assertEqual(AddOperator(v8, v8, IntLiteral(1)).mask, 0x1FF)
        self.assertEqual(AddOperator(v8, vu).mask, -1)
        self.assertEqual(
            AddOperator(IntLiteral(0xFC00), IntLiteral(0x300)).mask,
            0xFF00
            )
        self.assertEqual(
            AddOperator(IntLiteral(0xFF00), IntLiteral(0x300)).mask,
            0x1FF00
            )
        self.assertEqual(
            AddOperator(IntLiteral(0xFF00FF), IntLiteral(0xFF00FF)).mask,
            0x1FF01FF
            )

    def test_complement(self):
        '''Checks mask for complement.'''
        v8 = TestValue('A', IntType.u(8))
        self.assertEqual(Complement(v8).mask, -1)
        self.assertEqual(Complement(IntLiteral(-123)).mask, -1)
        self.assertEqual(Complement(IntLiteral(0)).mask, 0)
        self.assertEqual(Complement(IntLiteral(68)).mask, -4)
        self.assertEqual(Complement(IntLiteral(-68)).mask, -4)

    def test_lshift(self):
        '''Checks mask for left shift.'''
        v8 = TestValue('A', IntType.u(8))
        self.assertEqual(LShift(v8, 0).mask, 0xFF)
        self.assertEqual(LShift(v8, 3).mask, 0x7F8)
        self.assertEqual(LShift(v8, 8).mask, 0xFF00)
        self.assertEqual(LShift(IntLiteral(0x1234), 12).mask, 0x1234000)

    def test_rshift(self):
        '''Checks mask for right shift.'''
        v8 = TestValue('A', IntType.u(8))
        self.assertEqual(RShift(v8, 0).mask, 0xFF)
        self.assertEqual(RShift(v8, 3).mask, 0x1F)
        self.assertEqual(RShift(v8, 8).mask, 0)
        self.assertEqual(RShift(IntLiteral(0x12345678), 12).mask, 0x12345)

    def test_lvshift(self):
        '''Checks mask for left variable shift.'''
        v8 = TestValue('A', IntType.u(8))
        v1 = TestValue('B', IntType.u(1))
        i = TestValue('I', IntType.int)
        # Test expr or offset mask 0.
        self.assertEqual(LVShift(v8, IntLiteral(0)).mask, 0xFF)
        self.assertEqual(LVShift(IntLiteral(0), v8).mask, 0)
        self.assertEqual(LVShift(IntLiteral(0), IntLiteral(0)).mask, 0)
        # Test generic variable mask.
        self.assertEqual(
            LVShift(v8, TestValue('C', IntType.u(3))).mask,
            0x7FFF
            )
        # Test expr masks with gaps.
        self.assertEqual(
            LVShift(OrOperator(v1, LShift(v1, 4)), v1).mask,
            0x33
            )
        self.assertEqual(
            LVShift(OrOperator(LShift(v1, 2), LShift(v1, 4)), v1).mask,
            0x3C
            )
        # Test offset masks with gaps.
        self.assertEqual(
            LVShift(v8, OrOperator(v1, LShift(v1, 4))).mask,
            0x1FF01FF
            )
        self.assertEqual(
            LVShift(v8, OrOperator(LShift(v1, 2), LShift(v1, 4))).mask,
            0xFFF0FFF
            )
        # Test gaps on both args.
        self.assertEqual(
            LVShift(
                OrOperator(LShift(v1, 2), LShift(v1, 7)),
                OrOperator(LShift(v1, 2), LShift(v1, 4))
                ).mask,
            0x8C408C4
            )
        # Test negative expression mask.
        self.assertEqual(LVShift(i, v1).mask, -1)
        self.assertEqual(LVShift(LShift(i, 4), v1).mask, -1 << 4)
        self.assertEqual(
            LVShift(OrOperator(LShift(i, 32), v8), LShift(v1, 4)).mask,
            (-1 << 32) | 0xFF00FF
            )
        # Test negative offset mask.
        self.assertEqual(LVShift(v8, i).mask, -1)
        self.assertEqual(LVShift(v8, LShift(i, 4)).mask, (-1 << 16) | 0xFF)
        self.assertEqual(
            LVShift(LShift(v8, 16), LShift(i, 4)).mask,
            (-1 << 32) | 0xFF0000
            )
        self.assertEqual(LVShift(IntLiteral(-160), i).mask, -1 << 5)

    def test_rvshift(self):
        '''Checks mask for right variable shift.'''
        v8 = TestValue('A', IntType.u(8))
        v1 = TestValue('B', IntType.u(1))
        i = TestValue('I', IntType.int)
        # Test expr or offset mask 0.
        self.assertEqual(RVShift(v8, IntLiteral(0)).mask, 0xFF)
        self.assertEqual(RVShift(IntLiteral(0), v8).mask, 0)
        self.assertEqual(RVShift(IntLiteral(0), IntLiteral(0)).mask, 0)
        # Test generic variable mask.
        self.assertEqual(
            RVShift(v8, TestValue('C', IntType.u(3))).mask,
            0xFF
            )
        # Test expr masks with gaps.
        self.assertEqual(
            RVShift(OrOperator(v1, LShift(v1, 4)), v1).mask,
            0x19
            )
        self.assertEqual(
            RVShift(OrOperator(LShift(v1, 2), LShift(v1, 4)), v1).mask,
            0x1E
            )
        # Test offset masks with gaps.
        self.assertEqual(
            RVShift(LShift(v8, 20), OrOperator(v1, LShift(v1, 4))).mask,
            0xFF80FF8
            )
        self.assertEqual(
            RVShift(LShift(v8, 16), OrOperator(v1, LShift(v1, 4))).mask,
            0xFF80FF
            )
        # Test gaps on both args.
        self.assertEqual(
            RVShift(
                OrOperator(LShift(v1, 12), LShift(v1, 15)),
                OrOperator(LShift(v1, 1), LShift(v1, 3))
                ).mask,
            0xB4B4
            )
        # Test negative offset mask.
        self.assertEqual(RVShift(v8, i).mask, 0xFF)
        self.assertEqual(RVShift(LShift(v8, 4), LShift(i, 8)).mask, 0xFF0)
        self.assertEqual(RVShift(LShift(v8, 16), LShift(i, 4)).mask, 0xFF00FF)
        self.assertEqual(RVShift(IntLiteral(-160), i).mask, -1)

if __name__ == '__main__':
    unittest.main()
