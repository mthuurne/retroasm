from utils_expression import TestValue

from retroasm.expression import (
    AddOperator, AndOperator, Complement, IntLiteral, LShift, LVShift,
    OrOperator, RShift, RVShift, XorOperator
    )
from retroasm.types import IntType


def test_int():
    """Checks mask for integer literals."""
    assert IntLiteral(0).mask == 0
    assert IntLiteral(1).mask == 1
    assert IntLiteral(123).mask == 123
    assert IntLiteral(-1).mask == -1
    assert IntLiteral(-9).mask == -9

def test_width():
    """Checks mask computed from value width."""
    v8 = TestValue('A', IntType.u(8))
    v16 = TestValue('B', IntType.u(16))
    assert v8.mask == 0xFF
    assert v16.mask == 0xFFFF

def test_and():
    """Checks mask for bitwise AND."""
    v8 = TestValue('A', IntType.u(8))
    v16 = TestValue('B', IntType.u(16))
    litval = 0x1234
    lit = IntLiteral(litval)
    assert AndOperator(v8, v8).mask == 0xFF
    assert AndOperator(v16, v16).mask == 0xFFFF
    assert AndOperator(lit, lit).mask == litval
    assert AndOperator(v8, v16).mask == 0xFF
    assert AndOperator(v8, lit).mask == litval & 0xFF
    assert AndOperator(v16, lit).mask == litval
    assert AndOperator(v8, v16, lit).mask == litval & 0xFF

def test_or():
    """Checks mask for bitwise OR."""
    v8 = TestValue('A', IntType.u(8))
    v16 = TestValue('B', IntType.u(16))
    litval = 0x1234
    lit = IntLiteral(litval)
    assert OrOperator(v8, v8).mask == 0xFF
    assert OrOperator(v16, v16).mask == 0xFFFF
    assert OrOperator(lit, lit).mask == litval
    assert OrOperator(v8, v16).mask == 0xFFFF
    assert OrOperator(v8, lit).mask == litval | 0xFF
    assert OrOperator(v16, lit).mask == 0xFFFF
    assert OrOperator(v8, v16, lit).mask == 0xFFFF

def test_xor():
    """Checks mask for bitwise XOR."""
    v8 = TestValue('A', IntType.u(8))
    v16 = TestValue('B', IntType.u(16))
    litval = 0x1234
    lit = IntLiteral(litval)
    assert XorOperator(v8, v8).mask == 0xFF
    assert XorOperator(v16, v16).mask == 0xFFFF
    assert XorOperator(v8, v16).mask == 0xFFFF
    assert XorOperator(v8, lit).mask == litval | 0xFF
    assert XorOperator(v16, lit).mask == 0xFFFF
    assert XorOperator(v8, v16, lit).mask == 0xFFFF

def test_add():
    """Checks mask for addition."""
    v8 = TestValue('A', IntType.u(8))
    v16 = TestValue('B', IntType.u(16))
    vu = TestValue('C', IntType.int)
    assert AddOperator(v8, v8).mask == 0x1FF
    assert AddOperator(v16, v16).mask == 0x1FFFF
    assert AddOperator(v8, v16).mask == 0x1FFFF
    assert AddOperator(v8, v8, IntLiteral(1)).mask == 0x1FF
    assert AddOperator(v8, vu).mask == -1
    assert AddOperator(IntLiteral(0xFC00), IntLiteral(0x300)).mask == 0xFF00
    assert AddOperator(IntLiteral(0xFF00), IntLiteral(0x300)).mask == 0x1FF00
    assert AddOperator(IntLiteral(0xFF00FF), IntLiteral(0xFF00FF)).mask == 0x1FF01FF

def test_complement():
    """Checks mask for complement."""
    v8 = TestValue('A', IntType.u(8))
    assert Complement(v8).mask == -1
    assert Complement(IntLiteral(-123)).mask == -1
    assert Complement(IntLiteral(0)).mask == 0
    assert Complement(IntLiteral(68)).mask == -4
    assert Complement(IntLiteral(-68)).mask == -4

def test_lshift():
    """Checks mask for left shift."""
    v8 = TestValue('A', IntType.u(8))
    assert LShift(v8, 0).mask == 0xFF
    assert LShift(v8, 3).mask == 0x7F8
    assert LShift(v8, 8).mask == 0xFF00
    assert LShift(IntLiteral(0x1234), 12).mask == 0x1234000

def test_rshift():
    """Checks mask for right shift."""
    v8 = TestValue('A', IntType.u(8))
    assert RShift(v8, 0).mask == 0xFF
    assert RShift(v8, 3).mask == 0x1F
    assert RShift(v8, 8).mask == 0
    assert RShift(IntLiteral(0x12345678), 12).mask == 0x12345

def test_lvshift():
    """Checks mask for left variable shift."""
    v8 = TestValue('A', IntType.u(8))
    v1 = TestValue('B', IntType.u(1))
    i = TestValue('I', IntType.int)
    # Test expr or offset mask 0.
    assert LVShift(v8, IntLiteral(0)).mask == 0xFF
    assert LVShift(IntLiteral(0), v8).mask == 0
    assert LVShift(IntLiteral(0), IntLiteral(0)).mask == 0
    # Test generic variable mask.
    assert LVShift(v8, TestValue('C', IntType.u(3))).mask == 0x7FFF
    # Test expr masks with gaps.
    assert LVShift(OrOperator(v1, LShift(v1, 4)), v1).mask == 0x33
    assert LVShift(OrOperator(LShift(v1, 2), LShift(v1, 4)), v1).mask == 0x3C
    # Test offset masks with gaps.
    assert LVShift(v8, OrOperator(v1, LShift(v1, 4))).mask == 0x1FF01FF
    assert LVShift(v8, OrOperator(LShift(v1, 2), LShift(v1, 4))).mask == 0xFFF0FFF
    # Test gaps on both args.
    assert LVShift(
            OrOperator(LShift(v1, 2), LShift(v1, 7)),
            OrOperator(LShift(v1, 2), LShift(v1, 4))
            ).mask == 0x8C408C4
    # Test negative expression mask.
    assert LVShift(i, v1).mask == -1
    assert LVShift(LShift(i, 4), v1).mask == -1 << 4
    assert LVShift(
            OrOperator(LShift(i, 32), v8),
            LShift(v1, 4)
            ).mask == (-1 << 32) | 0xFF00FF
    # Test negative offset mask.
    assert LVShift(v8, i).mask == -1
    assert LVShift(v8, LShift(i, 4)).mask == (-1 << 16) | 0xFF
    assert LVShift(LShift(v8, 16), LShift(i, 4)).mask == (-1 << 32) | 0xFF0000
    assert LVShift(IntLiteral(-160), i).mask == -1 << 5

def test_rvshift():
    """Checks mask for right variable shift."""
    v8 = TestValue('A', IntType.u(8))
    v1 = TestValue('B', IntType.u(1))
    i = TestValue('I', IntType.int)
    # Test expr or offset mask 0.
    assert RVShift(v8, IntLiteral(0)).mask == 0xFF
    assert RVShift(IntLiteral(0), v8).mask == 0
    assert RVShift(IntLiteral(0), IntLiteral(0)).mask == 0
    # Test generic variable mask.
    assert RVShift(v8, TestValue('C', IntType.u(3))).mask == 0xFF
    # Test expr masks with gaps.
    assert RVShift(OrOperator(v1, LShift(v1, 4)), v1).mask == 0x19
    assert RVShift(OrOperator(LShift(v1, 2), LShift(v1, 4)), v1).mask == 0x1E
    # Test offset masks with gaps.
    assert RVShift(LShift(v8, 20), OrOperator(v1, LShift(v1, 4))).mask == 0xFF80FF8
    assert RVShift(LShift(v8, 16), OrOperator(v1, LShift(v1, 4))).mask == 0xFF80FF
    # Test gaps on both args.
    assert RVShift(
            OrOperator(LShift(v1, 12), LShift(v1, 15)),
            OrOperator(LShift(v1, 1), LShift(v1, 3))
            ).mask == 0xB4B4
    # Test negative offset mask.
    assert RVShift(v8, i).mask == 0xFF
    assert RVShift(LShift(v8, 4), LShift(i, 8)).mask == 0xFF0
    assert RVShift(LShift(v8, 16), LShift(i, 4)).mask == 0xFF00FF
    assert RVShift(IntLiteral(-160), i).mask == -1
