from utils_expression import TestValue

from retroasm.expression import RShift, truncate
from retroasm.expression_simplifier import simplifyExpression
from retroasm.types import IntType


def test_slice():
    """Formats slice expressions."""
    addr = TestValue('A', IntType.u(16))
    # Truncation and shift in isolation.
    assert str(simplifyExpression(truncate(addr, 8))) == 'A[:8]'
    assert str(simplifyExpression(RShift(addr, 8))) == 'A[8:]'
    # Truncation and shift combined.
    assert str(simplifyExpression(truncate(RShift(addr, 0), 8))) == 'A[:8]'
    assert str(simplifyExpression(truncate(RShift(addr, 4), 8))) == 'A[4:12]'
    assert str(simplifyExpression(truncate(RShift(addr, 8), 8))) == 'A[8:]'

def test_index():
    """Formats bit index expressions."""
    addr = TestValue('A', IntType.u(16))
    assert str(simplifyExpression(truncate(addr, 1))) == 'A[0]'
    assert str(simplifyExpression(truncate(RShift(addr, 11), 1))) == 'A[11]'
    assert str(simplifyExpression(RShift(addr, 15))) == 'A[15]'
