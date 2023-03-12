from __future__ import annotations

from retroasm.expression import RShift, truncate
from retroasm.expression_simplifier import simplify_expression
from retroasm.types import IntType

from .utils_expression import TestValue


def test_slice() -> None:
    """Formats slice expressions."""
    addr = TestValue("A", IntType.u(16))
    # Truncation and shift in isolation.
    assert str(simplify_expression(truncate(addr, 8))) == "A[:8]"
    assert str(simplify_expression(RShift(addr, 8))) == "A[8:]"
    # Truncation and shift combined.
    assert str(simplify_expression(truncate(RShift(addr, 0), 8))) == "A[:8]"
    assert str(simplify_expression(truncate(RShift(addr, 4), 8))) == "A[4:12]"
    assert str(simplify_expression(truncate(RShift(addr, 8), 8))) == "A[8:]"


def test_index() -> None:
    """Formats bit index expressions."""
    addr = TestValue("A", IntType.u(16))
    assert str(simplify_expression(truncate(addr, 1))) == "A[0]"
    assert str(simplify_expression(truncate(RShift(addr, 11), 1))) == "A[11]"
    assert str(simplify_expression(RShift(addr, 15))) == "A[15]"
