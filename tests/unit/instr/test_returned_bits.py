from __future__ import annotations

from retroasm.codeblock_builder import SemanticsCodeBlockBuilder, returned_bits
from retroasm.expression import IntLiteral
from retroasm.reference import FixedValue, Reference, Variable
from retroasm.types import IntType

from ..expression.utils import assert_int_literal


def test_ret_bits_override() -> None:
    """A returned bit string doesn't have to be named "ret"."""

    ref_v = Reference(Variable("V", 20), IntType.u(20))

    builder = SemanticsCodeBlockBuilder()
    ref_v.emit_store(builder, IntLiteral(604), None)
    code = builder.create_code_block(returned_bits(ref_v))

    assert len(code.returned) == 1
    (ret_bits,) = code.returned
    assert isinstance(ret_bits, FixedValue)
    expr, width = ret_bits.expr, ret_bits.width
    assert_int_literal(expr, 604)
    assert width == 20
