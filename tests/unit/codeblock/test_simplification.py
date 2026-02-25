from __future__ import annotations

from pytest import fixture

from retroasm.expression import IntLiteral
from retroasm.types import IntType

from .utils import TestNamespace, assert_ret_val


@fixture
def namespace() -> TestNamespace:
    return TestNamespace()


def test_ret_bits_override(namespace: TestNamespace) -> None:
    """Test code block creation with a non-default returned bit string."""
    ref_v = namespace.add_variable("V", IntType.u(20))
    value = IntLiteral(604)
    namespace.emit_store(ref_v, value)

    code = namespace.create_code_block(ref_v)
    assert len(code.returned) == 1
    (ret_bits,) = code.returned
    assert ret_bits.width == 20
    assert_ret_val(code, 604)
