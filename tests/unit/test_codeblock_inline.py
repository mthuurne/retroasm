from __future__ import annotations

from typing import Callable

from retroasm.codeblock import CodeBlock, Load, Store
from retroasm.expression import AddOperator, Expression, IntLiteral, XorOperator
from retroasm.function import Function
from retroasm.namespace import LocalNamespace
from retroasm.reference import (
    BitString,
    ConcatenatedBits,
    FixedValue,
    Reference,
    SingleStorage,
    SlicedBits,
)
from retroasm.types import IntType

from .utils_codeblock import TestNamespace, assert_nodes, assert_ret_val, get_ret_val

verbose = False


def create_simplified_code(namespace: LocalNamespace) -> CodeBlock:
    if verbose:
        print("=" * 40)
        namespace.dump()
    if "ret" in namespace:
        ret_ref = namespace.elements["ret"]
        assert isinstance(ret_ref, Reference), ret_ref
    else:
        ret_ref = None
    code = namespace.create_code_block(ret_ref)
    if verbose:
        print("-" * 40)
        code.dump()
    return code


def args(**kvargs: BitString) -> Callable[[str], BitString]:
    """Argument fetcher helper function for inlineBlock()."""
    return kvargs.__getitem__


def test_inline_easy() -> None:
    """Test whether inlining works when there are no complications."""
    inner = TestNamespace()
    inner_a = inner.add_register("a", IntType.u(16))
    const = IntLiteral(12345)
    inner.emit_store(inner_a, const)

    # Share the global namespace to make sure that the outer and inner block
    # are using the same registers.
    outer = TestNamespace(inner)
    outer_a = outer.add_register("a", IntType.u(16))
    zero = IntLiteral(0)
    outer.emit_store(outer_a, zero)
    outer.inline_block(inner.create_code_block(None), args())
    load_a = outer.emit_load(outer_a)
    outer_ret = outer.add_variable("ret", IntType.u(16))
    outer.emit_store(outer_ret, load_a)

    code = create_simplified_code(outer)
    ret_val, ret_width = get_ret_val(code)
    correct = (Store(ret_val, outer_a.bits.storage),)
    assert_nodes(code.nodes, correct)
    assert_ret_val(code, 12345)
    assert ret_width == 16


def test_inline_arg_ret() -> None:
    """Test whether inlining works with an argument and return value."""
    inc = TestNamespace()
    inc_arg_ref = inc.add_argument("V")
    inc_arg_val = inc.emit_load(inc_arg_ref)
    inc_add = AddOperator(inc_arg_val, IntLiteral(1))
    inc_ret = inc.add_variable("ret")
    inc.emit_store(inc_ret, inc_add)
    inc_code = inc.create_code_block(inc_ret)

    outer = TestNamespace()

    def args_v(value: Expression) -> Callable[[str], BitString]:
        return args(V=FixedValue(value, 8))

    step0 = IntLiteral(100)
    (ret1,) = outer.inline_block(inc_code, args_v(step0))
    step1 = outer.emit_load(ret1)
    (ret2,) = outer.inline_block(inc_code, args_v(step1))
    step2 = outer.emit_load(ret2)
    (ret3,) = outer.inline_block(inc_code, args_v(step2))
    step3 = outer.emit_load(ret3)
    outer_ret = outer.add_variable("ret")
    outer.emit_store(outer_ret, step3)

    code = create_simplified_code(outer)
    correct = ()
    assert_nodes(code.nodes, correct)
    ret_val_, ret_width = get_ret_val(code)
    assert_ret_val(code, 103)
    assert ret_width == 8


def test_inline_multiret() -> None:
    """Test whether inlining works when "ret" is written multiple times."""
    inner = TestNamespace()
    val0 = IntLiteral(1000)
    val1 = IntLiteral(2000)
    val2 = IntLiteral(3000)
    inner_ret = inner.add_variable("ret", IntType.u(16))
    inner.emit_store(inner_ret, val0)
    inner.emit_store(inner_ret, val1)
    inner.emit_store(inner_ret, val2)
    inner_code = inner.create_code_block(inner_ret)

    outer = TestNamespace()
    (inlined_ret,) = outer.inline_block(inner_code, args())
    inlined_val = outer.emit_load(inlined_ret)
    outer_ret = outer.add_variable("ret", IntType.u(16))
    outer.emit_store(outer_ret, inlined_val)

    code = create_simplified_code(outer)
    correct = ()
    assert_nodes(code.nodes, correct)
    ret_val_, ret_width = get_ret_val(code)
    assert_ret_val(code, 3000)
    assert ret_width == 16


def test_ret_truncate() -> None:
    """Test whether the value returned by a block is truncated."""
    inner = TestNamespace()
    inner_val = IntLiteral(0x8472)
    inner_ret = inner.add_variable("ret")
    inner.emit_store(inner_ret, inner_val)
    inner_code = inner.create_code_block(inner_ret)
    func = Function(IntType.u(8), {}, inner_code)

    outer = TestNamespace()
    inline_ret = outer.inline_function_call(func, {})
    assert inline_ret is not None
    outer_val = outer.emit_load(inline_ret)
    outer_ret = outer.add_variable("ret", IntType.u(16))
    outer.emit_store(outer_ret, outer_val)

    code = create_simplified_code(outer)
    correct = ()
    assert_nodes(code.nodes, correct)
    ret_val_, ret_width = get_ret_val(code)
    assert_ret_val(code, 0x72)
    assert ret_width == 16


def test_pass_by_reference() -> None:
    """Test whether pass-by-reference arguments work correctly."""
    inc = TestNamespace()
    inc_arg_ref = inc.add_argument("R")
    inc_arg_val = inc.emit_load(inc_arg_ref)
    inc_add = AddOperator(inc_arg_val, IntLiteral(1))
    inc.emit_store(inc_arg_ref, inc_add)
    inc_code = inc.create_code_block(None)

    outer = TestNamespace()
    outer_a = outer.add_register("a")
    init_a = IntLiteral(100)
    outer.emit_store(outer_a, init_a)
    outer.inline_block(inc_code, args(R=outer_a.bits))
    outer.inline_block(inc_code, args(R=outer_a.bits))
    outer.inline_block(inc_code, args(R=outer_a.bits))
    outer_ret = outer.add_variable("ret")
    final_a = outer.emit_load(outer_a)
    outer.emit_store(outer_ret, final_a)

    code = create_simplified_code(outer)
    ret_val, ret_width = get_ret_val(code)
    correct = (Store(ret_val, outer_a.bits.storage),)
    assert_nodes(code.nodes, correct)
    assert_ret_val(code, 103)
    assert ret_width == 8


def test_pass_concat_by_reference() -> None:
    """Test concatenated storages as pass-by-reference arguments."""
    inc = TestNamespace()
    inc_arg_ref = inc.add_argument("R", IntType.u(16))
    inc_arg_val = inc.emit_load(inc_arg_ref)
    inc_add = AddOperator(inc_arg_val, IntLiteral(0x1234))
    inc.emit_store(inc_arg_ref, inc_add)
    inc_code = inc.create_code_block(None)

    outer = TestNamespace()
    outer_h = outer.add_register("h")
    outer_l = outer.add_register("l")
    bits_hl = ConcatenatedBits(outer_l.bits, outer_h.bits)

    init_h = IntLiteral(0xAB)
    init_l = IntLiteral(0xCD)
    outer.emit_store(outer_h, init_h)
    outer.emit_store(outer_l, init_l)
    outer.inline_block(inc_code, args(R=bits_hl))
    outer.inline_block(inc_code, args(R=bits_hl))
    outer.inline_block(inc_code, args(R=bits_hl))
    outer_ret = outer.add_variable("ret", IntType.u(16))
    final_hl = outer.emit_load(Reference(bits_hl, IntType.u(16)))
    outer.emit_store(outer_ret, final_hl)

    final_val = 0xABCD + 3 * 0x1234
    code = create_simplified_code(outer)
    correct = (
        Store(IntLiteral(final_val & 0xFF), outer_l.bits.storage),
        Store(IntLiteral(final_val >> 8), outer_h.bits.storage),
    )
    assert_nodes(code.nodes, correct)
    ret_val, ret_width = get_ret_val(code)
    assert_ret_val(code, final_val)
    assert ret_width == 16


def test_pass_concat_fixed_by_reference() -> None:
    """Test concatenated storages arguments containing fixed_values."""
    inc = TestNamespace()
    inc_arg_ref = inc.add_argument("R", IntType.u(16))
    inc_arg_val = inc.emit_load(inc_arg_ref)
    inc_add = AddOperator(inc_arg_val, IntLiteral(0x1234))
    inc.emit_store(inc_arg_ref, inc_add)
    inc_code = inc.create_code_block(None)

    outer = TestNamespace()
    outer_h = outer.add_register("h")
    outer_l = FixedValue(IntLiteral(0xCD), 8)
    bits_hl = ConcatenatedBits(outer_l, outer_h.bits)

    init_h = IntLiteral(0xAB)
    outer.emit_store(outer_h, init_h)
    outer.inline_block(inc_code, args(R=bits_hl))
    outer.inline_block(inc_code, args(R=bits_hl))
    outer.inline_block(inc_code, args(R=bits_hl))
    outer_ret = outer.add_variable("ret", IntType.u(16))
    final_hl = outer.emit_load(Reference(bits_hl, IntType.u(16)))
    outer.emit_store(outer_ret, final_hl)

    final_val = 0xABCD + 3 * 0x1300
    code = create_simplified_code(outer)
    correct = (Store(IntLiteral(final_val >> 8), outer_h.bits.storage),)
    assert_nodes(code.nodes, correct)
    ret_val, ret_width = get_ret_val(code)
    assert_ret_val(code, final_val)
    assert ret_width == 16


def test_pass_slice_by_reference() -> None:
    """Test sliced storages as pass-by-reference arguments."""
    inc = TestNamespace()
    inc_arg_ref = inc.add_argument("R")
    inc_arg_val = inc.emit_load(inc_arg_ref)
    inc_add = AddOperator(inc_arg_val, IntLiteral(0x12))
    inc.emit_store(inc_arg_ref, inc_add)
    inc_code = inc.create_code_block(None)

    outer = TestNamespace()
    outer_r = outer.add_register("r", IntType.u(16))
    init_r = IntLiteral(0xCDEF)
    outer.emit_store(outer_r, init_r)
    slice_r = SlicedBits(outer_r.bits, IntLiteral(4), 8)
    outer.inline_block(inc_code, args(R=slice_r))
    outer.inline_block(inc_code, args(R=slice_r))
    outer.inline_block(inc_code, args(R=slice_r))
    outer_ret = outer.add_variable("ret", IntType.u(16))
    final_r = outer.emit_load(outer_r)
    outer.emit_store(outer_ret, final_r)

    final_val = 0xC00F | (((0xDE + 3 * 0x12) & 0xFF) << 4)
    code = create_simplified_code(outer)
    correct = (Store(IntLiteral(final_val), outer_r.bits.storage),)
    assert_nodes(code.nodes, correct)
    ret_val, ret_width = get_ret_val(code)
    assert_ret_val(code, final_val)
    assert ret_width == 16


def test_inline_unsigned_reg() -> None:
    """Test reading of an unsigned register."""
    inner = TestNamespace()
    inner_a = inner.add_register("a")
    inner_load = inner.emit_load(inner_a)
    inner_ret = inner.add_variable("ret", IntType.u(16))
    inner.emit_store(inner_ret, inner_load)
    inner_code = inner.create_code_block(inner_ret)

    outer = TestNamespace(inner)
    outer_a = outer.add_register("a")
    init_a = IntLiteral(0xB2)
    outer.emit_store(outer_a, init_a)
    (ret_bits,) = outer.inline_block(inner_code, args())
    outer_ret = outer.add_variable("ret", IntType.u(16))
    ret_val = outer.emit_load(ret_bits)
    outer.emit_store(outer_ret, ret_val)

    final_val = 0x00B2
    code = create_simplified_code(outer)
    correct = (Store(init_a, outer_a.bits.storage),)
    assert_nodes(code.nodes, correct)
    assert_ret_val(code, final_val)


def test_inline_signed_reg() -> None:
    """Test reading of a signed register."""
    inner = TestNamespace()
    inner_a = inner.add_register("a", IntType.s(8))
    inner_load = inner.emit_load(inner_a)
    inner_ret = inner.add_variable("ret", IntType.u(16))
    inner.emit_store(inner_ret, inner_load)
    inner_code = inner.create_code_block(inner_ret)

    outer = TestNamespace(inner)
    outer_a = outer.add_register("a", IntType.s(8))
    init_a = IntLiteral(0xB2)
    outer.emit_store(outer_a, init_a)
    (ret_bits,) = outer.inline_block(inner_code, args())
    outer_ret = outer.add_variable("ret", IntType.u(16))
    ret_val = outer.emit_load(ret_bits)
    outer.emit_store(outer_ret, ret_val)

    final_val = 0xFFB2
    code = create_simplified_code(outer)
    correct = (Store(init_a, outer_a.bits.storage),)
    assert_nodes(code.nodes, correct)
    ret_val, ret_width = get_ret_val(code)
    assert_ret_val(code, final_val)
    assert ret_width == 16


def test_load_from_unsigned_reference_arg() -> None:
    """Test reading of a value passed via an unsigned reference."""
    inner = TestNamespace()
    arg_ref = inner.add_argument("R")
    arg_val = inner.emit_load(arg_ref)
    inner_ret = inner.add_variable("ret", IntType.u(16))
    inner.emit_store(inner_ret, arg_val)
    inner_code = inner.create_code_block(inner_ret)

    outer = TestNamespace()
    fixed_val = FixedValue(IntLiteral(0xA4), 8)
    (ret_bits,) = outer.inline_block(inner_code, args(R=fixed_val))
    outer_ret = outer.add_variable("ret", IntType.u(16))
    ret_val = outer.emit_load(ret_bits)
    outer.emit_store(outer_ret, ret_val)

    code = create_simplified_code(outer)
    correct = ()
    assert_nodes(code.nodes, correct)
    ret_val, ret_width = get_ret_val(code)
    assert_ret_val(code, 0x00A4)
    assert ret_width == 16


def test_load_from_signed_reference_arg() -> None:
    """Test reading of a value passed via a signed reference."""
    inner = TestNamespace()
    arg_ref = inner.add_argument("R", IntType.s(8))
    arg_val = inner.emit_load(arg_ref)
    inner_ret = inner.add_variable("ret", IntType.u(16))
    inner.emit_store(inner_ret, arg_val)
    inner_code = inner.create_code_block(inner_ret)

    outer = TestNamespace()
    fixed_val = FixedValue(IntLiteral(0xA4), 8)
    (ret_bits,) = outer.inline_block(inner_code, args(R=fixed_val))
    outer_ret = outer.add_variable("ret", IntType.u(16))
    ret_val = outer.emit_load(ret_bits)
    outer.emit_store(outer_ret, ret_val)

    code = create_simplified_code(outer)
    correct = ()
    assert_nodes(code.nodes, correct)
    ret_val, ret_width = get_ret_val(code)
    assert_ret_val(code, 0xFFA4)
    assert ret_width == 16


def test_return_simple_reference() -> None:
    """Test returning a reference to a global."""
    inner = TestNamespace()
    inner_a = inner.add_register("a")
    inner.add_ret_reference(inner_a)
    inner_ret = inner["ret"]
    assert isinstance(inner_ret, Reference), inner_ret
    inner_code = inner.create_code_block(inner_ret)
    assert len(inner_code.returned) == 1

    outer = TestNamespace(inner)
    (ret_bits,) = outer.inline_block(inner_code, args())
    outer_a = outer.add_register("a")
    fake = IntLiteral(0xDC)
    outer.emit_store(outer_a, fake)
    value = IntLiteral(0xBA)
    outer.emit_store(ret_bits, value)
    outer_ret = outer.add_variable("ret")
    ret_val = outer.emit_load(outer_a)
    outer.emit_store(outer_ret, ret_val)

    code = create_simplified_code(outer)
    correct = (Store(value, outer_a.bits.storage),)
    assert_nodes(code.nodes, correct)
    ret_val, ret_width = get_ret_val(code)
    assert_ret_val(code, 0xBA)
    assert ret_width == 8


def test_return_io_reference() -> None:
    """Test returning a reference to an index in an I/O channel."""
    inner = TestNamespace()
    addr_arg = inner.add_argument("A", IntType.u(16))
    addr_val = inner.emit_load(addr_arg)
    mem_byte = inner.add_io_storage("mem", addr_val)
    inner.add_ret_reference(mem_byte)
    inner_ret = inner["ret"]
    assert isinstance(inner_ret, Reference), inner_ret
    inner_code = inner.create_code_block(inner_ret)
    assert len(inner_code.returned) == 1

    outer = TestNamespace()
    addr = FixedValue(IntLiteral(0x4002), 16)
    (ret_bits,) = outer.inline_block(inner_code, args(A=addr))
    assert isinstance(ret_bits, SingleStorage), ret_bits
    outer_ret = outer.add_variable("ret")
    ret_val = outer.emit_load(ret_bits)
    outer.emit_store(outer_ret, ret_val)

    code = create_simplified_code(outer)
    correct = (Load(ret_bits.storage),)
    assert_nodes(code.nodes, correct)
    ret_val, ret_width = get_ret_val(code)
    assert ret_width == 8
    assert ret_val == code.nodes[0].expr


def test_unique_loads() -> None:
    """Test whether multiple instances of the same load are kept separate.
    If the possibility of side effects is ignored, only one load will
    remain.
    If the load results are not kept separate, the load result will be
    XOR-ed with itself, resulting in a 0 result.
    """
    addr = IntLiteral(0xFFFF)

    inner = TestNamespace()
    mem_byte = inner.add_io_storage("mem", addr)
    load_r = inner.emit_load(mem_byte)
    inner_ret = inner.add_variable("ret")
    inner.emit_store(inner_ret, load_r)
    inner_code = inner.create_code_block(inner_ret)
    assert len(inner_code.returned) == 1

    outer = TestNamespace()
    (val1_bits,) = outer.inline_block(inner_code)
    assert isinstance(val1_bits, FixedValue)
    val1 = val1_bits.expr
    (val2_bits,) = outer.inline_block(inner_code)
    assert isinstance(val2_bits, FixedValue)
    val2 = val2_bits.expr
    outer_ret = outer.add_variable("ret")
    outer.emit_store(outer_ret, XorOperator(val1, val2))

    code = create_simplified_code(outer)
    correct = (
        Load(mem_byte.bits.storage),
        Load(mem_byte.bits.storage),
    )
    assert_nodes(code.nodes, correct)
    ret_val, ret_width = get_ret_val(code)
    assert ret_width == 8
    assert isinstance(ret_val, XorOperator)
