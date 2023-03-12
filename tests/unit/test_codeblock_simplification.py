from __future__ import annotations

from typing import Iterator

from pytest import fixture

from retroasm.codeblock import CodeBlock, Load, Node, Store
from retroasm.expression import (
    AddOperator,
    AndOperator,
    IntLiteral,
    OrOperator,
    truncate,
)
from retroasm.expression_simplifier import simplify_expression
from retroasm.reference import (
    ConcatenatedBits,
    FixedValue,
    Reference,
    SingleStorage,
    SlicedBits,
)
from retroasm.storage import IOStorage
from retroasm.types import IntType

from .utils_codeblock import (
    SingleStorageReference,
    TestNamespace,
    assert_nodes,
    assert_ret_val,
    get_ret_val,
)
from .utils_expression import assert_int_literal, assert_or, assert_trunc, make_concat

verbose = False


@fixture
def namespace() -> TestNamespace:
    return TestNamespace()


def create_simplified_code(namespace: TestNamespace) -> CodeBlock:
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


def test_no_change(namespace: TestNamespace) -> None:
    """Test whether a basic sequence survives a simplification attempt."""
    ref_a = namespace.add_register("a")
    ref_b = namespace.add_register("b")
    load_a = namespace.emit_load(ref_a)
    namespace.emit_store(ref_b, load_a)

    def check_nodes(code: CodeBlock) -> None:
        assert len(code.nodes) == 2
        load, store = code.nodes
        assert isinstance(load, Load)
        assert isinstance(store, Store)
        assert load.storage == ref_a.bits.storage
        assert store.storage == ref_b.bits.storage
        assert store.expr is load.expr

    code = namespace.create_code_block(None)
    check_nodes(code)
    code = create_simplified_code(namespace)
    check_nodes(code)


def test_stored_expression(namespace: TestNamespace) -> None:
    """Test whether stored expressions are simplified."""
    const1 = IntLiteral(2)
    const2 = AddOperator(IntLiteral(1), IntLiteral(1))
    ref_a = namespace.add_register("a")
    ref_b = namespace.add_register("b")
    namespace.emit_store(ref_a, const1)
    namespace.emit_store(ref_b, const2)

    correct = (
        Store(const1, ref_a.bits.storage),
        Store(const1, ref_b.bits.storage),
    )

    code = create_simplified_code(namespace)
    assert_nodes(code.nodes, correct)


def test_unused_load(namespace: TestNamespace) -> None:
    """Test whether unused loads are removed."""
    ref_a = namespace.add_register("a")
    load_a = namespace.emit_load(ref_a)
    and_a = AndOperator(load_a, IntLiteral(0))
    namespace.emit_store(ref_a, and_a)

    code = create_simplified_code(namespace)
    assert len(code.nodes) == 1
    node = code.nodes[0]
    assert isinstance(node, Store)
    assert node.storage is ref_a.bits.storage
    assert_int_literal(node.expr, 0)


def test_unused_load_nonremoval(namespace: TestNamespace) -> None:
    """Test whether unused loads are kept for possible side effects."""
    addr = IntLiteral(0xD0D0)
    ref_m = namespace.add_io_storage("mem", addr)
    load_m_ = namespace.emit_load(ref_m)

    correct = (Load(ref_m.bits.storage),)

    code = create_simplified_code(namespace)
    assert_nodes(code.nodes, correct)


def test_redundant_load_after_load(namespace: TestNamespace) -> None:
    """Test whether redundant successive loads are removed."""
    ref_a = namespace.add_register("a")
    ref_b = namespace.add_register("b")
    ref_c = namespace.add_register("c")
    load_a1 = namespace.emit_load(ref_a)
    load_a2 = namespace.emit_load(ref_a)
    namespace.emit_store(ref_b, load_a1)
    namespace.emit_store(ref_c, load_a2)

    def correct() -> Iterator[Node]:
        load_a = Load(ref_a.bits.storage)
        yield load_a
        yield Store(load_a.expr, ref_b.bits.storage)
        yield Store(load_a.expr, ref_c.bits.storage)

    code = create_simplified_code(namespace)
    assert_nodes(code.nodes, correct())


def test_redundant_load_after_store(namespace: TestNamespace) -> None:
    """Test whether a redundant load after a store is removed."""
    ref_a = namespace.add_register("a")
    ref_b = namespace.add_register("b")
    load_a1 = namespace.emit_load(ref_a)
    inc_a = AddOperator(load_a1, IntLiteral(1))
    namespace.emit_store(ref_a, inc_a)
    load_a2 = namespace.emit_load(ref_a)
    namespace.emit_store(ref_b, load_a2)

    code = create_simplified_code(namespace)
    assert len(code.nodes) == 3
    load, store1, store2 = code.nodes
    assert isinstance(load, Load)
    assert isinstance(store1, Store)
    assert isinstance(store2, Store)
    assert load.storage == ref_a.bits.storage
    assert store1.storage == ref_a.bits.storage
    assert store2.storage == ref_b.bits.storage
    assert store1.expr == store2.expr
    assert_trunc(
        simplify_expression(store1.expr),
        simplify_expression(inc_a).substitute(
            lambda expr: load.expr if expr is load_a1 else None
        ),
        inc_a.mask.bit_length(),
        ref_a.width,
    )


def test_redundant_same_value_store(namespace: TestNamespace) -> None:
    """Test removal of storing the same value in the same storage twice."""
    ref_a = namespace.add_register("a")
    ref_b = namespace.add_register("b")
    load_a = namespace.emit_load(ref_a)
    namespace.emit_store(ref_b, load_a)
    namespace.emit_store(ref_b, load_a)

    def correct() -> Iterator[Node]:
        load_a = Load(ref_a.bits.storage)
        yield load_a
        yield Store(load_a.expr, ref_b.bits.storage)

    code = create_simplified_code(namespace)
    assert_nodes(code.nodes, correct())


def test_redundant_other_value_store(namespace: TestNamespace) -> None:
    """Test removal of storing a different value in the same storage."""
    ref_a = namespace.add_register("a")
    ref_b = namespace.add_register("b")
    ref_c = namespace.add_register("c")
    load_a = namespace.emit_load(ref_a)
    load_b = namespace.emit_load(ref_b)
    namespace.emit_store(ref_c, load_a)
    namespace.emit_store(ref_c, load_b)

    def correct() -> Iterator[Node]:
        load_b = Load(ref_b.bits.storage)
        yield load_b
        yield Store(load_b.expr, ref_c.bits.storage)

    code = create_simplified_code(namespace)
    assert_nodes(code.nodes, correct())


def test_uncertain_redundant_load(namespace: TestNamespace) -> None:
    """Test whether aliasing prevents loads from being removed."""
    const = IntLiteral(23)
    ref_a = namespace.add_register("a")
    ref_b = namespace.add_register("b")
    ref_c = namespace.add_register("c")
    ref_x = namespace.add_argument("X")
    load_a1 = namespace.emit_load(ref_a)
    namespace.emit_store(ref_x, const)
    load_a2 = namespace.emit_load(ref_a)
    namespace.emit_store(ref_b, load_a1)
    namespace.emit_store(ref_c, load_a2)

    def correct() -> Iterator[Node]:
        load_a1 = Load(ref_a.bits.storage)
        yield load_a1
        yield Store(const, ref_x.bits.storage)
        load_a2 = Load(ref_a.bits.storage)
        yield load_a2
        yield Store(load_a1.expr, ref_b.bits.storage)
        yield Store(load_a2.expr, ref_c.bits.storage)

    code = create_simplified_code(namespace)
    assert_nodes(code.nodes, correct())


def test_same_value_redundant_load(namespace: TestNamespace) -> None:
    """Test handling of writing the same value to a potential alias."""
    ref_a = namespace.add_register("a")
    ref_b = namespace.add_register("b")
    ref_x = namespace.add_argument("X")
    load_a1 = namespace.emit_load(ref_a)
    namespace.emit_store(ref_x, load_a1)
    load_a2 = namespace.emit_load(ref_a)
    namespace.emit_store(ref_b, load_a2)

    def correct() -> Iterator[Node]:
        load_a = Load(ref_a.bits.storage)
        yield load_a
        yield Store(load_a.expr, ref_x.bits.storage)
        yield Store(load_a.expr, ref_b.bits.storage)

    code = create_simplified_code(namespace)
    assert_nodes(code.nodes, correct())


def test_local_value(namespace: TestNamespace) -> None:
    """Test whether load and stores of variables are removed."""
    ref_a = namespace.add_register("a")
    ref_b = namespace.add_register("b")
    ref_v = namespace.add_variable("V")
    load_a = namespace.emit_load(ref_a)
    namespace.emit_store(ref_v, load_a)
    load_v = namespace.emit_load(ref_v)
    namespace.emit_store(ref_b, load_v)

    def correct() -> Iterator[Node]:
        load_a = Load(ref_a.bits.storage)
        yield load_a
        yield Store(load_a.expr, ref_b.bits.storage)

    code = create_simplified_code(namespace)
    assert_nodes(code.nodes, correct())


def test_unused_storage_removal(namespace: TestNamespace) -> None:
    """Test whether unused storages are removed."""
    ref_a = namespace.add_register("a")
    load_a = namespace.emit_load(ref_a)
    ref_m_ = namespace.add_io_storage("mem", load_a)

    correct = ()

    code = create_simplified_code(namespace)
    assert_nodes(code.nodes, correct)


def test_return_value(namespace: TestNamespace) -> None:
    """Test whether a return value is stored correctly."""
    ref_v = namespace.add_argument("V")
    ref_a = namespace.add_register("a")
    ref_ret = namespace.add_variable("ret")
    load_a = namespace.emit_load(ref_a)
    load_v = namespace.emit_load(ref_v)
    combined = OrOperator(load_a, load_v)
    namespace.emit_store(ref_ret, combined)

    correct = (
        Load(ref_a.bits.storage),
        Load(ref_v.bits.storage),
    )

    code = create_simplified_code(namespace)
    assert_nodes(code.nodes, correct)
    ret_val, ret_width = get_ret_val(code)
    value_v = code.nodes[0].expr
    value_a = code.nodes[1].expr
    assert ret_width == 8
    assert_or(ret_val, simplify_expression(value_a), simplify_expression(value_v))


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


def test_return_io_index(namespace: TestNamespace) -> None:
    """Test returning an I/O reference with a simplifiable index."""
    addr = AddOperator(IntLiteral(1), IntLiteral(1))
    mem_byte = namespace.add_io_storage("mem", addr)
    namespace.add_ret_reference(mem_byte)

    code = create_simplified_code(namespace)
    assert len(code.returned) == 1
    (ret_bits,) = code.returned
    assert isinstance(ret_bits, SingleStorage)
    assert ret_bits.width == 8
    storage = ret_bits.storage
    assert isinstance(storage, IOStorage)
    assert_int_literal(storage.index, 2)


def test_return_redundant_load_index(namespace: TestNamespace) -> None:
    """Test returning a redundant loaded value."""
    ref_a = namespace.add_register("a", IntType.u(16))
    namespace.emit_store(ref_a, IntLiteral(0x4120))
    load_a = namespace.emit_load(ref_a)
    mem_byte = namespace.add_io_storage("mem", load_a)
    namespace.add_ret_reference(mem_byte)

    code = create_simplified_code(namespace)
    assert len(code.returned) == 1
    (ret_bits,) = code.returned
    assert isinstance(ret_bits, SingleStorage)
    assert ret_bits.width == 8
    storage = ret_bits.storage
    assert isinstance(storage, IOStorage)
    assert_int_literal(storage.index, 0x4120)


def test_return_fixed_value_ref(namespace: TestNamespace) -> None:
    """Test returning a reference to a fixed value."""
    add = AddOperator(IntLiteral(1), IntLiteral(2))
    value = FixedValue(add, 8)
    namespace.add_ret_reference(Reference(value, IntType.u(8)))
    code = create_simplified_code(namespace)
    assert_nodes(code.nodes, ())
    assert len(code.returned) == 1
    (ret_bits,) = code.returned
    assert isinstance(ret_bits, FixedValue)
    assert ret_bits.width == 8
    assert_int_literal(ret_bits.expr, 3)


def test_return_complex_ref(namespace: TestNamespace) -> None:
    """Test returning a non-trivial reference."""
    ref_h = namespace.add_register("h")
    ref_l = namespace.add_register("l")
    bits_hl = ConcatenatedBits(ref_l.bits, ref_h.bits)
    sliced_bits = SlicedBits(bits_hl, IntLiteral(0), 8)
    namespace.add_ret_reference(Reference(sliced_bits, IntType.u(8)))
    code = create_simplified_code(namespace)
    assert_nodes(code.nodes, ())
    assert len(code.returned) == 1
    (ret_bits,) = code.returned
    assert ret_bits.width == 8
    # Note that we only simplify expressions, not references, so the
    # reference itself is still complex. All we really check here is
    # that code block creation doesn't break, but that is worthwhile
    # in itself.


def run_repeated_increase(
    namespace: TestNamespace, counter_ref: SingleStorageReference, counter_remains: bool
) -> None:
    """Helper method for repeated increase tests."""

    def emit_inc() -> None:
        load_counter = namespace.emit_load(counter_ref)
        inc_a = AddOperator(load_counter, IntLiteral(1))
        namespace.emit_store(counter_ref, inc_a)

    init_counter = IntLiteral(23)
    namespace.emit_store(counter_ref, init_counter)
    emit_inc()
    emit_inc()
    emit_inc()
    final_counter = namespace.emit_load(counter_ref)
    ret = namespace.add_variable("ret")
    namespace.emit_store(ret, final_counter)

    code = create_simplified_code(namespace)
    ret_val, ret_width = get_ret_val(code)
    correct: list[Node] = []
    if counter_remains:
        correct.insert(0, Store(ret_val, counter_ref.bits.storage))
    assert_nodes(code.nodes, correct)
    assert_ret_val(code, 26)
    assert ret_width == 8


def test_repeated_increase_reg(namespace: TestNamespace) -> None:
    """Test removal of redundant loads and stores to a register."""
    ref_a = namespace.add_register("a")
    run_repeated_increase(namespace, ref_a, True)


def test_repeated_increase_var(namespace: TestNamespace) -> None:
    """Test removal of redundant loads and stores to a local variable."""
    ref_a = namespace.add_variable("A")
    run_repeated_increase(namespace, ref_a, False)


def run_signed_load(namespace: TestNamespace, write: int, compare: int) -> None:
    """Helper method for signed load tests."""
    signed_var = namespace.add_variable("s", IntType.s(8))
    namespace.emit_store(signed_var, IntLiteral(write))
    loaded = namespace.emit_load(signed_var)
    ret = namespace.add_variable("ret", IntType.int)
    namespace.emit_store(ret, loaded)

    code = create_simplified_code(namespace)
    assert_ret_val(code, compare)


def test_signed_load_positive(namespace: TestNamespace) -> None:
    """Test loading of a positive signed integer."""
    run_signed_load(namespace, 56, 56)


def test_signed_load_negative(namespace: TestNamespace) -> None:
    """Test loading of a negative signed integer."""
    run_signed_load(namespace, -78, -78)


def test_signed_load_wrap(namespace: TestNamespace) -> None:
    """Test loading of an unsigned integer as signed."""
    run_signed_load(namespace, 135, 135 - 256)


def test_signed_load_unlimited(namespace: TestNamespace) -> None:
    """Test loading of an unlimited width integer."""

    # No sign extension should be happening here, but the code once
    # contained a bug where it would try to apply sign extension and
    # triggered an internal consistency check.

    signed_var = namespace.add_variable("s", IntType.int)
    namespace.emit_store(signed_var, IntLiteral(987654321))
    loaded = namespace.emit_load(signed_var)
    ret = namespace.add_variable("ret", IntType.int)
    namespace.emit_store(ret, loaded)

    code = create_simplified_code(namespace)
    assert_ret_val(code, 987654321)


def test_6502_pull(namespace: TestNamespace) -> None:
    """Test simplification of the 6502 PULL instructions."""

    ref_d = namespace.add_argument("D")
    ref_s = namespace.add_register("s")
    load_s1 = namespace.emit_load(ref_s)
    inc_s = AddOperator(load_s1, IntLiteral(1))
    namespace.emit_store(ref_s, inc_s)
    const1 = IntLiteral(1)
    load_s2 = namespace.emit_load(ref_s)
    ref_m = namespace.add_io_storage("mem", make_concat(const1, load_s2, 8))
    load_m = namespace.emit_load(ref_m)
    namespace.emit_store(ref_d, load_m)

    code = create_simplified_code(namespace)
    (io_storage,) = (
        node.storage for node in code.nodes if isinstance(node.storage, IOStorage)
    )

    def correct() -> Iterator[Node]:
        load_s = Load(ref_s.bits.storage)
        yield load_s
        expr = truncate(inc_s, 8).substitute(
            lambda expr: load_s.expr if expr is load_s1 else None
        )
        yield Store(expr, ref_s.bits.storage)
        load_m = Load(io_storage)
        yield load_m
        yield Store(load_m.expr, ref_d.bits.storage)

    assert_nodes(code.nodes, correct())
