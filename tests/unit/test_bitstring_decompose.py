from __future__ import annotations

from typing import Callable, Iterator, Sequence, cast

from pytest import fixture, mark

from retroasm.codeblock import Load, LoadedValue, Store
from retroasm.expression import (
    AndOperator,
    Expression,
    IntLiteral,
    LShift,
    OrOperator,
    RVShift,
)
from retroasm.reference import BitString, ConcatenatedBits, SingleStorage, SlicedBits
from retroasm.storage import Storage
from retroasm.types import IntType, Segment, Width, mask_for_width, width_for_mask

from .utils_codeblock import TestNamespace


@fixture
def namespace() -> TestNamespace:
    return TestNamespace()


def slice_bits(bits: BitString, offset: int, width: Width) -> SlicedBits:
    return SlicedBits(bits, IntLiteral(offset), width)


def decompose_expr(expr: Expression) -> Iterator[tuple[Expression, int, Width, int]]:
    assert isinstance(expr, Expression)
    if isinstance(expr, AndOperator):
        assert len(expr.exprs) == 2
        sub_expr, mask_expr = expr.exprs
        assert isinstance(mask_expr, IntLiteral)
        mask = mask_expr.value
        mask_width = width_for_mask(mask)
        assert mask_for_width(mask_width) == mask
        for dex, offset, width, shift in decompose_expr(sub_expr):
            width = min(width, mask_width - shift)
            if width > 0:
                yield dex, offset, width, shift
    elif isinstance(expr, OrOperator):
        for sub_expr in expr.exprs:
            yield from decompose_expr(sub_expr)
    elif isinstance(expr, LShift):
        for dex, offset, width, shift in decompose_expr(expr.expr):
            yield dex, offset, width, shift + expr.offset
    elif isinstance(expr, RVShift):
        assert isinstance(expr.offset, IntLiteral)
        rv_offset = expr.offset.value
        for dex, offset, width, shift in decompose_expr(expr.expr):
            shift -= rv_offset
            if shift < 0:
                dropped_bits = -shift
                shift = 0
                offset += dropped_bits
                width -= dropped_bits
                if width <= 0:
                    continue
            yield dex, offset, width, shift
    else:
        yield expr, 0, width_for_mask(expr.mask), 0


def iter_slices(bits: BitString) -> Iterator[SlicedBits]:
    """Iterate through the SlicedBits contained in the given bit string."""
    if isinstance(bits, SingleStorage):
        pass
    elif isinstance(bits, ConcatenatedBits):
        for sub in bits:
            yield from iter_slices(sub)
    elif isinstance(bits, SlicedBits):
        yield bits
    else:
        raise TypeError(f"Unsupported BitString subtype: {type(bits).__name__}")


def iter_slice_loads(bits: BitString) -> Iterator[Storage]:
    """
    Iterate through the storages that must be loaded when storing into
    a bit string that may contain slicing.
    Sliced bit strings must load their original version to combine it with
    the written value before the store happens. If sliced bit strings are
    nested, the same storage might exist within multiple parts of the tree.
    To avoid losing updates, each node must complete its load-combine-store
    cycle before other nodes can be processed.
    """
    for slice_bits in iter_slices(bits):
        yield from slice_bits.iter_storages()
        yield from iter_slice_loads(slice_bits.bits)


def check_flatten(
    namespace: TestNamespace,
    bits: BitString,
    expected: Sequence[tuple[Storage, Segment]],
) -> None:
    """
    Check that the produced bit string flattens to the expected output.
    This is more likely to find errors in the test cases rather than
    the code under test, but it is valuable anyway as a sanity check
    separate from the more complex load and store testing.
    """

    i = 0
    width: Width = 0
    for base, base_seg in bits.decompose():
        try:
            expected_item = expected[i]
        except IndexError:
            raise AssertionError(
                f"Bit string produced more than the {len(expected)} expected items"
            )
        else:
            i += 1
        assert isinstance(base, SingleStorage), base
        assert base.storage == expected_item[0]
        assert base_seg == expected_item[1]
        width += base_seg.width
    assert i >= len(
        expected
    ), f"Bit string produced only {i} of the {len(expected)} expected items"
    assert width <= bits.width


def check_load(
    namespace: TestNamespace,
    bits: BitString,
    expected: Sequence[tuple[Storage, Segment]],
) -> None:
    """Check that loading from a bit string works as expected."""

    # Check that emit_load only emits Load nodes.
    value = namespace.emit_load(bits)
    nodes = namespace.builder.nodes
    for node in nodes:
        assert isinstance(node, Load)

    # Check that all underlying storages are loaded from.
    # Also check that the load order matches the depth-first tree walk.
    # Even storages that are not part of the decomposition should still be
    # loaded from since loading might trigger side effects.
    all_storages = tuple(bits.iter_storages())
    loaded_storages = tuple(node.storage for node in nodes)
    assert all_storages == loaded_storages

    # Check the loaded value expression's bit mask.
    assert (
        width_for_mask(value.mask) <= bits.width
    ), "loaded value is wider than bit string"

    # Check that the loaded expression's terms don't overlap.
    decomposed_val = tuple(decompose_expr(value))
    mask = 0
    for dex, offset, width, shift in decomposed_val:
        term_mask = mask_for_width(width) << shift
        assert mask & term_mask == 0, "loaded terms overlap"

    # Check loaded value.
    assert len(decomposed_val) == len(expected)
    offset = 0
    for actual_item, expected_item in zip(decomposed_val, expected):
        val_expr, val_offset, val_width, val_shift = actual_item
        exp_storage, exp_segment = expected_item
        assert isinstance(val_expr, LoadedValue)
        assert val_expr.load.storage == exp_storage
        assert val_shift == offset
        assert val_offset == exp_segment.start
        assert val_width == exp_segment.width
        offset += cast(int, val_width)


def check_store(
    namespace: TestNamespace,
    bits: BitString,
    expected: Sequence[tuple[Storage, Segment]],
) -> None:
    """Check that storing to a bit string works as expected."""

    # Check that emit_store only emits Load and Store nodes.
    nodes = namespace.builder.nodes
    value_ref = namespace.add_argument("V", IntType.int)
    value = namespace.emit_load(value_ref)
    init_idx = len(nodes)
    namespace.emit_store(bits, value)
    load_nodes: list[Load] = []
    store_nodes: list[Store] = []
    for node in nodes[init_idx:]:
        if isinstance(node, Load):
            load_nodes.append(node)
        elif isinstance(node, Store):
            store_nodes.append(node)
        else:
            assert False, f"unexpected node type: {type(node).__name__}"

    # Check that all storages reachable through slicing are loaded from.
    # Also check that the load order is as expected (see iterSliceLoads
    # docstring).
    sliced_storages = tuple(iter_slice_loads(bits))
    loaded_storages = tuple(node.storage for node in load_nodes)
    assert sliced_storages == loaded_storages

    # Check that all underlying storages are stored to.
    # Also check that the store order matches the depth-first tree walk.
    all_storages = tuple(bits.iter_storages())
    stored_storages = tuple(node.storage for node in store_nodes)
    assert all_storages == stored_storages

    # Note: Verifying that the right values are being stored based on the
    #       expectation list is quite complex.
    #       We're better off writing a separate test for that.


check_param = mark.parametrize("check", [check_flatten, check_load, check_store])

CheckFunc = Callable[
    [TestNamespace, BitString, Sequence[tuple[Storage, Segment]]],
    None,
]


@check_param
def test_decompose_single(namespace: TestNamespace, check: CheckFunc) -> None:
    """Test construction of SingleStorage."""
    ref0 = namespace.add_argument("R0")
    expected = namespace.parse("R0[0:8]")
    check(namespace, ref0.bits, expected)


@check_param
def test_decompose_basic_concat(namespace: TestNamespace, check: CheckFunc) -> None:
    """Test construction of ConcatenatedBits."""
    ref0 = namespace.add_argument("R0", IntType.u(7))
    ref1 = namespace.add_argument("R1", IntType.u(3))
    ref2 = namespace.add_argument("R2", IntType.u(13))
    concat = ConcatenatedBits(ref2.bits, ref1.bits, ref0.bits)
    expected = namespace.parse("R2[0:13]", "R1[0:3]", "R0[0:7]")
    check(namespace, concat, expected)


@check_param
def test_decompose_self_concat(namespace: TestNamespace, check: CheckFunc) -> None:
    """Test concatenation of a bit string to itself."""
    ref0 = namespace.add_argument("R0")
    concat = ConcatenatedBits(ref0.bits, ref0.bits)
    expected = namespace.parse("R0[0:8]", "R0[0:8]")
    check(namespace, concat, expected)


@check_param
def test_decompose_basic_slice(namespace: TestNamespace, check: CheckFunc) -> None:
    """Test construction of SlicedBits."""
    ref0 = namespace.add_argument("R0")
    sliced = slice_bits(ref0.bits, 2, 3)
    expected = namespace.parse("R0[2:5]")
    check(namespace, sliced, expected)


@check_param
def test_decompose_slice_past_end(namespace: TestNamespace, check: CheckFunc) -> None:
    """Test clipping of slice width against parent width."""
    ref0 = namespace.add_argument("R0")
    sliced = slice_bits(ref0.bits, 2, 30)
    expected = namespace.parse("R0[2:8]")
    check(namespace, sliced, expected)


@check_param
def test_decompose_slice_outside(namespace: TestNamespace, check: CheckFunc) -> None:
    """Test handling of slice index outside parent width."""
    ref0 = namespace.add_argument("R0")
    sliced = slice_bits(ref0.bits, 12, 30)
    expected = ()
    check(namespace, sliced, expected)


@check_param
def test_decompose_slice_concat(namespace: TestNamespace, check: CheckFunc) -> None:
    """Test slicing concatenated values."""
    ref0 = namespace.add_argument("R0")
    ref1 = namespace.add_argument("R1")
    ref2 = namespace.add_argument("R2")
    concat = ConcatenatedBits(ref2.bits, ref1.bits, ref0.bits)
    sliced = slice_bits(concat, 5, 13)
    expected = namespace.parse("R2[5:8]", "R1[0:8]", "R0[0:2]")
    check(namespace, sliced, expected)


@check_param
def test_decompose_combined(namespace: TestNamespace, check: CheckFunc) -> None:
    """Test combinations of slicing and concatenation."""
    ref0 = namespace.add_argument("R0")
    ref1 = namespace.add_argument("R1")
    concat_a = ConcatenatedBits(ref0.bits, ref1.bits)
    slice_a = slice_bits(concat_a, 5, 6)
    ref2 = namespace.add_argument("R2")
    concat_b = ConcatenatedBits(slice_a, ref2.bits)
    storage = slice_bits(concat_b, 4, 7)
    expected = namespace.parse("R1[1:3]", "R2[0:5]")
    check(namespace, storage, expected)


@check_param
def test_decompose_nested_slice(namespace: TestNamespace, check: CheckFunc) -> None:
    """Test taking a slice from sliced bit strings."""
    ref0 = namespace.add_argument("R0")
    ref1 = namespace.add_argument("R1")
    slice0 = slice_bits(ref0.bits, 2, 5)
    slice1 = slice_bits(ref1.bits, 1, 4)
    concat = ConcatenatedBits(slice0, slice1)
    slice_c = slice_bits(concat, 3, 3)
    expected = namespace.parse("R0[5:7]", "R1[1:2]")
    check(namespace, slice_c, expected)
