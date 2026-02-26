from __future__ import annotations

from collections.abc import Callable, Iterator, Sequence
from typing import cast

from pytest import fixture, mark

from retroasm.codeblock import Load, LoadedValue, Store
from retroasm.codeblock_builder import SemanticsCodeBlockBuilder
from retroasm.expression import AndOperator, Expression, IntLiteral, LShift, OrOperator, RVShift
from retroasm.namespace import GlobalNamespace, LocalNamespace, ReadOnlyNamespace
from retroasm.reference import BitString, ConcatenatedBits, Reference, SingleStorage, SlicedBits
from retroasm.storage import Storage
from retroasm.types import IntType, Segment, Width, mask_for_width, width_for_mask

from .utils_segment import parse_segment


def parse_slices(
    namespace: ReadOnlyNamespace, *storage_slices: str
) -> tuple[tuple[Storage, Segment], ...]:
    def _parse_one(storage_str: str) -> tuple[Storage, Segment]:
        idx = storage_str.index("[")
        name = storage_str[:idx]
        slice_str = storage_str[idx:]
        ref = namespace[name]
        assert isinstance(ref, Reference), ref
        bits = ref.bits
        assert isinstance(bits, SingleStorage), bits
        return bits.storage, parse_segment(slice_str)

    return tuple(_parse_one(storage_str) for storage_str in storage_slices)


@fixture
def namespace() -> LocalNamespace:
    return LocalNamespace(GlobalNamespace())


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
    namespace: LocalNamespace, bits: BitString, expected: Sequence[tuple[Storage, Segment]]
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
    assert i >= len(expected), (
        f"Bit string produced only {i} of the {len(expected)} expected items"
    )
    assert width <= bits.width


def check_load(
    namespace: LocalNamespace, bits: BitString, expected: Sequence[tuple[Storage, Segment]]
) -> None:
    """Check that loading from a bit string works as expected."""

    builder = SemanticsCodeBlockBuilder()

    # Check that emit_load only emits Load nodes.
    value = bits.emit_load(builder, None)

    operations = builder.operations
    for operation in operations:
        assert isinstance(operation, Load)

    # Check that all underlying storages are loaded from.
    # Also check that the load order matches the depth-first tree walk.
    # Even storages that are not part of the decomposition should still be
    # loaded from since loading might trigger side effects.
    all_storages = tuple(bits.iter_storages())
    loaded_storages = tuple(operation.storage for operation in operations)
    assert all_storages == loaded_storages

    # Check the loaded value expression's bit mask.
    assert width_for_mask(value.mask) <= bits.width, "loaded value is wider than bit string"

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
    namespace: LocalNamespace, bits: BitString, expected: Sequence[tuple[Storage, Segment]]
) -> None:
    """Check that storing to a bit string works as expected."""

    # Check that emit_store only emits Load and Store nodes.
    builder = SemanticsCodeBlockBuilder()
    operations = builder.operations
    value_ref = namespace.add_argument("V", IntType.int, None)
    value = value_ref.emit_load(builder, None)
    init_idx = len(operations)
    bits.emit_store(builder, value, None)
    load_ops: list[Load] = []
    store_ops: list[Store] = []
    for operation in operations[init_idx:]:
        if isinstance(operation, Load):
            load_ops.append(operation)
        elif isinstance(operation, Store):
            store_ops.append(operation)
        else:
            assert False, f"unexpected node type: {type(operation).__name__}"

    # Check that all storages reachable through slicing are loaded from.
    # Also check that the load order is as expected (see iterSliceLoads
    # docstring).
    sliced_storages = tuple(iter_slice_loads(bits))
    loaded_storages = tuple(load.storage for load in load_ops)
    assert sliced_storages == loaded_storages

    # Check that all underlying storages are stored to.
    # Also check that the store order matches the depth-first tree walk.
    all_storages = tuple(bits.iter_storages())
    stored_storages = tuple(store.storage for store in store_ops)
    assert all_storages == stored_storages

    # Note: Verifying that the right values are being stored based on the
    #       expectation list is quite complex.
    #       We're better off writing a separate test for that.


check_param = mark.parametrize("check", [check_flatten, check_load, check_store])

CheckFunc = Callable[[LocalNamespace, BitString, Sequence[tuple[Storage, Segment]]], None]


@check_param
def test_decompose_single(namespace: LocalNamespace, check: CheckFunc) -> None:
    """Test construction of SingleStorage."""
    ref0 = namespace.add_argument("R0", IntType.u(8), None)
    expected = parse_slices(namespace, "R0[0:8]")
    check(namespace, ref0.bits, expected)


@check_param
def test_decompose_basic_concat(namespace: LocalNamespace, check: CheckFunc) -> None:
    """Test construction of ConcatenatedBits."""
    ref0 = namespace.add_argument("R0", IntType.u(7), None)
    ref1 = namespace.add_argument("R1", IntType.u(3), None)
    ref2 = namespace.add_argument("R2", IntType.u(13), None)
    concat = ConcatenatedBits(ref2.bits, ref1.bits, ref0.bits)
    expected = parse_slices(namespace, "R2[0:13]", "R1[0:3]", "R0[0:7]")
    check(namespace, concat, expected)


@check_param
def test_decompose_self_concat(namespace: LocalNamespace, check: CheckFunc) -> None:
    """Test concatenation of a bit string to itself."""
    ref0 = namespace.add_argument("R0", IntType.u(8), None)
    concat = ConcatenatedBits(ref0.bits, ref0.bits)
    expected = parse_slices(namespace, "R0[0:8]", "R0[0:8]")
    check(namespace, concat, expected)


@check_param
def test_decompose_basic_slice(namespace: LocalNamespace, check: CheckFunc) -> None:
    """Test construction of SlicedBits."""
    ref0 = namespace.add_argument("R0", IntType.u(8), None)
    sliced = SlicedBits(ref0.bits, IntLiteral(2), 3)
    expected = parse_slices(namespace, "R0[2:5]")
    check(namespace, sliced, expected)


@check_param
def test_decompose_slice_past_end(namespace: LocalNamespace, check: CheckFunc) -> None:
    """Test clipping of slice width against parent width."""
    ref0 = namespace.add_argument("R0", IntType.u(8), None)
    sliced = SlicedBits(ref0.bits, IntLiteral(2), 30)
    expected = parse_slices(namespace, "R0[2:8]")
    check(namespace, sliced, expected)


@check_param
def test_decompose_slice_outside(namespace: LocalNamespace, check: CheckFunc) -> None:
    """Test handling of slice index outside parent width."""
    ref0 = namespace.add_argument("R0", IntType.u(8), None)
    sliced = SlicedBits(ref0.bits, IntLiteral(12), 30)
    expected = ()
    check(namespace, sliced, expected)


@check_param
def test_decompose_slice_concat(namespace: LocalNamespace, check: CheckFunc) -> None:
    """Test slicing concatenated values."""
    ref0 = namespace.add_argument("R0", IntType.u(8), None)
    ref1 = namespace.add_argument("R1", IntType.u(8), None)
    ref2 = namespace.add_argument("R2", IntType.u(8), None)
    concat = ConcatenatedBits(ref2.bits, ref1.bits, ref0.bits)
    sliced = SlicedBits(concat, IntLiteral(5), 13)
    expected = parse_slices(namespace, "R2[5:8]", "R1[0:8]", "R0[0:2]")
    check(namespace, sliced, expected)


@check_param
def test_decompose_combined(namespace: LocalNamespace, check: CheckFunc) -> None:
    """Test combinations of slicing and concatenation."""
    ref0 = namespace.add_argument("R0", IntType.u(8), None)
    ref1 = namespace.add_argument("R1", IntType.u(8), None)
    concat_a = ConcatenatedBits(ref0.bits, ref1.bits)
    slice_a = SlicedBits(concat_a, IntLiteral(5), 6)
    ref2 = namespace.add_argument("R2", IntType.u(8), None)
    concat_b = ConcatenatedBits(slice_a, ref2.bits)
    storage = SlicedBits(concat_b, IntLiteral(4), 7)
    expected = parse_slices(namespace, "R1[1:3]", "R2[0:5]")
    check(namespace, storage, expected)


@check_param
def test_decompose_nested_slice(namespace: LocalNamespace, check: CheckFunc) -> None:
    """Test taking a slice from sliced bit strings."""
    ref0 = namespace.add_argument("R0", IntType.u(8), None)
    ref1 = namespace.add_argument("R1", IntType.u(8), None)
    slice0 = SlicedBits(ref0.bits, IntLiteral(2), 5)
    slice1 = SlicedBits(ref1.bits, IntLiteral(1), 4)
    concat = ConcatenatedBits(slice0, slice1)
    slice_c = SlicedBits(concat, IntLiteral(3), 3)
    expected = parse_slices(namespace, "R0[5:7]", "R1[1:2]")
    check(namespace, slice_c, expected)
