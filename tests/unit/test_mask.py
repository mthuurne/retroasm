from __future__ import annotations

from dataclasses import dataclass, replace
from itertools import pairwise

from hypothesis import given, infer, note
from hypothesis.strategies import (
    DrawFn,
    SearchStrategy,
    builds,
    composite,
    integers,
    just,
    one_of,
    register_type_strategy,
)
from pytest import raises

from retroasm.types import (
    CarryMask,
    Segment,
    mask_for_width,
    mask_to_segments,
    segments_to_mask,
    unlimited,
    width_for_mask,
)

from .utils_segment import parse_segment

offsets = integers(min_value=0, max_value=100)
"""
Strategy for generating bit offsets.
We must limit the maximum offset to avoid huge bitmasks.
"""

width = one_of(offsets, just(unlimited))
"""
Strategy for generating bitstring widths.
"""

segments: SearchStrategy[Segment] = builds(Segment, start=offsets, width=width)
"""
Strategy for generating bit index segments.
"""

register_type_strategy(Segment, segments)


@dataclass(frozen=True)
class MaskedValue:
    """
    A bitmask and a value that falls within that mask.
    """

    mask: int
    value: int


@composite
def masked_value(draw: DrawFn) -> MaskedValue:
    mask = draw(integers())
    value = draw(integers()) & mask
    return MaskedValue(mask, value)


register_type_strategy(MaskedValue, masked_value())


def test_mask_for_width() -> None:
    """Test mask_for_width function."""
    assert mask_for_width(0) == 0x000
    assert mask_for_width(1) == 0x001
    assert mask_for_width(7) == 0x07F
    assert mask_for_width(8) == 0x0FF
    assert mask_for_width(9) == 0x1FF
    assert mask_for_width(unlimited) == -1


def test_width_for_mask() -> None:
    """Test width_for_mask function."""
    # Full masks.
    assert width_for_mask(0x000) == 0
    assert width_for_mask(0x001) == 1
    assert width_for_mask(0x07F) == 7
    assert width_for_mask(0x0FF) == 8
    assert width_for_mask(0x1FF) == 9
    assert width_for_mask(-1) == unlimited
    # Masks with holes.
    assert width_for_mask(0xC7) == 8
    assert width_for_mask(0x80) == 8
    assert width_for_mask(-16) == unlimited


def test_mask_to_segments() -> None:
    """Test mask_to_segments function."""

    def to_segs(mask: int) -> list[str]:
        return [str(segment) for segment in mask_to_segments(mask)]

    assert to_segs(0x0000) == []
    assert to_segs(0x0003) == ["[:2]"]
    assert to_segs(0x0078) == ["[3:7]"]
    assert to_segs(0xF7DE) == ["[1:5]", "[6:11]", "[12:16]"]
    assert to_segs(-1) == ["[:]"]
    assert to_segs(-16) == ["[4:]"]
    assert to_segs(-64 ^ 0x1C00) == ["[6:10]", "[13:]"]


def test_segments_to_mask() -> None:
    """Test segments_to_mask function."""

    def to_mask(*segments: str) -> int:
        return segments_to_mask(parse_segment(segment) for segment in segments)

    # Segments from mask_to_segments test.
    assert to_mask() == 0x0000
    assert to_mask("[0:2]") == 0x0003
    assert to_mask("[3:7]") == 0x0078
    assert to_mask("[1:5]", "[6:11]", "[12:16]") == 0xF7DE
    assert to_mask("[:]") == -1
    assert to_mask("[4:]") == -16
    assert to_mask("[6:10]", "[13:]") == -64 ^ 0x1C00

    # Segments that mask_to_segments won't return.
    assert to_mask("[6:11]", "[12:16]", "[1:5]") == 0xF7DE
    assert to_mask("[0:0]", "[9:9]") == 0x0000
    assert to_mask("[6:13]", "[3:8]") == 0x1FF8


@given(mask=infer)
def test_mask_to_segments_properties(mask: int) -> None:
    segments = list(mask_to_segments(mask))

    # Round trip preserves value.
    assert segments_to_mask(segments) == mask

    # Segments are non-empty.
    for seg in segments:
        assert seg.width > 0

    # Segments are ascending and don't touch.
    for seg1, seg2 in pairwise(segments):
        assert seg1.end < seg2.start


def test_segment_validation() -> None:
    """Segment construction refuses out-of-range arguments."""

    with raises(ValueError, match=r"^Segment start cannot be negative: -5$"):
        Segment(-5, 2)
    with raises(ValueError, match=r"^Segment width cannot be negative: -8$"):
        Segment(0, -8)

    with raises(ValueError, match=r"^Segment start cannot be negative: -1$"):
        replace(Segment(12, 4), start=-1)
    with raises(ValueError, match=r"^Segment width cannot be negative: -16$"):
        replace(Segment(3, 7), width=-16)


def test_segment_cut() -> None:
    """A segment can be used to slice bits from an integer."""

    assert parse_segment("[:12]").cut(0x1234) == 0x234
    assert parse_segment("[4:12]").cut(0x1234) == 0x23
    assert parse_segment("[4:]").cut(0x1234) == 0x123
    assert parse_segment("[4:]").cut(-64) == -4


def test_segment_shift() -> None:
    """Segments can be shifted left or right."""

    assert str(parse_segment("[3:7]") << 5) == "[8:12]"
    assert str(parse_segment("[4:]") << 10) == "[14:]"

    assert str(parse_segment("[8:12]") >> 5) == "[3:7]"
    assert str(parse_segment("[14:]") >> 10) == "[4:]"

    assert str(parse_segment("[3:7]") >> 6) == "[0]"
    assert str(parse_segment("[:8]") >> 2) == "[:6]"
    assert str(parse_segment("[4:]") >> 10) == "[:]"


@given(segment=infer, offset=offsets)
def test_segment_lshift_diamond(segment: Segment, offset: int) -> None:
    """
    The mask of a shifted segment is equal to the shifted mask of the original segment.
    """
    shifted = segment << offset
    note(f"Shifted segment: {shifted}")
    assert shifted.mask == segment.mask << offset


@given(segment=infer, offset=offsets)
def test_segment_rshift_diamond(segment: Segment, offset: int) -> None:
    """
    The mask of a shifted segment is equal to the shifted mask of the original segment.
    """
    shifted = segment >> offset
    note(f"Shifted segment: {shifted}")
    assert shifted.mask == segment.mask >> offset


def test_segment_intersect() -> None:
    """Segments can be intersected using the '&' operator."""

    assert str(parse_segment("[3:7]") & parse_segment("[6:9]")) == "[6]"
    assert str(parse_segment("[3:10]") & parse_segment("[5:]")) == "[5:10]"

    assert (parse_segment("[:4]") & parse_segment("[8:]")).width == 0
    assert not (parse_segment("[:4]") & parse_segment("[8:]"))


@given(segment1=infer, segment2=infer)
def test_segment_intersect_diamond(segment1: Segment, segment2: Segment) -> None:
    """
    The mask of a segment intersection is equal to the intersection of the masks.
    """
    assert (segment1 & segment2).mask == segment1.mask & segment2.mask


@given(mask=infer)
def test_carry_mask_round_trip(mask: int) -> None:
    """
    A binary pattern mask converted to `CarryMask` and back to integer remains the same.
    """
    assert CarryMask.from_pattern(mask).pattern == mask


@given(mask=infer, offset=offsets)
def test_carry_mask_lshift(mask: int, offset: int) -> None:
    """
    Left shift of a `CarryMask` is equivalent to that of a pattern mask.
    """
    assert (CarryMask.from_pattern(mask) << offset).pattern == mask << offset


@given(mask=infer, offset=offsets)
def test_carry_mask_rshift(mask: int, offset: int) -> None:
    """
    Right shift of a `CarryMask` is equivalent to that of a pattern mask.
    """
    assert (CarryMask.from_pattern(mask) >> offset).pattern == mask >> offset


@given(a=infer, b=infer)
def test_carry_mask_add_values(a: MaskedValue, b: MaskedValue) -> None:
    """
    The result of adding two values fits into the additiion of the values' masks.
    """
    value = a.value + b.value
    mask = CarryMask.from_pattern(a.mask) + CarryMask.from_pattern(b.mask)
    assert value & mask.pattern == value


@given(mask1=infer, mask2=infer)
def test_carry_mask_add_symmetry(mask1: int, mask2: int) -> None:
    """Carry mask addition is symmetrical."""
    cm1 = CarryMask.from_pattern(mask1)
    cm2 = CarryMask.from_pattern(mask2)
    add12 = cm1 + cm2
    add21 = cm2 + cm1
    assert add12 == add21
