from hypothesis import given, infer

from retroasm.types import (
    mask_for_width,
    mask_to_segments,
    segments_to_mask,
    unlimited,
    width_for_mask,
)

from .utils_segment import parse_segment


def test_mask_for_width():
    """Test mask_for_width function."""
    assert mask_for_width(0) == 0x000
    assert mask_for_width(1) == 0x001
    assert mask_for_width(7) == 0x07F
    assert mask_for_width(8) == 0x0FF
    assert mask_for_width(9) == 0x1FF
    assert mask_for_width(unlimited) == -1


def test_width_for_mask():
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


def test_mask_to_segments():
    """Test mask_to_segments function."""

    def to_segs(mask):
        return [str(segment) for segment in mask_to_segments(mask)]

    assert to_segs(0x0000) == []
    assert to_segs(0x0003) == ["[:2]"]
    assert to_segs(0x0078) == ["[3:7]"]
    assert to_segs(0xF7DE) == ["[1:5]", "[6:11]", "[12:16]"]
    assert to_segs(-1) == ["[:]"]
    assert to_segs(-16) == ["[4:]"]
    assert to_segs(-64 ^ 0x1C00) == ["[6:10]", "[13:]"]


def test_segments_to_mask():
    """Test segments_to_mask function."""

    def to_mask(*segments):
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
    # TODO: Python 3.10 has itertools.pairwise().
    for idx in range(len(segments) - 1):
        assert segments[idx].end < segments[idx + 1].start


def test_segment_cut():
    """A segment can be used to slice bits from an integer."""

    assert parse_segment("[:12]").cut(0x1234) == 0x234
    assert parse_segment("[4:12]").cut(0x1234) == 0x23
    assert parse_segment("[4:]").cut(0x1234) == 0x123
    assert parse_segment("[4:]").cut(-64) == -4


def test_segment_shift():
    """Segments can be shifted left or right."""

    assert str(parse_segment("[3:7]") << 5) == "[8:12]"
    assert str(parse_segment("[4:]") << 10) == "[14:]"

    assert str(parse_segment("[8:12]") >> 5) == "[3:7]"
    assert str(parse_segment("[14:]") >> 10) == "[4:]"

    assert str(parse_segment("[3:7]") >> 6) == "[0]"
    assert str(parse_segment("[:8]") >> 2) == "[:6]"
    assert str(parse_segment("[4:]") >> 10) == "[:]"


def test_segment_intersect():
    """Segments can be intersected using the '&' operator."""

    assert str(parse_segment("[3:7]") & parse_segment("[6:9]")) == "[6]"
    assert str(parse_segment("[3:10]") & parse_segment("[5:]")) == "[5:10]"

    assert (parse_segment("[:4]") & parse_segment("[8:]")).width == 0
    assert not (parse_segment("[:4]") & parse_segment("[8:]"))
