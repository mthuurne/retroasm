from retroasm.types import (
    maskForWidth, maskToSegments, segmentsToMask, widthForMask, unlimited
)

from .utils_segment import parse_segment


def test_maskForWidth():
    """Test maskForWidth function."""
    assert maskForWidth(0) == 0x000
    assert maskForWidth(1) == 0x001
    assert maskForWidth(7) == 0x07F
    assert maskForWidth(8) == 0x0FF
    assert maskForWidth(9) == 0x1FF
    assert maskForWidth(unlimited) == -1

def test_widthForMask():
    """Test widthForMask function."""
    # Full masks.
    assert widthForMask(0x000) == 0
    assert widthForMask(0x001) == 1
    assert widthForMask(0x07F) == 7
    assert widthForMask(0x0FF) == 8
    assert widthForMask(0x1FF) == 9
    assert widthForMask(-1) == unlimited
    # Masks with holes.
    assert widthForMask(0xC7) == 8
    assert widthForMask(0x80) == 8
    assert widthForMask(-16) == unlimited

def test_maskToSegments():
    """Test maskToSegments function."""

    def to_segs(mask):
        return [str(segment) for segment in maskToSegments(mask)]

    assert to_segs(0x0000) == []
    assert to_segs(0x0003) == ['[:2]']
    assert to_segs(0x0078) == ['[3:7]']
    assert to_segs(0xF7DE) == ['[1:5]', '[6:11]', '[12:16]']
    assert to_segs(-1) == ['[:]']
    assert to_segs(-16) == ['[4:]']
    assert to_segs(-64 ^ 0x1C00) == ['[6:10]', '[13:]']

def test_segmentsToMask():
    """Test segmentsToMask function."""

    def to_mask(*segments):
        return segmentsToMask(parse_segment(segment) for segment in segments)

    # Segments from maskToSegments test.
    assert to_mask() == 0x0000
    assert to_mask('[0:2]') == 0x0003
    assert to_mask('[3:7]') == 0x0078
    assert to_mask('[1:5]', '[6:11]', '[12:16]') == 0xF7DE
    assert to_mask('[:]') == -1
    assert to_mask('[4:]') == -16
    assert to_mask('[6:10]', '[13:]') == -64 ^ 0x1C00

    # Segments that maskToSegments won't return.
    assert to_mask('[6:11]', '[12:16]', '[1:5]') == 0xF7DE
    assert to_mask('[0:0]', '[9:9]') == 0x0000
    assert to_mask('[6:13]', '[3:8]') == 0x1FF8

def test_segment_cut():
    """A segment can be used to slice bits from an integer."""

    assert parse_segment('[:12]').cut(0x1234) == 0x234
    assert parse_segment('[4:12]').cut(0x1234) == 0x23
    assert parse_segment('[4:]').cut(0x1234) == 0x123
    assert parse_segment('[4:]').cut(-64) == -4

def test_segment_shift():
    """Segments can be shifted left or right."""

    assert str(parse_segment('[3:7]') << 5) == '[8:12]'
    assert str(parse_segment('[4:]') << 10) == '[14:]'

    assert str(parse_segment('[8:12]') >> 5) == '[3:7]'
    assert str(parse_segment('[14:]') >> 10) == '[4:]'

    assert str(parse_segment('[3:7]') >> 6) == '[0]'
    assert str(parse_segment('[:8]') >> 2) == '[:6]'
    assert str(parse_segment('[4:]') >> 10) == '[:]'

def test_segment_intersect():
    """Segments can be intersected using the '&' operator."""

    assert str(parse_segment('[3:7]') & parse_segment('[6:9]')) == '[6]'
    assert str(parse_segment('[3:10]') & parse_segment('[5:]')) == '[5:10]'

    assert (parse_segment('[:4]') & parse_segment('[8:]')).width == 0
    assert not (parse_segment('[:4]') & parse_segment('[8:]'))
