from retroasm.types import (
    Segment, maskForWidth, maskToSegments, segmentsToMask, widthForMask,
    unlimited
    )


def test_maskForWidth():
    '''Test maskForWidth function.'''
    assert maskForWidth(0) == 0x000
    assert maskForWidth(1) == 0x001
    assert maskForWidth(7) == 0x07F
    assert maskForWidth(8) == 0x0FF
    assert maskForWidth(9) == 0x1FF
    assert maskForWidth(unlimited) == -1

def test_widthForMask():
    '''Test widthForMask function.'''
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
    '''Test maskToSegments function.'''
    assert list(maskToSegments(0x0000)) == []
    assert list(maskToSegments(0x0003)) == [Segment(0, 2)]
    assert list(maskToSegments(0x0078)) == [Segment(3, 7)]
    assert list(maskToSegments(0xF7DE)) == [
        Segment(1, 5), Segment(6, 11), Segment(12, 16)
        ]
    assert list(maskToSegments(-1)) == [Segment(0, unlimited)]
    assert list(maskToSegments(-16)) == [Segment(4, unlimited)]
    assert list(maskToSegments(-64 ^ 0x1C00)) == [
        Segment(6, 10), Segment(13, unlimited)
        ]

def test_segmentsToMask():
    '''Test segmentsToMask function.'''
    # Segments from maskToSegments test.
    assert segmentsToMask(()) == 0x0000
    assert segmentsToMask([Segment(0, 2)]) == 0x0003
    assert segmentsToMask([Segment(3, 7)]) == 0x0078
    assert segmentsToMask([
        Segment(1, 5), Segment(6, 11), Segment(12, 16)
        ]) == 0xF7DE
    assert segmentsToMask([Segment(0, unlimited)]) == -1
    assert segmentsToMask([Segment(4, unlimited)]) == -16
    assert segmentsToMask([
        Segment(6, 10), Segment(13, unlimited)
        ]) == -64 ^ 0x1C00
    # Segments that maskToSegments won't return.
    assert segmentsToMask([
        Segment(6, 11), Segment(12, 16), Segment(1, 5)
        ]) == 0xF7DE
    assert segmentsToMask([Segment(0, 0), Segment(9, 9)]) == 0x0000
    assert segmentsToMask([Segment(6, 13), Segment(3, 8)]) == 0x1FF8
