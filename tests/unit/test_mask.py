from retroasm.types import (
    maskForWidth, maskToSegments, segmentsToMask, widthForMask, unlimited
    )

import unittest

class MaskTests(unittest.TestCase):

    def test_maskForWidth(self):
        '''Test maskForWidth function.'''
        assert maskForWidth(0) == 0x000
        assert maskForWidth(1) == 0x001
        assert maskForWidth(7) == 0x07F
        assert maskForWidth(8) == 0x0FF
        assert maskForWidth(9) == 0x1FF
        assert maskForWidth(unlimited) == -1

    def test_widthForMask(self):
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

    def test_maskToSegments(self):
        '''Test maskToSegments function.'''
        assert list(maskToSegments(0x0000)) == []
        assert list(maskToSegments(0x0003)) == [(0, 2)]
        assert list(maskToSegments(0x0078)) == [(3, 7)]
        assert list(maskToSegments(0xF7DE)) == [(1, 5), (6, 11), (12, 16)]
        assert list(maskToSegments(-1)) == [(0, unlimited)]
        assert list(maskToSegments(-16)) == [(4, unlimited)]
        assert list(maskToSegments(-64 ^ 0x1C00)) == [(6, 10), (13, unlimited)]

    def test_segmentsToMask(self):
        '''Test segmentsToMask function.'''
        # Segments from maskToSegments test.
        assert segmentsToMask(()) == 0x0000
        assert segmentsToMask(((0, 2),)) == 0x0003
        assert segmentsToMask(((3, 7),)) == 0x0078
        assert segmentsToMask(((1, 5), (6, 11), (12, 16))) == 0xF7DE
        assert segmentsToMask(((0, unlimited),)) == -1
        assert segmentsToMask(((4, unlimited),)) == -16
        assert segmentsToMask(((6, 10), (13, unlimited))) == -64 ^ 0x1C00
        # Segments that maskToSegments won't return.
        assert segmentsToMask(((6, 11), (12, 16), (1, 5))) == 0xF7DE
        assert segmentsToMask(((0, 0), (9, 9))) == 0x0000
        assert segmentsToMask(((6, 13), (3, 8))) == 0x1FF8

if __name__ == '__main__':
    unittest.main()
