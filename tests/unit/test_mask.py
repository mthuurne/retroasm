from retroasm.types import (
    maskForWidth, maskToSegments, segmentsToMask, widthForMask, unlimited
    )

import unittest

class MaskTests(unittest.TestCase):

    def test_maskForWidth(self):
        '''Test maskForWidth function.'''
        self.assertEqual(maskForWidth(0), 0x000)
        self.assertEqual(maskForWidth(1), 0x001)
        self.assertEqual(maskForWidth(7), 0x07F)
        self.assertEqual(maskForWidth(8), 0x0FF)
        self.assertEqual(maskForWidth(9), 0x1FF)
        self.assertEqual(maskForWidth(unlimited), -1)

    def test_widthForMask(self):
        '''Test widthForMask function.'''
        # Full masks.
        self.assertEqual(widthForMask(0x000), 0)
        self.assertEqual(widthForMask(0x001), 1)
        self.assertEqual(widthForMask(0x07F), 7)
        self.assertEqual(widthForMask(0x0FF), 8)
        self.assertEqual(widthForMask(0x1FF), 9)
        self.assertEqual(widthForMask(-1), unlimited)
        # Masks with holes.
        self.assertEqual(widthForMask(0xC7), 8)
        self.assertEqual(widthForMask(0x80), 8)
        self.assertEqual(widthForMask(-16), unlimited)

    def test_maskToSegments(self):
        '''Test maskToSegments function.'''
        self.assertSequenceEqual(list(maskToSegments(0x0000)), ())
        self.assertSequenceEqual(list(maskToSegments(0x0003)), ((0, 2),))
        self.assertSequenceEqual(list(maskToSegments(0x0078)), ((3, 7),))
        self.assertSequenceEqual(list(maskToSegments(0xF7DE)), ((1, 5), (6, 11), (12, 16)))
        self.assertSequenceEqual(list(maskToSegments(-1)), ((0, unlimited),))
        self.assertSequenceEqual(list(maskToSegments(-16)), ((4, unlimited),))
        self.assertSequenceEqual(list(maskToSegments(-64 ^ 0x1C00)), ((6, 10), (13, unlimited)))

    def test_segmentsToMask(self):
        '''Test segmentsToMask function.'''
        # Segments from maskToSegments test.
        self.assertEqual(segmentsToMask(()), 0x0000)
        self.assertEqual(segmentsToMask(((0, 2),)), 0x0003)
        self.assertEqual(segmentsToMask(((3, 7),)), 0x0078)
        self.assertEqual(segmentsToMask(((1, 5), (6, 11), (12, 16))), 0xF7DE)
        self.assertEqual(segmentsToMask(((0, unlimited),)), -1)
        self.assertEqual(segmentsToMask(((4, unlimited),)), -16)
        self.assertEqual(segmentsToMask(((6, 10), (13, unlimited))), -64 ^ 0x1C00)
        # Segments that maskToSegments won't return.
        self.assertEqual(segmentsToMask(((6, 11), (12, 16), (1, 5))), 0xF7DE)
        self.assertEqual(segmentsToMask(((0, 0), (9, 9))), 0x0000)
        self.assertEqual(segmentsToMask(((6, 13), (3, 8))), 0x1FF8)

if __name__ == '__main__':
    unittest.main()
