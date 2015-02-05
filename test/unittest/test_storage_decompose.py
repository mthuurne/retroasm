from retroasm.storage import (
    ComposedStorage, Concatenation, ReferencedValue, Slice
    )
from retroasm.types import IntType

import unittest

class DecomposeTests(unittest.TestCase):

    def assertDecomposed(self, storage, expected):
        i = 0
        for actualItem in ComposedStorage.decompose(storage):
            try:
                expectedItem = expected[i]
            except IndexError:
                self.fail(
                    'ComposedStorage produced more than the %d expected '
                    'items' % len(expected)
                    )
                self.fail('execution continues after fail')
            else:
                i += 1
            self.assertEqual(actualItem, expectedItem)
        if i < len(expected):
            self.fail(
                'ComposedStorage produced only %d of the %d expected items'
                % (i, len(expected))
                )

    def test_value(self):
        '''Checks that ReferencedValues are yielded whole.'''
        storage = ReferencedValue(0, IntType(8))
        expected = (
            (0, 0, 8),
            )
        self.assertDecomposed(storage, expected)

    def test_basic_concat(self):
        '''Checks Concatenation values and offsets.'''
        ref0 = ReferencedValue(0, IntType(7))
        ref1 = ReferencedValue(1, IntType(3))
        ref2 = ReferencedValue(2, IntType(13))
        storage = Concatenation((ref0, ref1, ref2))
        expected = (
            (2, 0, 13),
            (1, 0, 3),
            (0, 0, 7),
            )
        self.assertDecomposed(storage, expected)

    def test_basic_slice(self):
        '''Checks Slice range and offset.'''
        ref0 = ReferencedValue(0, IntType(8))
        storage = Slice(ref0, 2, 3)
        expected = (
            (0, 2, 3),
            )
        self.assertDecomposed(storage, expected)

    def test_slice_past_end(self):
        '''Checks clipping of Slice width against storage width.'''
        ref0 = ReferencedValue(0, IntType(8))
        storage = Slice(ref0, 2, 30)
        expected = (
            (0, 2, 6),
            )
        self.assertDecomposed(storage, expected)

    def test_slice_concat(self):
        '''Checks slicing concatenated values.'''
        ref0 = ReferencedValue(0, IntType(8))
        ref1 = ReferencedValue(1, IntType(8))
        ref2 = ReferencedValue(2, IntType(8))
        concat = Concatenation((ref0, ref1, ref2))
        storage = Slice(concat, 5, 13)
        expected = (
            (2, 5, 3),
            (1, 0, 8),
            (0, 0, 2),
            )
        self.assertDecomposed(storage, expected)

    def test_combined(self):
        '''Checks combinations of slicing and concatenation.'''
        ref0 = ReferencedValue(0, IntType(8))
        ref1 = ReferencedValue(1, IntType(8))
        concatA = Concatenation((ref1, ref0))
        sliceA = Slice(concatA, 5, 6)
        ref2 = ReferencedValue(2, IntType(8))
        concatB = Concatenation((ref2, sliceA))
        storage = Slice(concatB, 4, 7)
        expected = (
            (1, 1, 2),
            (2, 0, 5),
            )
        self.assertDecomposed(storage, expected)

if __name__ == '__main__':
    unittest.main()
