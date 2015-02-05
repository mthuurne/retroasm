from retroasm.storage import (
    Concatenation, FixedValue, ReferencedValue, Slice, decomposeStorage
    )
from retroasm.types import IntType

import unittest

class DecomposeTests(unittest.TestCase):

    def assertDecomposed(self, storage, expected):
        i = 0
        for actualItem in decomposeStorage(storage):
            try:
                expectedItem = expected[i]
            except IndexError:
                self.fail(
                    'decomposeStorage() produced more than the %d expected '
                    'items' % len(expected)
                    )
                self.fail('execution continues after fail')
            else:
                i += 1
            self.assertEqual(actualItem, expectedItem)
        if i < len(expected):
            self.fail(
                'decomposeStorage() produced only %d of the %d expected items'
                % (i, len(expected))
                )

    def test_fixed_value(self):
        '''Checks that FixedValues are yielded whole.'''
        storage = FixedValue(0, IntType(8))
        expected = (
            (storage, 0, 8),
            )
        self.assertDecomposed(storage, expected)

    def test_referenced_value(self):
        '''Checks that ReferencedValues are yielded whole.'''
        storage = ReferencedValue(0, IntType(8))
        expected = (
            (storage, 0, 8),
            )
        self.assertDecomposed(storage, expected)

    def test_basic_concat(self):
        '''Checks Concatenation values and offsets.'''
        ref0 = ReferencedValue(0, IntType(7))
        fix0 = FixedValue(0, IntType(3))
        ref1 = ReferencedValue(1, IntType(13))
        storage = Concatenation((ref0, fix0, ref1))
        expected = (
            (ref1, 0, 13),
            (fix0, 0, 3),
            (ref0, 0, 7),
            )
        self.assertDecomposed(storage, expected)

    def test_basic_slice(self):
        '''Checks Slice range and offset.'''
        ref0 = ReferencedValue(0, IntType(8))
        storage = Slice(ref0, 2, 3)
        expected = (
            (ref0, 2, 3),
            )
        self.assertDecomposed(storage, expected)

    def test_slice_past_end(self):
        '''Checks clipping of Slice width against storage width.'''
        ref0 = ReferencedValue(0, IntType(8))
        storage = Slice(ref0, 2, 30)
        expected = (
            (ref0, 2, 6),
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
            (ref2, 5, 3),
            (ref1, 0, 8),
            (ref0, 0, 2),
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
            (ref1, 1, 2),
            (ref2, 0, 5),
            )
        self.assertDecomposed(storage, expected)

if __name__ == '__main__':
    unittest.main()
