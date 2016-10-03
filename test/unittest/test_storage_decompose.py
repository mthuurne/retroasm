from retroasm.codeblock import BoundReference
from retroasm.types import IntType

import unittest

class DecomposeTests(unittest.TestCase):

    def assertDecomposed(self, storage, expected):
        self.assertIsInstance(storage, BoundReference)
        i = 0
        width = 0
        for actualItem in storage:
            try:
                expectedItem = expected[i]
            except IndexError:
                self.fail(
                    'BoundReference produced more than the %d expected items'
                    % len(expected)
                    )
            else:
                i += 1
            self.assertEqual(actualItem, expectedItem)
            width += actualItem[2]
        if i < len(expected):
            self.fail(
                'BoundReference produced only %d of the %d expected items'
                % (i, len(expected))
                )
        self.assertEqual(storage.width, width)

    def test_single(self):
        '''Test construction via BoundReference.single() class method.'''
        storage = BoundReference.single(IntType.u(8), 123)
        expected = (
            (123, 0, 8),
            )
        self.assertDecomposed(storage, expected)

    def test_basic_concat(self):
        '''Checks concatenated storages.'''
        ref0 = BoundReference.single(IntType.u(7), 0)
        ref1 = BoundReference.single(IntType.u(3), 1)
        ref2 = BoundReference.single(IntType.u(13), 2)
        storage = ref2.concat(ref1).concat(ref0)
        expected = (
            (2, 0, 13),
            (1, 0, 3),
            (0, 0, 7),
            )
        self.assertDecomposed(storage, expected)

    def test_basic_slice(self):
        '''Checks sliced storages.'''
        ref0 = BoundReference.single(IntType.u(8), 0)
        storage = ref0.slice(2, 3)
        expected = (
            (0, 2, 3),
            )
        self.assertDecomposed(storage, expected)

    def test_slice_past_end(self):
        '''Checks clipping of slice width against storage width.'''
        ref0 = BoundReference.single(IntType.u(8), 0)
        storage = ref0.slice(2, 30)
        expected = (
            (0, 2, 6),
            )
        self.assertDecomposed(storage, expected)

    def test_slice_outside(self):
        '''Checks handling of slice index outside storage width.'''
        ref0 = BoundReference.single(IntType.u(8), 0)
        storage = ref0.slice(12, 30)
        expected = (
            )
        self.assertDecomposed(storage, expected)

    def test_slice_concat(self):
        '''Checks slicing concatenated values.'''
        ref0 = BoundReference.single(IntType.u(8), 0)
        ref1 = BoundReference.single(IntType.u(8), 1)
        ref2 = BoundReference.single(IntType.u(8), 2)
        concat = ref2.concat(ref1).concat(ref0)
        storage = concat.slice(5, 13)
        expected = (
            (2, 5, 3),
            (1, 0, 8),
            (0, 0, 2),
            )
        self.assertDecomposed(storage, expected)

    def test_combined(self):
        '''Checks combinations of slicing and concatenation.'''
        ref0 = BoundReference.single(IntType.u(8), 0)
        ref1 = BoundReference.single(IntType.u(8), 1)
        concatA = ref0.concat(ref1)
        sliceA = concatA.slice(5, 6)
        ref2 = BoundReference.single(IntType.u(8), 2)
        concatB = sliceA.concat(ref2)
        storage = concatB.slice(4, 7)
        expected = (
            (1, 1, 2),
            (2, 0, 5),
            )
        self.assertDecomposed(storage, expected)

if __name__ == '__main__':
    unittest.main()
