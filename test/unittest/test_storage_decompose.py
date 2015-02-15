from retroasm.storage import ComposedStorage, Storage, sliceStorage
from retroasm.types import IntType
from retroasm.utils import checkType

import unittest

class ReferencedValue(Storage):
    '''A value in a storage location accessed through a reference.
    '''
    __slots__ = ('_rid',)

    rid = property(lambda self: self._rid)

    def __init__(self, rid, typ):
        Storage.__init__(self, typ)
        self._rid = rid

    def __repr__(self):
        return 'ReferencedValue(%d, %s)' % (self._rid, repr(self._type))

    def __str__(self):
        return 'R%d' % self._rid

class Concatenation:
    '''Concatenates the bit strings of storages.
    '''
    __slots__ = ('_exprs',)

    exprs = property(lambda self: self._exprs)
    width = property(lambda self: sum(expr.width for expr in self._exprs))
    type = property(lambda self: IntType(self.width))

    def __init__(self, exprs):
        self._exprs = exprs = tuple(exprs)
        for expr in exprs:
            if not isinstance(expr, (Concatenation, Slice, Storage)):
                raise TypeError(
                    'expected storage, got %s' % type(expr).__name__
                    )

    def __repr__(self):
        return 'Concatenation(%s)' % ', '.join(
            repr(expr) for expr in self._exprs
            )

    def __str__(self):
        return '(%s)' % ' ; '.join(str(expr) for expr in self._exprs)

class Slice:
    '''Slices the bit strings of a storage.
    '''
    __slots__ = ('_expr', '_index', '_width')

    expr = property(lambda self: self._expr)
    index = property(lambda self: self._index)
    width = property(lambda self: self._width)
    type = property(lambda self: IntType(self.width))

    def __init__(self, expr, index, width):
        self._expr = expr
        self._index = checkType(index, int, 'slice index')
        self._width = checkType(width, int, 'slice width')
        if index < 0:
            raise ValueError('slice index must not be negative: %d' % index)
        if width < 0:
            raise ValueError('slice width must not be negative: %d' % width)

    def __repr__(self):
        return 'Slice(%s, %d, %d)' % (
            repr(self._expr), self._index, self._width
            )

    def __str__(self):
        return '%s[%d:%d]' % (
            self._expr, self._index, self._index + self._width
            )

def _decomposeStorage(storage):
    '''Iterates through the basic storages inside the given composed storage.
    Each element is a triple of a reference ID and the start index and the width
    of the slice of the storage that is affected.
    '''
    if isinstance(storage, Concatenation):
        for concatTerm in reversed(storage.exprs):
            yield from _decomposeStorage(concatTerm)
    elif isinstance(storage, Slice):
        yield from sliceStorage(
            _decomposeStorage(storage.expr), storage.index, storage.width
            )
    else:
        assert isinstance(storage, ReferencedValue), repr(storage)
        yield storage.rid, 0, storage.width

class DecomposeTests(unittest.TestCase):

    def assertDecomposed(self, storage, expected):
        i = 0
        for actualItem in ComposedStorage(_decomposeStorage(storage)):
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
