from retroasm.expression import (
    IntType, LocalValue, createSlice, createSubtraction
    )

import unittest

class FormatTests(unittest.TestCase):

    def test_subtraction(self):
        '''Formats subtraction expressions.'''
        a = LocalValue('A', IntType(16))
        b = LocalValue('B', IntType(16))
        c = LocalValue('C', IntType(16))
        self.assertEquals(str(createSubtraction(a, b)), '(A - B)')
        self.assertEquals(str(createSubtraction(a, b, c)), '(A - B - C)')

    def test_slice(self):
        '''Formats slice expressions.'''
        addr = LocalValue('A', IntType(16))
        self.assertEquals(str(createSlice(addr, 0, 8).simplify()), 'A[:8]')
        self.assertEquals(str(createSlice(addr, 4, 8).simplify()), 'A[4:12]')
        self.assertEquals(str(createSlice(addr, 8, 8).simplify()), 'A[8:]')

if __name__ == '__main__':
    unittest.main()
