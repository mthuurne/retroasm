from utils_expression import TestValue

from retroasm.expression import RShift, Truncation
from retroasm.storage import Variable
from retroasm.types import IntType

import unittest

class FormatTests(unittest.TestCase):

    def test_slice(self):
        '''Formats slice expressions.'''
        addr = TestValue('A', IntType(16))
        # Truncation and shift in isolation.
        self.assertEquals(str(Truncation(addr, 8).simplify()), 'A[:8]')
        self.assertEquals(str(RShift(addr, 8).simplify()), 'A[8:]')
        # Truncation and shift combined.
        self.assertEquals(
            str(Truncation(RShift(addr, 0), 8).simplify()), 'A[:8]'
            )
        self.assertEquals(
            str(Truncation(RShift(addr, 4), 8).simplify()), 'A[4:12]'
            )
        self.assertEquals(
            str(Truncation(RShift(addr, 8), 8).simplify()), 'A[8:]'
            )

if __name__ == '__main__':
    unittest.main()
