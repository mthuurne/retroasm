from utils_expression import TestValue

from retroasm.expression import RShift, Truncation
from retroasm.expression_simplifier import simplifyExpression
from retroasm.storage import Variable
from retroasm.types import IntType

import unittest

class FormatTests(unittest.TestCase):

    def test_slice(self):
        '''Formats slice expressions.'''
        addr = TestValue('A', IntType(16))
        # Truncation and shift in isolation.
        self.assertEquals(str(simplifyExpression(Truncation(addr, 8))), 'A[:8]')
        self.assertEquals(str(simplifyExpression(RShift(addr, 8))), 'A[8:]')
        # Truncation and shift combined.
        self.assertEquals(
            str(simplifyExpression(Truncation(RShift(addr, 0), 8))), 'A[:8]'
            )
        self.assertEquals(
            str(simplifyExpression(Truncation(RShift(addr, 4), 8))), 'A[4:12]'
            )
        self.assertEquals(
            str(simplifyExpression(Truncation(RShift(addr, 8), 8))), 'A[8:]'
            )

if __name__ == '__main__':
    unittest.main()
