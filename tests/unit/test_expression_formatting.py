from utils_expression import TestValue

from retroasm.expression import RShift, truncate
from retroasm.expression_simplifier import simplifyExpression
from retroasm.storage import Variable
from retroasm.types import IntType

import unittest

class FormatTests(unittest.TestCase):

    def test_slice(self):
        '''Formats slice expressions.'''
        addr = TestValue('A', IntType.u(16))
        # Truncation and shift in isolation.
        self.assertEqual(str(simplifyExpression(truncate(addr, 8))), 'A[:8]')
        self.assertEqual(str(simplifyExpression(RShift(addr, 8))), 'A[8:]')
        # Truncation and shift combined.
        self.assertEqual(
            str(simplifyExpression(truncate(RShift(addr, 0), 8))), 'A[:8]'
            )
        self.assertEqual(
            str(simplifyExpression(truncate(RShift(addr, 4), 8))), 'A[4:12]'
            )
        self.assertEqual(
            str(simplifyExpression(truncate(RShift(addr, 8), 8))), 'A[8:]'
            )

    def test_index(self):
        '''Formats bit index expressions.'''
        addr = TestValue('A', IntType.u(16))
        self.assertEqual(str(simplifyExpression(truncate(addr, 1))), 'A[0]')
        self.assertEqual(
            str(simplifyExpression(truncate(RShift(addr, 11), 1))), 'A[11]'
            )
        self.assertEqual(
            str(simplifyExpression(RShift(addr, 15))), 'A[15]'
            )

if __name__ == '__main__':
    unittest.main()
