from retroasm.expression import Slice
from retroasm.storage import Variable
from retroasm.types import IntType

import unittest

class FormatTests(unittest.TestCase):

    def test_slice(self):
        '''Formats slice expressions.'''
        addr = Variable('A', IntType(16))
        self.assertEquals(str(Slice(addr, 0, 8).simplify()), 'A[:8]')
        self.assertEquals(str(Slice(addr, 4, 8).simplify()), 'A[4:12]')
        self.assertEquals(str(Slice(addr, 8, 8).simplify()), 'A[8:]')

if __name__ == '__main__':
    unittest.main()
