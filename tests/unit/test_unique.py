from retroasm.utils import Unique

import unittest

class DataType(metaclass=Unique):

    def __init__(self, a):
        self.a = a

class UniqueTests(unittest.TestCase):

    def assertData(self, data, a):
        self.assertIsInstance(data, DataType)
        self.assertEqual(data.a, a)

    def test_create(self):
        '''Test that construction creates correct objects.'''
        self.assertData(DataType(0), 0)
        self.assertData(DataType(123), 123)
        self.assertData(DataType(None), None)
        self.assertData(DataType('foo'), 'foo')

    def test_id(self):
        '''Test that construction with same arguments returns same object.'''
        d1 = DataType(123)
        d2 = DataType('foo')
        d3 = DataType(120 + 3)
        d4 = DataType(None)
        d5 = DataType('f' + 'oo')
        self.assertIs(d1, d3)
        self.assertIs(d2, d5)
        self.assertIsNot(d1, d2)
        self.assertIsNot(d1, d4)
        self.assertIsNot(d2, d4)

    def test_star(self):
        '''Test passing arguments using the '*' operator.'''
        d1 = DataType(*(123,))
        d2 = DataType(*('foo',))
        d3 = DataType(*(120 + 3,))
        d4 = DataType(*(None,))
        d5 = DataType(*('f' + 'oo',))
        self.assertIs(d1, d3)
        self.assertIs(d2, d5)
        self.assertIsNot(d1, d2)
        self.assertIsNot(d1, d4)
        self.assertIsNot(d2, d4)

if __name__ == '__main__':
    unittest.main()
