from retroasm.utils import Unique

import unittest

class DataType(metaclass=Unique):

    def __init__(self, a):
        self.a = a

class UniqueTests(unittest.TestCase):

    def assertData(self, data, a):
        assert isinstance(data, DataType)
        assert data.a == a

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
        assert d1 is d3
        assert d2 is d5
        assert d1 is not d2
        assert d1 is not d4
        assert d2 is not d4

    def test_star(self):
        '''Test passing arguments using the '*' operator.'''
        d1 = DataType(*(123,))
        d2 = DataType(*('foo',))
        d3 = DataType(*(120 + 3,))
        d4 = DataType(*(None,))
        d5 = DataType(*('f' + 'oo',))
        assert d1 is d3
        assert d2 is d5
        assert d1 is not d2
        assert d1 is not d4
        assert d2 is not d4

if __name__ == '__main__':
    unittest.main()
