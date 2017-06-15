from retroasm.utils import const_property

import unittest

class DataType:

    def __init__(self, value):
        self.value = value
        self.calls = 0

    @const_property
    def prop(self):
        'This is the docstring for prop.'
        self.calls += 1
        return self.value

class ConstPropertyTests(unittest.TestCase):

    def test_no_get(self):
        '''Test that nothing happens unless the property is read.'''
        obj = DataType(12345)
        self.assertEqual(obj.calls, 0)

    def test_get(self):
        '''Test that getter is evaluated only once.'''
        obj = DataType(12345)
        self.assertEqual(obj.calls, 0)

        self.assertEqual(obj.prop, 12345)
        self.assertEqual(obj.calls, 1)

        self.assertEqual(obj.prop, 12345)
        self.assertEqual(obj.calls, 1)

        self.assertEqual(obj.prop, 12345)
        self.assertEqual(obj.calls, 1)

    def test_get_multiple_instances(self):
        '''Test that every instance has its own cached value.'''
        obj1 = DataType(123)
        obj2 = DataType(456)
        self.assertEqual(obj1.calls, 0)
        self.assertEqual(obj2.calls, 0)

        self.assertEqual(obj1.prop, 123)
        self.assertEqual(obj1.calls, 1)
        self.assertEqual(obj2.calls, 0)

        self.assertEqual(obj1.prop, 123)
        self.assertEqual(obj1.calls, 1)
        self.assertEqual(obj2.calls, 0)

        self.assertEqual(obj2.prop, 456)
        self.assertEqual(obj1.calls, 1)
        self.assertEqual(obj2.calls, 1)

        self.assertEqual(obj2.prop, 456)
        self.assertEqual(obj1.calls, 1)
        self.assertEqual(obj2.calls, 1)

    def test_readonly_property(self):
        '''Test setting and deleting of the property.'''
        obj = DataType(12345)

        with self.assertRaises(AttributeError):
            obj.prop = 678
        with self.assertRaises(AttributeError):
            del obj.prop

        self.assertEqual(obj.prop, 12345)

        with self.assertRaises(AttributeError):
            obj.prop = 678
        with self.assertRaises(AttributeError):
            del obj.prop

        self.assertEqual(obj.prop, 12345)

        with self.assertRaises(AttributeError):
            obj.prop = 678
        with self.assertRaises(AttributeError):
            del obj.prop

    def test_get_class(self):
        '''Test that wrapper can be accessed via class.'''
        self.assertIsInstance(DataType.prop, const_property)

    def test_docstring(self):
        '''Test whether the docstring is copied.'''
        self.assertEqual(
            DataType.prop.__doc__,
            'This is the docstring for prop.'
            )

if __name__ == '__main__':
    unittest.main()
