from retroasm.utils import const_property

import pytest


class DataType:

    def __init__(self, value):
        self.value = value
        self.calls = 0

    @const_property
    def prop(self):
        'This is the docstring for prop.'
        self.calls += 1
        return self.value

class SlotsDataType:
    __slots__ = ('value', 'calls', '_prop')

    def __init__(self, value):
        self.value = value
        self.calls = 0

    @const_property
    def prop(self):
        'This is the docstring for prop.'
        self.calls += 1
        return self.value

def test_no_get():
    '''Test that nothing happens unless the property is read.'''
    obj = DataType(12345)
    assert obj.calls == 0

def test_get():
    '''Test that getter is evaluated only once.'''
    obj = DataType(12345)
    assert obj.calls == 0

    assert obj.prop == 12345
    assert obj.calls == 1

    assert obj.prop == 12345
    assert obj.calls == 1

    assert obj.prop == 12345
    assert obj.calls == 1

def test_get_multiple_instances():
    '''Test that every instance has its own cached value.'''
    obj1 = DataType(123)
    obj2 = DataType(456)
    assert obj1.calls == 0
    assert obj2.calls == 0

    assert obj1.prop == 123
    assert obj1.calls == 1
    assert obj2.calls == 0

    assert obj1.prop == 123
    assert obj1.calls == 1
    assert obj2.calls == 0

    assert obj2.prop == 456
    assert obj1.calls == 1
    assert obj2.calls == 1

    assert obj2.prop == 456
    assert obj1.calls == 1
    assert obj2.calls == 1

def test_get_slots():
    '''Test getting a value from a class that defines __slots__.'''
    obj = SlotsDataType(12345)
    assert obj.calls == 0

    assert obj.prop == 12345
    assert obj.calls == 1

    assert obj.prop == 12345
    assert obj.calls == 1

    assert obj.prop == 12345
    assert obj.calls == 1

def test_readonly_list_value():
    '''Test that the returned value is converted to a read-only type.'''
    data = (1, 2, 3)
    obj = DataType(list(data))
    assert obj.prop == data
    with pytest.raises(AttributeError):
        obj.prop.append(4)
    assert obj.prop == data

def test_readonly_set_value():
    '''Test that the returned value is converted to a read-only type.'''
    data = frozenset((1, 2, 3))
    obj = DataType(set(data))
    assert obj.prop == data
    with pytest.raises(AttributeError):
        obj.prop.add(4)
    assert obj.prop == data

def test_readonly_map_value():
    '''Test that the returned value is converted to a read-only type.'''
    data = {1: 'a', 2: 'b', 3: 'c'}
    obj = DataType(dict(data))
    assert dict(obj.prop) == data
    with pytest.raises(TypeError):
        obj.prop[4] = 'd'
    assert dict(obj.prop) == data

def test_readonly_iter_value():
    '''Test that the returned value is converted to a read-only type.'''
    data = tuple(range(10))
    obj = DataType(iter(data))
    assert obj.prop == data
    with pytest.raises(AttributeError):
        obj.prop.append(10)
    assert obj.prop == data

def test_readonly_property():
    '''Test setting and deleting of the property.'''
    obj = DataType(12345)

    with pytest.raises(AttributeError):
        obj.prop = 678
    with pytest.raises(AttributeError):
        del obj.prop

    assert obj.prop == 12345

    with pytest.raises(AttributeError):
        obj.prop = 678
    with pytest.raises(AttributeError):
        del obj.prop

    assert obj.prop == 12345

    with pytest.raises(AttributeError):
        obj.prop = 678
    with pytest.raises(AttributeError):
        del obj.prop

def test_get_class():
    '''Test that wrapper can be accessed via class.'''
    assert isinstance(DataType.prop, const_property)

def test_docstring():
    '''Test whether the docstring is copied.'''
    assert DataType.prop.__doc__ == \
        'This is the docstring for prop.'
