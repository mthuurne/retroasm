from retroasm.utils import Unique


class DataType(metaclass=Unique):
    def __init__(self, a):
        self.a = a


def assertData(data, a):
    assert isinstance(data, DataType)
    assert data.a == a


def test_create() -> None:
    """Test that construction creates correct objects."""
    assertData(DataType(0), 0)
    assertData(DataType(123), 123)
    assertData(DataType(None), None)
    assertData(DataType("foo"), "foo")


def test_id() -> None:
    """Test that construction with same arguments returns same object."""
    d1 = DataType(123)
    d2 = DataType("foo")
    d3 = DataType(120 + 3)
    d4 = DataType(None)
    d5 = DataType("f" + "oo")
    assert d1 is d3
    assert d2 is d5
    assert d1 is not d2
    assert d1 is not d4
    assert d2 is not d4


def test_star() -> None:
    """Test passing arguments using the '*' operator."""
    d1 = DataType(*(123,))
    d2 = DataType(*("foo",))
    d3 = DataType(*(120 + 3,))
    d4 = DataType(*(None,))
    d5 = DataType(*("f" + "oo",))
    assert d1 is d3
    assert d2 is d5
    assert d1 is not d2
    assert d1 is not d4
    assert d2 is not d4
