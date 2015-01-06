from .utils import Singleton, Unique

class Unlimited(metaclass=Singleton):
    '''Width value for arbitrary-width integer types.
    Compares as infinity: larger than any integer.
    '''
    __slots__ = ()

    def __repr__(self):
        return 'unlimited'

    def __str__(self):
        return 'unlimited'

    def __hash__(self):
        return super().__hash__()

    def __eq__(self, other):
        if isinstance(other, (int, Unlimited)):
            return self is other
        else:
            return NotImplemented

    def __ne__(self, other):
        if isinstance(other, (int, Unlimited)):
            return self is not other
        else:
            return NotImplemented

    def __lt__(self, other):
        if isinstance(other, (int, Unlimited)):
            return False
        else:
            return NotImplemented

    def __le__(self, other):
        if isinstance(other, (int, Unlimited)):
            return self is other
        else:
            return NotImplemented

    def __gt__(self, other):
        if isinstance(other, (int, Unlimited)):
            return self is not other
        else:
            return NotImplemented

    def __ge__(self, other):
        if isinstance(other, (int, Unlimited)):
            return True
        else:
            return NotImplemented

unlimited = Unlimited()

class IntType(metaclass=Unique):
    '''An integer value type of "width" bits.
    Width can be an integer or the singleton 'unlimited', which indicates an
    unlimited width integer type.
    There is at most one instance of IntType for each width, so instances can
    be compared using the "is" operator.
    '''
    __slots__ = ('_width', '__weakref__')

    width = property(lambda self: self._width)

    def __init__(self, width):
        if isinstance(width, int):
            if width < 0:
                raise ValueError('width must not be negative: %d' % width)
        elif width is not unlimited:
            raise TypeError(
                'width must be integer or unlimited, got %s' % type(width)
                )
        self._width = width

    def __repr__(self):
        return 'IntType(%s)' % self._width

    def __str__(self):
        return 'int' if self._width is unlimited else 'u%d' % self._width
