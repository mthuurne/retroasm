from .utils import Unique

class IntType(metaclass=Unique):
    '''An integer value type of "width" bits.
    Width can be None, which indicates an unlimited width integer type.
    There is at most one instance of IntType for each width, so instances can
    be compared using the "is" operator.
    '''
    __slots__ = ('_width', '__weakref__')

    width = property(lambda self: self._width)

    def __init__(self, width):
        if width is not None:
            if not isinstance(width, int):
                raise TypeError(
                    'width must be integer or None, got %s' % type(width)
                    )
            if width < 0:
                raise ValueError('width must not be negative: %d' % width)
        self._width = width

    def __repr__(self):
        return 'IntType(%s)' % self._width

    def __str__(self):
        return 'int' if self._width is None else 'u%d' % self._width

def minWidth(exprs):
    '''Returns the minimum of the widths of the given expressions.
    Unlimited width is considered larger than any fixed width.
    '''
    minSoFar = None
    for expr in exprs:
        width = expr.width
        if width is not None:
            minSoFar = width if minSoFar is None else min(minSoFar, width)
    return minSoFar

def maxWidth(exprs):
    '''Returns the maximum of the widths of the given expressions.
    Unlimited width is considered larger than any fixed width.
    '''
    maxSoFar = 0
    for expr in exprs:
        width = expr.width
        if width is None:
            return None
        maxSoFar = max(maxSoFar, width)
    return maxSoFar
