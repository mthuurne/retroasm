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

    def __add__(self, other):
        if isinstance(other, (int, Unlimited)):
            return self
        else:
            return NotImplemented

    __radd__ = __add__

    def __sub__(self, other):
        if isinstance(other, int):
            return self
        else:
            return NotImplemented

    def __rsub__(self, other):
        return NotImplemented

unlimited = Unlimited()

def maskForWidth(width):
    return -1 if width is unlimited else (1 << width) - 1

def widthForMask(mask):
    return unlimited if mask < 0 else mask.bit_length()

def maskToSegments(mask):
    '''Iterates through pairs of start and end indices of maximally long
    segments of consecutive set bits in the given mask.
    '''
    i = 0
    while True:
        if mask == 0:
            break
        while (mask & 1) == 0:
            i += 1
            mask >>= 1
        if mask == -1:
            yield i, unlimited
            break
        start = i
        while (mask & 1) == 1:
            i += 1
            mask >>= 1
        yield start, i

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

class Reference(metaclass=Unique):
    '''A reference to a value of a certain type.
    '''
    __slots__ = ('_type', '__weakref__')

    type = property(lambda self: self._type)

    def __init__(self, typ):
        self._type = typ

    def __repr__(self):
        return 'Reference(%s)' % repr(self._type)

    def __str__(self):
        return '%s&' % self._type

def parseType(typeName):
    if typeName == 'int':
        return IntType(unlimited)
    if not typeName.startswith('u'):
        raise ValueError('type name "%s" does not start with "u"' % typeName)
    widthStr = typeName[1:]
    if not widthStr.isdigit():
        raise ValueError(
            'integer type "%s" is not of the form "u<width>"' % typeName
            )
    return IntType(int(widthStr))

def parseTypeDecl(typeDecl):
    if typeDecl.endswith('&'):
        return Reference(parseType(typeDecl[:-1]))
    else:
        return parseType(typeDecl)
