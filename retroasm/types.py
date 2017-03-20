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

def trailingZeroes(n):
    if n == 0:
        return unlimited
    count = 0
    while (n >> count) & 1 == 0:
        count += 1
    return count

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
    '''An integer value type of "width" bits, signed or unsigned.
    Width can be an integer or the singleton 'unlimited', which indicates an
    unlimited width integer type.
    There is at most one instance of IntType for each width + signedness,
    so instances can be compared using the "is" operator.
    '''
    __slots__ = ('_width', '_signed', '__weakref__')

    width = property(lambda self: self._width)
    signed = property(lambda self: self._signed)
    mask = property(
         lambda self: -1 if self._signed else maskForWidth(self._width)
         )

    @classmethod
    def u(cls, width):
        '''Creates an unsigned integer type of the given width.
        '''
        return cls(width, False)

    @classmethod
    def s(cls, width):
        '''Creates a signed integer type of the given width.
        '''
        return cls(width, True)

    def __init__(self, width, signed):
        if isinstance(width, int):
            if width < 0:
                raise ValueError('width must not be negative: %d' % width)
        elif width is not unlimited:
            raise TypeError(
                'width must be integer or unlimited, got %s' % type(width)
                )
        self._width = width
        self._signed = signed

    def __repr__(self):
        return 'IntType(%s, %s)' % (self._width, self._signed)

    def __str__(self):
        return 'int' if self._width is unlimited else '%s%d' % (
            's' if self._signed else 'u', self._width
            )

IntType.int = IntType(unlimited, True)

class Reference(metaclass=Unique):
    '''A reference to a value of a certain type.
    '''
    __slots__ = ('_type', '__weakref__')

    type = property(lambda self: self._type)

    def __init__(self, typ):
        self._type = typ

    def __repr__(self):
        return 'Reference(%r)' % self._type

    def __str__(self):
        return '%s&' % self._type

def parseType(typeName):
    if typeName == 'int':
        return IntType.int
    if typeName.startswith('u') or typeName.startswith('s'):
        widthStr = typeName[1:]
        if widthStr.isdigit():
            return IntType(int(widthStr), typeName.startswith('s'))
    raise ValueError('"%s" is not a valid type name' % typeName)

def parseTypeDecl(typeDecl):
    if typeDecl.endswith('&'):
        return Reference(parseType(typeDecl[:-1]))
    else:
        return parseType(typeDecl)
