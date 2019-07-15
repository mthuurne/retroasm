from __future__ import annotations

from typing import TYPE_CHECKING, Iterable, Iterator, Tuple, Union, cast

from .utils import Singleton, Unique


class Unlimited(metaclass=Singleton):
    '''Width value for arbitrary-width integer types.
    Compares as infinity: larger than any integer.
    '''
    __slots__ = ()

    def __repr__(self) -> str:
        return 'unlimited'

    def __str__(self) -> str:
        return 'unlimited'

    def __hash__(self) -> int:
        return super().__hash__()

    def __eq__(self, other: object) -> bool:
        if isinstance(other, (int, Unlimited)):
            return self is other
        else:
            return NotImplemented

    def __ne__(self, other: object) -> bool:
        if isinstance(other, (int, Unlimited)):
            return self is not other
        else:
            return NotImplemented

    def __lt__(self, other: object) -> bool:
        if isinstance(other, (int, Unlimited)):
            return False
        else:
            return NotImplemented

    def __le__(self, other: object) -> bool:
        if isinstance(other, (int, Unlimited)):
            return self is other
        else:
            return NotImplemented

    def __gt__(self, other: object) -> bool:
        if isinstance(other, (int, Unlimited)):
            return self is not other
        else:
            return NotImplemented

    def __ge__(self, other: object) -> bool:
        if isinstance(other, (int, Unlimited)):
            return True
        else:
            return NotImplemented

    def __add__(self, other: object) -> Unlimited:
        if isinstance(other, (int, Unlimited)):
            return self
        else:
            return NotImplemented

    __radd__ = __add__

    def __sub__(self, other: object) -> Unlimited:
        if isinstance(other, int):
            return self
        else:
            return NotImplemented

    def __rsub__(self, other: int) -> None:
        return NotImplemented

unlimited = Unlimited()

Width = Union[int, Unlimited]

def maskForWidth(width: Width) -> int:
    return -1 if width is unlimited else (1 << cast(int, width)) - 1

def widthForMask(mask: int) -> Width:
    return unlimited if mask < 0 else mask.bit_length()

def trailingZeroes(n: int) -> Width:
    if n == 0:
        return unlimited
    count = 0
    while (n >> count) & 1 == 0:
        count += 1
    return count

Segment = Tuple[int, Width]

def maskToSegments(mask: int) -> Iterator[Segment]:
    '''Iterates through pairs of start and end indices of maximally long
    segments of consecutive set bits in the given mask.
    The segments are returned in increasing order.
    Negative masks are supported, the last segment will have 'unlimited' as
    its end in that case.
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

def segmentsToMask(segments: Iterable[Segment]) -> int:
    '''Computes a mask that corresponds to the given sequence of pairs of
    start and end indices.
    Overlapping or empty segments are allowed, start larger than end is not.
    End indices may be 'unlimited', start indices may not.
    '''
    mask = 0
    for start, end in segments:
        mask |= (
            -1 if end is unlimited else (1 << (cast(int, end) - start)) - 1
            ) << start
    return mask

class IntType(metaclass=Unique):
    '''An integer value type of "width" bits, signed or unsigned.
    Width can be an integer or the singleton 'unlimited', which indicates an
    unlimited width integer type.
    There is at most one instance of IntType for each width + signedness,
    so instances can be compared using the "is" operator.
    '''
    __slots__ = ('_width', '_signed', '__weakref__')

    @property
    def width(self) -> Width:
        return self._width

    @property
    def signed(self) -> bool:
        return self._signed

    @property
    def mask(self) -> int:
         return -1 if self._signed else maskForWidth(self._width)

    @classmethod
    def u(cls, width: Width) -> IntType:
        '''Creates an unsigned integer type of the given width.
        '''
        return cls(width, False)

    @classmethod
    def s(cls, width: Width) -> IntType:
        '''Creates a signed integer type of the given width.
        '''
        return cls(width, True)

    def __init__(self, width: Width, signed: bool):
        if isinstance(width, int):
            if width < 0:
                raise ValueError('width must not be negative: %d' % width)
        elif width is not unlimited:
            raise TypeError(
                'width must be integer or unlimited, got %s' % type(width)
                )
        self._width = width
        self._signed = signed

    def __repr__(self) -> str:
        return 'IntType(%s, %s)' % (self._width, self._signed)

    def __str__(self) -> str:
        return 'int' if self._width is unlimited else '%s%d' % (
            's' if self._signed else 'u', cast(int, self._width)
            )

    if TYPE_CHECKING:
        int = IntType(unlimited, True)

IntType.int = IntType(unlimited, True)

class ReferenceType(metaclass=Unique):
    '''A reference to a value of a certain type.
    '''
    __slots__ = ('_type', '__weakref__')

    @property
    def type(self) -> IntType:
        return self._type

    def __init__(self, typ: IntType):
        self._type = typ

    def __repr__(self) -> str:
        return 'ReferenceType(%r)' % self._type

    def __str__(self) -> str:
        return '%s&' % self._type

def parseType(typeName: str) -> IntType:
    if typeName == 'int':
        return IntType.int
    if typeName.startswith('u') or typeName.startswith('s'):
        widthStr = typeName[1:]
        if widthStr.isdigit():
            return IntType(int(widthStr), typeName.startswith('s'))
    raise ValueError('"%s" is not a valid type name' % typeName)

def parseTypeDecl(typeDecl: str) -> Union[IntType, ReferenceType]:
    if typeDecl.endswith('&'):
        return ReferenceType(parseType(typeDecl[:-1]))
    else:
        return parseType(typeDecl)
