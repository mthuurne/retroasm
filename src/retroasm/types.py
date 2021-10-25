from __future__ import annotations

from dataclasses import dataclass
from typing import Iterable, Iterator, NoReturn, Union, cast

from .utils import Singleton, Unique


class DoesNotExist:
    """Used in type annotations when no type is allowed to match."""


class Unlimited(metaclass=Singleton):
    """
    Width value for arbitrary-width integer types.
    Compares as infinity: larger than any integer.
    """

    __slots__ = ()

    def __repr__(self) -> str:
        return "unlimited"

    def __str__(self) -> str:
        return "unlimited"

    def __lt__(self, other: int | Unlimited) -> bool:
        if self is other:
            return False
        elif isinstance(other, int):
            return False
        else:
            return NotImplemented

    def __le__(self, other: int | Unlimited) -> bool:
        if self is other:
            return True
        elif isinstance(other, int):
            return False
        else:
            return NotImplemented

    def __gt__(self, other: int | Unlimited) -> bool:
        if self is other:
            return False
        elif isinstance(other, int):
            return True
        else:
            return NotImplemented

    def __ge__(self, other: int | Unlimited) -> bool:
        if self is other:
            return True
        elif isinstance(other, int):
            return True
        else:
            return NotImplemented

    def __add__(self, other: int | Unlimited) -> Unlimited:
        if self is other:
            return self
        elif isinstance(other, int):
            return self
        else:
            return NotImplemented

    __radd__ = __add__

    def __sub__(self, other: int) -> Unlimited:
        if isinstance(other, int):
            return self
        else:
            return NotImplemented  # type: ignore[unreachable]

    def __rsub__(self, other: DoesNotExist) -> NoReturn:
        raise ArithmeticError('Cannot subtract "unlimited"')


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


@dataclass(order=True, frozen=True)
class Segment:
    __slots__ = ("start", "width")
    start: int
    width: Width

    def __new__(cls, start: int, width: Width) -> Segment:
        if start < 0:
            raise ValueError(f"Segment start cannot be negative: {start:d}")
        instance: Segment = super().__new__(cls)
        instance.__init__(start, width)  # type: ignore[misc]
        return instance

    def __str__(self) -> str:
        start = self.start
        width = self.width
        if width == 1:
            return f"[{start:d}]"
        start_str = "" if start == 0 else f"{start:d}"
        end_str = "" if isinstance(width, Unlimited) else f"{start + width:d}"
        return f"[{start_str}:{end_str}]"

    @property
    def end(self) -> Width:
        return self.start + self.width

    @property
    def mask(self) -> int:
        return maskForWidth(self.width) << self.start

    def cut(self, value: int) -> int:
        """Slice the bits in this segment from the given integer value."""
        return (value >> self.start) & maskForWidth(self.width)

    def __bool__(self) -> bool:
        """Return True iff the segment is non-empty."""
        return self.width != 0

    def __lshift__(self, offset: int) -> Segment:
        return Segment(self.start + offset, self.width)

    def __rshift__(self, offset: int) -> Segment:
        start = self.start - offset
        if start < 0:
            return Segment(0, self.width + start)
        else:
            return Segment(start, self.width)

    def __and__(self, other: object) -> Segment:
        """Intersect two segments."""
        if isinstance(other, Segment):
            start = max(self.start, other.start)
            end = min(self.end, other.end)
            width = max(end - start, 0)
            return Segment(start, width)
        else:
            return NotImplemented


def maskToSegments(mask: int) -> Iterator[Segment]:
    """
    Iterates through pairs of start and end indices of maximally long
    segments of consecutive set bits in the given mask.
    The segments are returned in increasing order.
    Negative masks are supported, the last segment will have 'unlimited' as
    its end in that case.
    """
    i = 0
    while True:
        if mask == 0:
            break
        while (mask & 1) == 0:
            i += 1
            mask >>= 1
        if mask == -1:
            yield Segment(i, unlimited)
            break
        start = i
        while (mask & 1) == 1:
            i += 1
            mask >>= 1
        yield Segment(start, i - start)


def segmentsToMask(segments: Iterable[Segment]) -> int:
    """
    Computes a mask that corresponds to the given sequence of pairs of
    start and end indices.
    Overlapping or empty segments are allowed, start larger than end is not.
    End indices may be 'unlimited', start indices may not.
    """
    mask = 0
    for segment in segments:
        mask |= segment.mask
    return mask


class IntType(metaclass=Unique):
    """
    An integer value type of "width" bits, signed or unsigned.
    Width can be an integer or the singleton 'unlimited', which indicates an
    unlimited width integer type.
    There is at most one instance of IntType for each width + signedness,
    so instances can be compared using the "is" operator.
    """

    __slots__ = ("_width", "_signed", "__weakref__")

    @property
    def width(self) -> Width:
        return self._width

    @property
    def signed(self) -> bool:
        return self._signed

    @property
    def mask(self) -> int:  # pylint: disable=undefined-variable
        return -1 if self._signed else maskForWidth(self._width)

    @classmethod
    def u(cls, width: Width) -> IntType:
        """Creates an unsigned integer type of the given width."""
        return cls(width, False)

    @classmethod
    def s(cls, width: Width) -> IntType:
        """Creates a signed integer type of the given width."""
        return cls(width, True)

    def __init__(self, width: Width, signed: bool):
        if isinstance(width, int):
            if width < 0:
                raise ValueError(f"width must not be negative: {width:d}")
        elif width is not unlimited:
            raise TypeError(f"width must be integer or unlimited, got {type(width)}")
        self._width = width
        self._signed = signed

    def __repr__(self) -> str:
        return f"IntType({self._width}, {self._signed})"

    def __str__(self) -> str:
        return (
            "int"
            if self._width is unlimited
            else f"{'s' if self._signed else 'u'}{cast(int, self._width):d}"
        )

    int: IntType


IntType.int = IntType(unlimited, True)


class ReferenceType(metaclass=Unique):
    """A reference to a value of a certain type."""

    __slots__ = ("_type", "__weakref__")

    @property
    def type(self) -> IntType:
        return self._type

    def __init__(self, typ: IntType):
        self._type = typ

    def __repr__(self) -> str:
        return f"ReferenceType({self._type!r})"

    def __str__(self) -> str:
        return f"{self._type}&"


def parseType(typeName: str) -> IntType:
    if typeName == "int":
        return IntType.int
    if typeName.startswith("u") or typeName.startswith("s"):
        widthStr = typeName[1:]
        if widthStr.isdigit():
            return IntType(int(widthStr), typeName.startswith("s"))
    raise ValueError(f'"{typeName}" is not a valid type name')


def parseTypeDecl(typeDecl: str) -> IntType | ReferenceType:
    if typeDecl.endswith("&"):
        return ReferenceType(parseType(typeDecl[:-1]))
    else:
        return parseType(typeDecl)
