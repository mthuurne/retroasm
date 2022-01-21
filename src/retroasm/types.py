from __future__ import annotations

from collections.abc import Iterable, Iterator
from dataclasses import dataclass
from typing import NoReturn, Union, cast

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


def mask_for_width(width: Width) -> int:
    return -1 if width is unlimited else (1 << cast(int, width)) - 1


def width_for_mask(mask: int) -> Width:
    return unlimited if mask < 0 else mask.bit_length()


def trailing_zeroes(n: int) -> Width:
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

    def __post_init__(self) -> None:
        if self.start < 0:
            raise ValueError(f"Segment start cannot be negative: {self.start:d}")
        if self.width < 0:
            raise ValueError(f"Segment width cannot be negative: {self.width:d}")

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
        return mask_for_width(self.width) << self.start

    def cut(self, value: int) -> int:
        """Slice the bits in this segment from the given integer value."""
        return (value >> self.start) & mask_for_width(self.width)

    def __bool__(self) -> bool:
        """Return True iff the segment is non-empty."""
        return self.width != 0

    def __lshift__(self, offset: int) -> Segment:
        return Segment(self.start + offset, self.width)

    def __rshift__(self, offset: int) -> Segment:
        start = self.start - offset
        if start < 0:
            return Segment(0, max(self.width + start, 0))
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


def mask_to_segments(mask: int) -> Iterator[Segment]:
    """
    Yield maximally long disjunct segments corresponding to consecutive set bits
    in the given mask.
    The segments are returned in increasing order.
    Negative masks are supported, the last segment will have width 'unlimited'
    in that case.
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


def segments_to_mask(segments: Iterable[Segment]) -> int:
    """Compute a mask that corresponds to the union of the given segments."""
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
        return -1 if self._signed else mask_for_width(self._width)

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
        if self._signed:
            if self._width is unlimited:
                return "int"
            else:
                return f"s{cast(int, self._width):d}"
        else:
            if self._width is unlimited:
                return "uint"
            else:
                return f"u{cast(int, self._width):d}"

    def check_range(self, value: int) -> None:
        """
        Check whether the given value fits within this type.

        Raises ValueError if the value does not fit.
        """
        width = self._width
        if self._signed:
            if width is unlimited:
                return
            elif width == 0:
                if value == 0:
                    return
            elif -1 << (cast(int, width) - 1) <= value < 1 << (cast(int, width) - 1):
                return
        else:
            if width is unlimited:
                if value >= 0:
                    return
            elif 0 <= value < 1 << cast(int, width):
                return
        raise ValueError(f"value {value:d} does not fit in type {self}")

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


def parse_type(type_name: str) -> IntType:
    if type_name == "int":
        return IntType.int
    if type_name.startswith("u") or type_name.startswith("s"):
        width_str = type_name[1:]
        if width_str.isdigit():
            return IntType(int(width_str), type_name.startswith("s"))
    raise ValueError(f'"{type_name}" is not a valid type name')


def parse_type_decl(type_decl: str) -> IntType | ReferenceType:
    if type_decl.endswith("&"):
        return ReferenceType(parse_type(type_decl[:-1]))
    else:
        return parse_type(type_decl)
