from __future__ import annotations

from bisect import bisect
from collections.abc import Iterable, Iterator
from dataclasses import dataclass
from enum import Enum
from itertools import chain
from operator import itemgetter
from typing import ClassVar, Never, NoReturn, cast, override

from .utils import Unique


class Unlimited(Enum):
    """
    Width value for arbitrary-width integer types.
    Compares as infinity: larger than any integer.
    """

    instance = "unlimited"
    """Singleton instance."""

    @override
    def __repr__(self) -> str:
        return "unlimited"

    @override
    def __str__(self) -> str:
        return "unlimited"

    def __lt__(self, other: int | Unlimited) -> bool:
        if self is other:
            return False
        elif isinstance(other, int):
            return False
        else:
            return NotImplemented  # type: ignore[unreachable]

    def __le__(self, other: int | Unlimited) -> bool:
        if self is other:
            return True
        elif isinstance(other, int):
            return False
        else:
            return NotImplemented  # type: ignore[unreachable]

    def __gt__(self, other: int | Unlimited) -> bool:
        if self is other:
            return False
        elif isinstance(other, int):
            return True
        else:
            return NotImplemented  # type: ignore[unreachable]

    def __ge__(self, other: int | Unlimited) -> bool:
        if self is other:
            return True
        elif isinstance(other, int):
            return True
        else:
            return NotImplemented  # type: ignore[unreachable]

    def __add__(self, other: int | Unlimited) -> Unlimited:
        if self is other:
            return self
        elif isinstance(other, int):
            return self
        else:
            return NotImplemented  # type: ignore[unreachable]

    def __radd__(self, other: int | Unlimited) -> Unlimited:
        if self is other:
            return self
        elif isinstance(other, int):
            return self
        else:
            return NotImplemented  # type: ignore[unreachable]

    def __sub__(self, other: int) -> Unlimited:
        if isinstance(other, int):
            return self
        else:
            return NotImplemented  # type: ignore[unreachable]

    def __rsub__(self, other: Never) -> NoReturn:
        raise ArithmeticError('Cannot subtract "unlimited"')


unlimited = Unlimited.instance

type Width = int | Unlimited


def mask_for_width(width: Width) -> int:
    return -1 if width is unlimited else (1 << width) - 1


def width_for_mask(mask: int) -> Width:
    return unlimited if mask < 0 else mask.bit_length()


def trailing_zeroes(n: int) -> Width:
    if n == 0:
        return unlimited
    else:
        return (n ^ (n - 1)).bit_length() - 1


def is_power_of_two(n: int) -> bool:
    return n > 0 and n & (n - 1) == 0


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

    @override
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


class CarryMask:
    """
    A more detailed bitmask for the results of addition and multiplication.

    By keeping track of how many carries can occur, we can get a bitmask that is
    tighter fit than a regular binary bitmask.

    For example, take an addition of two 8-bit values and a 1-bit value.
    The result will be at most 9 bits wide, but the binary bitmask computation
    does not take into account that bit 0 can only overflow once in this addition
    and would therefore produce a mask that is 10 bits wide.
    """

    @classmethod
    def from_pattern(cls, mask: int) -> CarryMask:
        events = []
        state = 0
        idx = 0
        while True:
            level = mask & 1
            if level != state:
                events.append((idx, level))
                state = level
            if mask in (0, -1):
                # Level won't change again.
                break
            mask >>= 1
            idx += 1
        return cls(events)

    @staticmethod
    def _merge_events(
        events1: Iterator[tuple[int, int]], events2: Iterator[tuple[int, int]]
    ) -> Iterator[tuple[int, int]]:
        sentinel = (unlimited, 0)
        next_id1, next_level1 = next(events1, sentinel)
        next_id2, next_level2 = next(events2, sentinel)
        level1 = level2 = 0
        while next_id1 is not unlimited or next_id2 is not unlimited:
            if next_id1 < next_id2:
                level1 = next_level1
                yield cast(int, next_id1), level1 + level2
                next_id1, next_level1 = next(events1, sentinel)
            elif next_id1 > next_id2:
                level2 = next_level2
                yield cast(int, next_id2), level1 + level2
                next_id2, next_level2 = next(events2, sentinel)
            else:
                level1 = next_level1
                level2 = next_level2
                yield cast(int, next_id1), level1 + level2
                next_id1, next_level1 = next(events1, sentinel)
                next_id2, next_level2 = next(events2, sentinel)

    def __init__(self, events: Iterable[tuple[int, int]]):
        """
        Do not call this directly; use `from_pattern()` instead.
        """

        self._events = tuple(events)
        """
        A sequence of offset-level pairs, with increasing offsets, that denotes
        the maximum number of times each particular bit can occur in the summed
        values. The level applies from the index that it is paired with until
        the index of the next event.
        """

    @override
    def __eq__(self, other: object) -> bool:
        if isinstance(other, CarryMask):
            return self._events == other._events
        else:
            return NotImplemented

    @override
    def __hash__(self) -> int:
        return hash(self._events)

    @override
    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self._events})"

    def __bool__(self) -> bool:
        """
        Return False if this mask is empty (only matches the number zero),
        True otherwise.
        """
        return bool(self._events)

    def __lshift__(self, offset: int) -> CarryMask:
        return CarryMask((idx + offset, level) for idx, level in self._events)

    def __rshift__(self, offset: int) -> CarryMask:
        events = self._events
        cutoff = bisect(events, offset, key=itemgetter(0))
        shifted = ((idx - offset, level) for idx, level in events[cutoff:])
        if cutoff == 0 or (level := events[cutoff - 1][1]) == 0:
            return CarryMask(shifted)
        else:
            # Non-zero-level span ends at or below index zero after shift;
            # truncate it at index zero.
            return CarryMask(chain(((0, level),), shifted))

    def __add__(self, other: object) -> CarryMask:
        if isinstance(other, CarryMask):
            return CarryMask(self._merge_events(iter(self._events), iter(other._events)))
        else:
            return NotImplemented

    @property
    def pattern(self) -> int:
        """
        The smallest binary bitmask pattern that matches all values matched by
        this mask.
        """
        idx = 0
        pattern = 0
        count = 0
        state = 0
        for next_idx, next_state in self._events:
            while idx < next_idx:
                count += state
                if count == 0:
                    break
                pattern |= 1 << idx
                count //= 2
                idx += 1
            idx = next_idx
            state = next_state
        return pattern | (-1 if state else (1 << count.bit_length()) - 1) << idx


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
    def mask(self) -> int:
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

    @override
    def __repr__(self) -> str:
        return f"IntType({self._width}, {self._signed})"

    @override
    def __str__(self) -> str:
        if self._signed:
            if self._width is unlimited:
                return "int"
            else:
                return f"s{self._width:d}"
        else:
            if self._width is unlimited:
                return "uint"
            else:
                return f"u{self._width:d}"

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
            elif -1 << (width - 1) <= value < 1 << (width - 1):
                return
        else:
            if width is unlimited:
                if value >= 0:
                    return
            elif 0 <= value < 1 << width:
                return
        raise ValueError(f"value {value:d} does not fit in type {self}")

    # https://github.com/pylint-dev/pylint/issues/9950
    int: ClassVar[IntType]  # pylint: disable=E0245


IntType.int = IntType(unlimited, True)


class ReferenceType(metaclass=Unique):
    """A reference to a value of a certain type."""

    __slots__ = ("_type", "__weakref__")

    @property
    def type(self) -> IntType:
        return self._type

    def __init__(self, typ: IntType):
        self._type = typ

    @override
    def __repr__(self) -> str:
        return f"ReferenceType({self._type!r})"

    @override
    def __str__(self) -> str:
        return f"{self._type}&"


def parse_type(type_name: str) -> IntType:
    if type_name == "int":
        return IntType.int
    if type_name.startswith(("u", "s")):
        width_str = type_name[1:]
        if width_str.isdigit():
            return IntType(int(width_str), type_name.startswith("s"))
    raise ValueError(f'"{type_name}" is not a valid type name')


def parse_type_decl(type_decl: str) -> IntType | ReferenceType:
    if type_decl.endswith("&"):
        return ReferenceType(parse_type(type_decl[:-1]))
    else:
        return parse_type(type_decl)
