from __future__ import annotations

from collections.abc import Iterable, Iterator
from enum import Enum, auto

from .asm_directives import StructuredData
from .types import Unlimited, Width, unlimited
from .utils import search


class ByteOrder(Enum):
    undefined = auto()
    little = auto()
    big = auto()


class Section:
    """
    Base class for area in a binary with shared properties.

    Start (inclusive) and end (exclusive) are offsets into the image.
    It is allowed to define a section with offsets outside of the image.
    """

    @property
    def start(self) -> int:
        return self._start

    @property
    def end(self) -> int | Unlimited:
        return self._end

    @property
    def size(self) -> Width:
        return self._end - self._start

    @property
    def description(self) -> str:
        return self._description

    def __init__(self, start: int, end: int | Unlimited, description: str = "data"):
        if start < 0:
            raise ValueError(f"negative start: {start:d}")
        if end < 0:
            raise ValueError(f"negative end: {end:d}")
        if end < start:
            raise ValueError(f"end ({end:#x}) before start ({start:#x})")

        self._start = start
        self._end = end
        self._description = description

    def __repr__(self) -> str:
        end = self._end
        if end is unlimited:
            return f"Section({self._start:#x}, unlimited)"
        else:
            return f"Section({self._start:#x}, {end:#x})"

    def __str__(self) -> str:
        end = self._end
        if isinstance(end, Unlimited):
            return f"[{self._start:#x}..)"
        else:
            return f"[{self._start:#x}..{end:#x})"

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Section):
            return self._start == other._start and self._end == other._end
        else:
            return NotImplemented

    def __hash__(self) -> int:
        return hash((self._start, self._end))


class StructuredDataSection(Section):
    """Section that consists entirely of structured data."""

    @property
    def data(self) -> StructuredData:
        return self._data

    def __init__(self, offset: int, data: StructuredData, description: str = "struct"):
        super().__init__(offset, offset + len(data), description)
        self._data = data


class CodeSection(Section):
    """Section that contains code and possibly also data."""

    @property
    def instr_set_name(self) -> str:
        return self._instr_set_name

    @property
    def byte_order(self) -> ByteOrder:
        return self._byte_order

    @property
    def base(self) -> int:
        return self._base

    def __init__(
        self,
        start: int,
        end: int | Unlimited,
        base: int,
        instr_set_name: str,
        byte_order: ByteOrder,
        description: str = "code",
    ):
        super().__init__(start, end, description)
        self._instr_set_name = instr_set_name
        self._byte_order = byte_order
        self._base = base

        if base < 0:
            raise ValueError(f"negative base: {base:d}")

    def __repr__(self) -> str:
        end = self._end
        end_repr = "unlimited" if end is unlimited else f"{end:#x}"
        return (
            f"CodeSection({self._start:#x}, {end_repr}, "
            f"{self._base:#x}, {self._instr_set_name!r}, "
            f"ByteOrder.{self._byte_order.name})"
        )

    def offset_for_addr(self, addr: int) -> int:
        """
        Return the offset in the image at which the given address can be found.

        Raises ValueError if the given address is outside this section.
        """
        start = self._start
        offset = start + addr - self._base
        if start <= offset < self._end:
            return offset
        else:
            raise ValueError(f"address outside section: {addr:#x}")


class SectionMap:
    """
    A collection of sections.

    Sections cannot overlap, but there can be gaps between sections.
    """

    def __init__(self, sections: Iterable[Section]):
        sections = sorted(sections, key=lambda section: section.start)
        prev: Section | None = None
        for section in sections:
            if prev is not None and section.start < prev.end:
                raise ValueError(f"section {prev} overlaps section {section}")
            prev = section
        self._sections = sections

    def __iter__(self) -> Iterator[Section]:
        """Iterate through the sections, in address order."""
        return iter(self._sections)

    def __len__(self) -> int:
        return len(self._sections)

    def __repr__(self) -> str:
        return f"SectionMap({self._sections!r})"

    def section_at(self, offset: int) -> Section | None:
        """
        Return the section at the given offset, or None if there is no section
        at that offset.
        """
        sections = self._sections

        def after(i: int) -> bool:
            return sections[i].start > offset

        idx = search(0, len(sections), after)
        if idx != 0:
            section = sections[idx - 1]
            if section.start <= offset < section.end:
                return section
        return None
