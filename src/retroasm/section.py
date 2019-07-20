from enum import Enum
from typing import Iterable, Iterator, Optional

from .utils import checkType, search

ByteOrder = Enum('ByteOrder', ( # pylint: disable=invalid-name
    'undefined', 'little', 'big'
    ))

class Section:
    '''Base class for area in a binary with shared properties.
    Start (inclusive) and end (exclusive) are offsets into the image.
    It is allowed to define a section with offsets outside of the image.
    '''

    @property
    def start(self) -> int:
        return self._start

    @property
    def end(self) -> int:
        return self._end

    @property
    def size(self) -> int:
        return self._end - self._start

    def __init__(self, start: int, end: int):
        self._start = checkType(start, int, 'start offset')
        self._end = checkType(end, int, 'end offset')

        if start < 0:
            raise ValueError(f'negative start: {start:d}')
        if end < 0:
            raise ValueError(f'negative end: {end:d}')
        if end < start:
            raise ValueError(f'end (0x{end:x}) before start (0x{start:x})')

    def __repr__(self) -> str:
        return f'Section(0x{self._start:x}, 0x{self._end:x})'

    def __str__(self) -> str:
        return f'[0x{self._start:x}..0x{self._end:x})'

class CodeSection(Section):
    '''Section that contains code and possibly also data.
    '''

    @property
    def instrSetName(self) -> str:
        return self._instrSetName

    @property
    def byteOrder(self) -> ByteOrder:
        return self._byteOrder

    @property
    def base(self) -> int:
        return self._base

    def __init__(self,
                 start: int,
                 end: int,
                 base: int,
                 instrSetName: str,
                 byteOrder: ByteOrder
                 ):
        Section.__init__(self, start, end)
        self._instrSetName = checkType(
            instrSetName, str, 'instruction set name'
            )
        self._byteOrder = checkType(byteOrder, ByteOrder, 'byte order')
        self._base = checkType(base, int, 'base address')

        if base < 0:
            raise ValueError(f'negative base: {base:d}')

    def __repr__(self) -> str:
        return f'CodeSection(0x{self._start:x}, 0x{self._end:x}, ' \
                           f'0x{self._base:x}, {self._instrSetName!r}, ' \
                           f'ByteOrder.{self._byteOrder.name})'

    def offsetForAddr(self, addr: int) -> int:
        '''Returns the offset in the image at which the given address can be
        found.
        Raises ValueError if the given address is outside this section.
        '''
        start = self._start
        offset = start + addr - self._base
        if start <= offset < self._end:
            return offset
        else:
            raise ValueError(f'address outside section: 0x{addr:x}')

class SectionMap:
    '''A collection of sections.
    '''

    def __init__(self, sections: Iterable[Section]):
        sections = list(sections)
        for section in sections:
            checkType(section, Section, 'section')
        sections.sort(key=lambda section: section.start)
        prev: Optional[Section] = None
        for section in sections:
            if prev is not None and section.start < prev.end:
                raise ValueError(
                    f'section {prev} overlaps section {section}'
                    )
            prev = section
        self._sections = sections

    def __iter__(self) -> Iterator[Section]:
        return iter(self._sections)

    def __len__(self) -> int:
        return len(self._sections)

    def __repr__(self) -> str:
        return f'SectionMap({self._sections!r})'

    def sectionAt(self, offset: int) -> Optional[Section]:
        '''Returns the section at the given offset, or None if there is no
        section at that offset.
        '''
        sections = self._sections
        def after(i: int) -> bool:
            return sections[i].start > offset
        idx = search(0, len(sections), after)
        if idx != 0:
            section = sections[idx - 1]
            if section.start <= offset < section.end:
                return section
        return None
