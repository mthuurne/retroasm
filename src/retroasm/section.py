from .utils import checkType, search

from enum import Enum

ByteOrder = Enum('ByteOrder', ( # pylint: disable=invalid-name
    'undefined', 'little', 'big'
    ))

class Section:
    '''Base class for area in a binary with shared properties.
    Start (inclusive) and end (exclusive) are offsets into the image.
    It is allowed to define a section with offsets outside of the image.
    '''

    start = property(lambda self: self._start)
    end = property(lambda self: self._end)
    size = property(lambda self: self._end - self._start)

    def __init__(self, start, end):
        self._start = checkType(start, int, 'start offset')
        self._end = checkType(end, int, 'end offset')

        if start < 0:
            raise ValueError('negative start: %d' % start)
        if end < 0:
            raise ValueError('negative end: %d' % end)
        if end < start:
            raise ValueError('end (0x%x) before start (0x%x)' % (end, start))

    def __repr__(self):
        return 'Section(0x%x, 0x%x)' % (self._start, self._end)

    def __str__(self):
        return '[0x%x..0x%x)' % (self._start, self._end)

class CodeSection(Section):
    '''Section that contains code and possibly also data.
    '''

    instrSetName = property(lambda self: self._instrSetName)
    byteOrder = property(lambda self: self._byteOrder)
    base = property(lambda self: self._base)

    def __init__(self, start, end, base, instrSetName, byteOrder):
        Section.__init__(self, start, end)
        self._instrSetName = checkType(
            instrSetName, str, 'instruction set name'
            )
        self._byteOrder = checkType(byteOrder, ByteOrder, 'byte order')
        self._base = checkType(base, int, 'base address')

        if base < 0:
            raise ValueError('negative base: %d' % base)

    def __repr__(self):
        return 'CodeSection(0x%x, 0x%x, 0x%x, %r, ByteOrder.%s)' % (
            self._start, self._end, self._base, self._instrSetName,
            self._byteOrder.name
            )

    def offsetForAddr(self, addr):
        '''Returns the offset in the image at which the given address can be
        found.
        Raises ValueError if the given address is outside this section.
        '''
        start = self._start
        offset = start + addr - self._base
        if start <= offset < self._end:
            return offset
        else:
            raise ValueError('address outside section: 0x%x' % addr)

class SectionMap:
    '''A collection of sections.
    '''

    def __init__(self, sections):
        sections = list(sections)
        for section in sections:
            checkType(section, Section, 'section')
        sections.sort(key=lambda section: section.start)
        prev = None
        for section in sections:
            if prev is not None and section.start < prev.end:
                raise ValueError(
                    'section %s overlaps section %s' % (prev, section)
                    )
            prev = section
        self._sections = sections

    def __iter__(self):
        return iter(self._sections)

    def __len__(self):
        return len(self._sections)

    def __repr__(self):
        return 'SectionMap(%r)' % self._sections

    def sectionAt(self, offset):
        '''Returns the section at the given offset, or None if there is no
        section at that offset.
        '''
        sections = self._sections
        idx = search(
            0, len(sections),
            lambda i, sections=sections: sections[i].start > offset
            )
        if idx != 0:
            section = sections[idx - 1]
            if section.start <= offset < section.end:
                return section
        return None
