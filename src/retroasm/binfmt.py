from logging import getLogger
from pathlib import PurePath
from struct import Struct
from typing import (
    Any, ClassVar, Collection, Iterable, Iterator, NamedTuple, Optional,
    Protocol, Sequence, Type, overload
)

from .section import ByteOrder, CodeSection, Section

logger = getLogger('binfmt')

class Image(Protocol):
    """A binary image.

    `mmap.mmap` implements this interface, while it does not implement
    `ByteString` because it doesn't provide `__contains__`.
    """

    @overload
    def __getitem__(self, index: int) -> int: ...
    @overload
    def __getitem__(self, index: slice) -> bytes: ...
    def __len__(self) -> int: ...

class EntryPoint:
    '''A point at which execution can start.
    '''

    @property
    def offset(self) -> int:
        return self._offset

    @property
    def label(self) -> Optional[str]:
        return self._label

    def __init__(self, offset: int, label: Optional[str] = None):
        self._offset = offset
        self._label = label

        if offset < 0:
            raise ValueError(f'negative offset: {offset:d}')

    def __repr__(self) -> str:
        return f'EntryPoint({self._offset:#x}, {self._label!r})'

    def __str__(self) -> str:
        label = self._label
        if label is None:
            return f'{self._offset:#x}'
        else:
            return f'{self._offset:#x} ({label})'

def _yieldEntryPoint(section: CodeSection,
                     addr: int,
                     name: Optional[str] = None
                     ) -> Iterator[EntryPoint]:
    try:
        offset = section.offsetForAddr(addr)
    except ValueError as ex:
        logger.warning(
            'Ignoring entry point%s: %s',
            '' if name is None else f' "{name}"', ex
            )
    else:
        yield EntryPoint(offset, name)

class BinaryFormat:
    '''Abstract base class for binary formats.
    '''

    name: ClassVar[str]
    '''Short identifying name for this format.'''

    description: ClassVar[str]
    '''User-friendly name for this format.'''

    extensions: ClassVar[Sequence[str]]
    '''Sequence of file name extensions, lower case, excluding the dot.'''

    @property
    def image(self) -> Image:
        return self._image

    @classmethod
    def checkImage(cls, image: Image) -> int:
        '''Checks whether the given image (read-only buffer) might be an
        instance of this binary format.
        Returns a positive number for likely matches, zero for undecided and
        a negative number for unlikely matches. The more certain, the further
        the number should be from zero, where 1000 means "very likely" and
        -1000 means "very unlikely"; values are outside that range may be
        returned but will be clipped to [-1000, 1000].
        '''
        raise NotImplementedError

    def __init__(self, image: Image):
        self._image = image

    def iterSections(self) -> Iterator[Section]:
        '''Iterates through the Sections in this binary.
        '''
        raise NotImplementedError

    def iterEntryPoints(self) -> Iterator[EntryPoint]:
        '''Iterates through the EntryPoints in this binary.
        '''
        raise NotImplementedError

class GameBoyROM(BinaryFormat):

    name = 'gbrom'
    description = 'Game Boy ROM image'
    extensions = ('gb', 'gbc')

    header = Struct('<4s48s16s2sBBBBBBBBH')
    logo = bytes((
        0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B,
        0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D,
        0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E,
        0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99,
        0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC,
        0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E
        ))

    @classmethod
    def checkImage(cls, image: Image) -> int:
        header = _unpackStruct(image, 0x100, cls.header)
        if header is None:
            return -1000
        else:
            logo: bytes = header[1]
            return 1000 if logo == cls.logo else -1000

    def iterSections(self) -> Iterator[Section]:
        # Jump vectors.
        yield CodeSection(0x0, 0x104, 0x0, 'lr35902', ByteOrder.little)
        # Header.
        yield Section(0x104, 0x150)
        # ROM bank 0 (fixed).
        yield CodeSection(0x150, 0x4000, 0x150, 'lr35902', ByteOrder.little)
        # ROM bank 1+ (switchable).
        for bank in range(1, len(self._image) // 0x4000):
            offset = bank * 0x4000
            yield CodeSection(
                offset, offset + 0x4000, 0x4000, 'lr35902', ByteOrder.little
                )

    def iterEntryPoints(self) -> Iterator[EntryPoint]:
        # RST and interrupts.
        for addr in range(0x0, 0x68, 0x8):
            yield EntryPoint(addr)
        # Main entry point.
        yield EntryPoint(0x100)
        # Typical code start.
        # TODO: Once we can properly trace, we don't need this one anymore.
        yield EntryPoint(0x150)

class MSXROMHeader(NamedTuple):
    cartID: bytes
    init: int
    statement: int
    device: int
    text: int
    reserved: bytes

class MSXROM(BinaryFormat):

    name = 'msxrom'
    description = 'MSX ROM image'
    extensions = ('rom',)

    _headerStruct = Struct('<2sHHHH6s')

    @classmethod
    def checkImage(cls, image: Image) -> int:
        headerItems = _unpackStruct(image, 0, cls._headerStruct)
        if headerItems is None:
            return -1000
        header = MSXROMHeader(*headerItems)

        if header.cartID == b'AB':
            # ROM cartridge.
            score = 400
            if 0x4000 <= header.init < 0xC000:
                score += 200
            elif header.init != 0:
                score -= 500
            if not (header.statement == 0
                    or 0x4000 <= header.statement < 0x8000):
                score -= 100
            if not (header.device == 0 or 0x4000 <= header.device < 0x8000):
                score -= 100
            if not (header.text == 0 or 0x8000 <= header.text < 0xC000):
                score -= 100
            score += sum(10 if byte == 0 else 0 for byte in header.reserved)
        elif header.cartID == b'CD':
            # Sub ROM, only used for BIOS.
            score = 100
            if header.init >= 0x4000:
                score -= 200
            if header.statement >= 0x4000:
                score -= 200
            if header.device >= 0x4000:
                score -= 200
            if header.text >= 0x4000:
                score -= 200
            score += sum(10 if byte == 0 else 0 for byte in header.reserved[1:])
        else:
            score = -1000

        if len(image) % 8192 != 0:
            score -= 500

        return score

    def __init__(self, image: Image):
        BinaryFormat.__init__(self, image)

        headerItems = _unpackStruct(image, 0, self._headerStruct)
        if headerItems is None:
            raise ValueError('no header')
        self._header = header = MSXROMHeader(*headerItems)

        # Figure out address at which ROM is mapped into memory.
        cartID = header.cartID
        if cartID == b'AB':
            if len(image) <= 0x4000:
                baseFreqs = [0] * 4
                for addr in (
                        header.init, header.statement, header.device,
                        header.text):
                    if addr != 0:
                        baseFreqs[addr >> 14] += 1
                if baseFreqs[1] == 0 and baseFreqs[2] != 0:
                    base = 0x8000
                else:
                    base = 0x4000
            else:
                base = 0x4000
        elif cartID == b'CD':
            base = 0x0000
        else:
            raise ValueError('unknown cartridge type')

        self._section = CodeSection(
            0x10, 0x8000, base + 0x10, 'z80', ByteOrder.little
            )

    def iterSections(self) -> Iterator[Section]:
        # Header.
        yield Section(0x0, 0x10)
        # Fixed mapping.
        # Note: MegaROM mappers are not supported yet.
        yield self._section

    def iterEntryPoints(self) -> Iterator[EntryPoint]:
        header = self._header
        section = self._section

        # Note: TEXT points to tokenized MSX-BASIC and is therefore not an
        #       entry point.
        for name in ('init', 'statement', 'device'):
            addr: int = getattr(header, name)
            if addr != 0:
                yield from _yieldEntryPoint(section, addr, name)

class PSXEXEHeader(NamedTuple):
    id: bytes
    textOffset: int
    dataOffset: int
    pc0: int
    gp0: int
    textAddr: int
    textSize: int
    dataAddr: int
    dataSize: int
    bssAddr: int
    bssSize: int
    stackAddr: int
    stackSize: int

class PSXExecutable(BinaryFormat):

    name = 'psxexe'
    description = 'PlayStation executable'
    extensions = ('exe',)

    _headerStruct = Struct('<8sIIIIIIIIIIII')

    @classmethod
    def checkImage(cls, image: Image) -> int:
        return 1000 if image[:8] == b'PS-X EXE' else -1000

    def __init__(self, image: bytes):
        BinaryFormat.__init__(self, image)

        headerItems = _unpackStruct(image, 0, self._headerStruct)
        if headerItems is None:
            raise ValueError('incomplete header')
        self._header = header = PSXEXEHeader(*headerItems)

        textStart = header.textOffset + 0x800
        textEnd = textStart + header.textSize
        textAddr= header.textAddr
        self._text = CodeSection(
            textStart, textEnd, textAddr, 'mips-i', ByteOrder.little
            )

        dataStart = header.dataOffset + 0x800
        dataEnd = dataStart + header.dataSize
        self._data = (
            None
            if dataEnd == dataStart
            else Section(dataStart, dataEnd)
            )

    def iterSections(self) -> Iterator[Section]:
        yield self._text
        data = self._data
        if data is not None:
            yield data

    def iterEntryPoints(self) -> Iterator[EntryPoint]:
        return _yieldEntryPoint(self._text, self._header.pc0, 'start')

class RawBinary(BinaryFormat):

    name = 'raw'
    description = 'raw binary image'
    extensions = ('raw', 'bin')

    @classmethod
    def checkImage(cls, image: Image) -> int:
        return 0

    def iterSections(self) -> Iterator[Section]:
        return iter(())

    def iterEntryPoints(self) -> Iterator[EntryPoint]:
        return iter(())

# Build a dictionary of binary formats using introspection.
_formatsByName = {
    obj.name: obj
    for obj in locals().values()
    if isinstance(obj, type) and issubclass(obj, BinaryFormat)
                             and obj is not BinaryFormat
    }

def iterBinaryFormatNames() -> Iterable[str]:
    '''Iterates through the names of supported binary formats, in no particular
    order.
    '''
    return _formatsByName.keys()

def getBinaryFormat(name: str) -> Type[BinaryFormat]:
    '''Looks up a binary format by its name attribute.
    Returns the binary format with the given value for its name attribute.
    Raises KeyError if there is no match.
    '''
    return _formatsByName[name]

def _detectBinaryFormats(image: Image,
                         names: Collection[str],
                         extMatches: bool
                         ) -> Optional[Type[BinaryFormat]]:
    logger.debug(
        'Binary format autodetection, extension %s:',
        'matches' if extMatches else 'does not match'
        )
    if len(names) == 0:
        logger.debug('  no formats')
        return None

    boost = 100 if extMatches else 0
    checkResults = []
    for name in names:
        binfmt = _formatsByName[name]
        likely = min(1000, max(-1000, binfmt.checkImage(image))) + boost
        checkResults.append((likely, name))
        logger.debug('  format "%s": score %d', name, likely)

    likely, name = max(checkResults)
    if likely > 0:
        logger.debug('Best match: %s', name)
        return _formatsByName[name]
    else:
        logger.debug('No match')
        return None

def detectBinaryFormat(image: Image,
                       fileName: Optional[str] = None
                       ) -> Optional[Type[BinaryFormat]]:
    '''Attempts to autodetect the binary format of the given image.
    If a file name is given, its extension will be used to first test the
    formats matching that extension, as well as considering those formats
    to be more likely matches.
    Returns a binary format on success, None on failure.
    '''
    names = set(_formatsByName.keys())

    if fileName is not None:
        # First try formats for which the file name extension matches.
        binPath = PurePath(fileName)
        ext = binPath.suffix.lstrip('.').lower()
        namesForExt = {
            binfmt.name
            for binfmt in _formatsByName.values()
            if ext in binfmt.extensions
            }
        binfmt = _detectBinaryFormats(image, namesForExt, True)
        if binfmt is not None:
            return binfmt
        names -= namesForExt

    # Try other formats.
    return _detectBinaryFormats(image, names, False)

def _unpackStruct(image: Image,
                  offset: int,
                  struct: Struct
                  ) -> Optional[Sequence[Any]]:
    '''Unpacks the given struct from the given offset of the given image.
    Returns the unpacked data, or None if the image did not contain enough
    data at the given offset.
    '''
    end = offset + struct.size
    return None if end > len(image) else struct.unpack(image[offset:end])
