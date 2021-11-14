from __future__ import annotations

from dataclasses import astuple, dataclass, replace
from logging import getLogger
from pathlib import PurePath
from struct import Struct
from typing import (
    Any,
    ClassVar,
    Collection,
    Iterable,
    Iterator,
    Protocol,
    TypeVar,
    overload,
)

from .asm_directives import DataDirective, StringDirective, StructuredData
from .section import ByteOrder, CodeSection, Section, StructuredDataSection
from .types import IntType
from .utils import const_property

logger = getLogger("binfmt")


class Image(Protocol):
    """
    A binary image.

    `mmap.mmap` implements this interface, while it does not implement
    `ByteString` because it doesn't provide `__contains__`.
    """

    @overload
    def __getitem__(self, index: int) -> int:
        ...

    @overload
    def __getitem__(self, index: slice) -> bytes:
        ...

    # This false positive has already been fixed, but is not in a release yet:
    #   https://github.com/PyCQA/pylint/issues/4736
    def __len__(self) -> int:  # pylint: disable=invalid-length-returned
        ...


class EntryPoint:
    """A point at which execution can start."""

    @property
    def offset(self) -> int:
        """Location of this entry point in the image, in bytes from the start."""
        return self._offset

    @property
    def label(self) -> str | None:
        return self._label

    def __init__(self, offset: int, label: str | None = None):
        self._offset = offset
        self._label = label

        if offset < 0:
            raise ValueError(f"negative offset: {offset:d}")

    def __repr__(self) -> str:
        return f"EntryPoint({self._offset:#x}, {self._label!r})"

    def __str__(self) -> str:
        label = self._label
        if label is None:
            return f"{self._offset:#x}"
        else:
            return f"{self._offset:#x} ({label})"


def _yieldEntryPoint(
    section: CodeSection, addr: int, name: str | None = None
) -> Iterator[EntryPoint]:
    try:
        offset = section.offsetForAddr(addr)
    except ValueError as ex:
        logger.warning(
            "Ignoring entry point%s: %s", "" if name is None else f' "{name}"', ex
        )
    else:
        yield EntryPoint(offset, name)


BinFmtT = TypeVar("BinFmtT", bound="BinaryFormat")


class BinaryFormat:
    """Abstract base class for binary formats."""

    name: ClassVar[str]
    """Short identifying name for this format."""

    description: ClassVar[str]
    """User-friendly name for this format."""

    extensions: ClassVar[tuple[str, ...]]
    """File name extensions: lower case, excluding the dot."""

    @classmethod
    def autodetect(cls: type[BinFmtT], image: Image) -> BinFmtT | None:
        """
        Attempt to autodetect the given image as an instance of this binary format.

        Returns the most likely instance if the image could feasibly be an instance
        of this format, `None` otherwise.
        """
        return max(
            cls.detectAll(image),
            default=None,
            key=lambda fmt: -9999 if fmt is None else fmt.score,
        )

    @classmethod
    def detectAll(cls: type[BinFmtT], image: Image) -> Iterator[BinFmtT]:
        """
        Iterate through plausible intepretations of the given image as this binary
        format.

        Implementers are encouraged to yield all plausible instances even if they
        are not very likely; the `score` property can then be used to determine
        whether it's worth continuing with this interpretation of the image.
        """
        raise NotImplementedError

    def __init__(self, image: Image):
        self._image = image

    @property
    def image(self) -> Image:
        return self._image

    @property
    def score(self) -> int:
        """
        A number which indicates how likely it is that the wrapped image is actually
        an instance of this binary format.

        A positive number indicates a likely match, zero is undecided and a negative
        number indicates an unlikely match. The more certain, the further the number
        should be from zero, where 1000 means "very likely" and -1000 means "very
        unlikely".

        The default implementation returns 1000.
        """
        return 1000

    def iterSections(self) -> Iterator[Section]:
        """Iterates through the Sections in this binary."""
        raise NotImplementedError

    def iterEntryPoints(self) -> Iterator[EntryPoint]:
        """Iterates through the EntryPoints in this binary."""
        raise NotImplementedError


class GameBoyROM(BinaryFormat):

    name = "gbrom"
    description = "Game Boy ROM image"
    extensions = ("gb", "gbc")

    header = Struct("<4s48s16s2sBBBBBBBBH")
    logo = (
        b"\xce\xed\x66\x66\xcc\x0d\x00\x0b\x03\x73\x00\x83\x00\x0c\x00\x0d"
        b"\x00\x08\x11\x1f\x88\x89\x00\x0e\xdc\xcc\x6e\xe6\xdd\xdd\xd9\x99"
        b"\xbb\xbb\x67\x63\x6e\x0e\xec\xcc\xdd\xdc\x99\x9f\xbb\xb9\x33\x3e"
    )

    @classmethod
    def detectAll(cls, image: Image) -> Iterator[GameBoyROM]:
        header = _unpackStruct(image, 0x100, cls.header)
        if header is None:
            return
        logo: bytes = header[1]
        if logo != cls.logo:
            return
        yield cls(image)

    def iterSections(self) -> Iterator[Section]:
        # Jump vectors.
        yield CodeSection(0x0, 0x104, 0x0, "lr35902", ByteOrder.little)
        # Header.
        yield Section(0x104, 0x150)
        # ROM bank 0 (fixed).
        yield CodeSection(0x150, 0x4000, 0x150, "lr35902", ByteOrder.little)
        # ROM bank 1+ (switchable).
        for bank in range(1, len(self._image) // 0x4000):
            offset = bank * 0x4000
            yield CodeSection(
                offset, offset + 0x4000, 0x4000, "lr35902", ByteOrder.little
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


@dataclass(frozen=True)
class MSXROMHeader(StructuredData):
    struct: ClassVar[Struct] = Struct("<2sHHHH6s")

    cartID: bytes = b"AB"
    init: int = 0
    statement: int = 0
    device: int = 0
    text: int = 0
    reserved: bytes = bytes(6)

    size: int = struct.size
    """
    Size of the header in bytes.

    Some ROMs put code or data in the reserved area or even in the entry points
    after INIT. For those ROMs, the size can be overridden.

    The value must be a multiple of 2.
    """

    @classmethod
    def unpack(cls, image: Image, offset: int) -> MSXROMHeader | None:
        items = _unpackStruct(image, offset, cls.struct)
        return None if items is None else cls(*items)

    def __post_init__(self) -> None:
        size = self.size
        if size < 4 or size > self.struct.size or size % 2 != 0:
            raise ValueError(f"invalid size: {size:d}")
        # Clear addresses that are beyond the end of the header.
        for name in ("init", "statement", "device", "text")[(size - 2) // 2 :]:
            object.__setattr__(self, name, 0)

    @property
    def encoded(self) -> bytes:
        return self.struct.pack(astuple(self))

    @property
    def directives(self) -> Iterator[DataDirective | StringDirective]:
        yield StringDirective(self.cartID)
        remaining = self.size - 2
        for name in ("init", "statement", "device", "text"):
            if remaining <= 0:
                break
            remaining -= 2
            value: int = getattr(self, name)
            if value == 0:
                # TODO: Add comment with the entry point name.
                yield DataDirective.u16(0)
            else:
                yield DataDirective.symbol(IntType.u(16), name)
        if remaining > 0:
            yield DataDirective.u8(*self.reserved[:remaining])

    def __len__(self) -> int:
        return self.size


class MSXROM(BinaryFormat):

    name = "msxrom"
    description = "MSX ROM image"
    extensions = ("rom",)

    @classmethod
    def detectAll(cls, image: Image) -> Iterator[MSXROM]:
        # Attempt to deduce the address mapping from the ROM header.
        header = MSXROMHeader.unpack(image, 0x0000)
        if header is None:
            return
        if header.cartID == b"AB":
            yield cls._create(image, 0x0000, header, 0x0000)
            yield cls._create(image, 0x0000, header, 0x4000)
            if len(image) <= 0x4000:
                yield cls._create(image, 0x0000, header, 0x8000)
        elif header.cartID == b"CD":
            yield cls._create(image, 0x0000, replace(header, size=8), 0x0000)
        else:
            header = MSXROMHeader.unpack(image, 0x4000)
            if header is None:
                return
            if header.cartID == b"AB":
                yield cls._create(image, 0x4000, header, 0x0000)
                yield cls._create(image, 0x4000, header, 0x4000)

    @classmethod
    def _create(
        cls, image: Image, headerOffset: int, header: MSXROMHeader, base: int
    ) -> MSXROM:
        # Some ROMs put code or data in the header area.
        init = header.init
        headerBase = base + headerOffset
        headerSize = header.size
        if 4 <= init - headerBase < 16:
            headerSize = (init - headerBase) & ~1
            if headerSize < 10:
                # If the header is this incomplete, we can assume that INIT will not
                # return and therefore disregard STATEMENT and DEVICE.
                headerSize = 4

        # If STATEMENT, DEVICE or TEXT are out of range, that's a clue that INIT
        # will not return and therefore it would be better to truncate the header.
        imageSize = len(image)
        if imageSize <= 65536:
            addrEnd = base + imageSize
        else:
            addrEnd = 0xC000
        for name in ("statement", "device", "text"):
            addr: int = getattr(header, name)
            if addr != 0 and not base <= addr < addrEnd:
                logger.debug(
                    "truncating MSX ROM header because %s address looks bogus",
                    name.upper(),
                )
                headerSize = 4

        if headerSize != header.size:
            header = replace(header, size=headerSize)

        return cls(image, headerOffset, header, base)

    def __init__(
        self, image: Image, headerOffset: int, header: MSXROMHeader, base: int
    ):
        BinaryFormat.__init__(self, image)

        codeStart = headerOffset + header.size
        if base == 0x0000:
            codeEnd = min(0x10000, len(image))
        else:
            codeEnd = min(0xC000 - base, len(image))

        self._header = header
        self._headerOffset = headerOffset
        self._section = CodeSection(
            codeStart,
            codeEnd,
            base + codeStart,
            "z80",
            ByteOrder.little,
        )

    @const_property
    def score(self) -> int:
        header = self._header
        size = len(self._image)
        section = self._section

        if header.cartID == b"AB":
            # ROM cartridge.
            score = 400
            score += sum(10 for byte in header.reserved if byte == 0)
        elif header.cartID == b"CD":
            # Sub ROM, only used for BIOS.
            score = 100
        else:
            return -1000

        # ROMs that are mapped to page 1 + 2 are the most common, so give them a boost,
        # but a smaller amount than the penalty for INIT being out of range.
        if section.base // 0x4000 == 1:
            if size > 65536:
                # MegaROM mappers tend to use page 1 + 2.
                score += 200
            else:
                score += 100

        # Check whether entry points are in the mapped address range.
        numValidAddrs = 0
        for name in ("init", "statement", "device"):
            addr: int = getattr(header, name)
            if addr == 0:
                continue
            weight = 300 if name == "init" else 100
            try:
                offset = section.offsetForAddr(addr)
            except ValueError:
                score -= weight
            else:
                if offset < size:
                    numValidAddrs += 1
                else:
                    score -= weight

        # Tokenized BASIC.
        if header.text != 0:
            if (0x8000 + 10) <= header.text < 0xC000:
                numValidAddrs += 1
            else:
                score -= 100

        if numValidAddrs == 0:
            score -= 500

        if len(self._image) % 8192 != 0:
            score -= 500

        return score

    def iterSections(self) -> Iterator[Section]:
        # Note: MegaROM mappers are not supported yet.
        mainSection = self._section
        headerOffset = self._headerOffset

        if headerOffset != 0:
            # When the header is not at offset 0, we need a code section before it.
            base = mainSection.base - mainSection.start
            yield CodeSection(0, headerOffset, base, "z80", ByteOrder.little)

        yield StructuredDataSection(headerOffset, self._header, "header")
        yield mainSection

    def iterEntryPoints(self) -> Iterator[EntryPoint]:
        header = self._header
        section = self._section
        # Note: TEXT points to tokenized MSX-BASIC and is therefore not an entry point.
        for name in ("init", "statement", "device"):
            addr: int = getattr(header, name)
            if addr != 0:
                yield from _yieldEntryPoint(section, addr, name)


@dataclass(frozen=True)
class PSXEXEHeader:
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

    name = "psxexe"
    description = "PlayStation executable"
    extensions = ("exe",)

    _headerStruct = Struct("<8sIIIIIIIIIIII")

    @classmethod
    def detectAll(cls, image: Image) -> Iterator[PSXExecutable]:
        if image[:8] != b"PS-X EXE":
            return
        yield cls(image)

    def __init__(self, image: Image):
        BinaryFormat.__init__(self, image)

        headerItems = _unpackStruct(image, 0, self._headerStruct)
        if headerItems is None:
            raise ValueError("incomplete header")
        self._header = header = PSXEXEHeader(*headerItems)

        textStart = header.textOffset + 0x800
        textEnd = textStart + header.textSize
        textAddr = header.textAddr
        self._text = CodeSection(
            textStart, textEnd, textAddr, "mips-i", ByteOrder.little
        )

        dataStart = header.dataOffset + 0x800
        dataEnd = dataStart + header.dataSize
        self._data = None if dataEnd == dataStart else Section(dataStart, dataEnd)

    def iterSections(self) -> Iterator[Section]:
        yield self._text
        data = self._data
        if data is not None:
            yield data

    def iterEntryPoints(self) -> Iterator[EntryPoint]:
        return _yieldEntryPoint(self._text, self._header.pc0, "start")


class RawBinary(BinaryFormat):

    name = "raw"
    description = "raw binary image"
    extensions = ("raw", "bin")

    @classmethod
    def detectAll(cls, image: Image) -> Iterator[RawBinary]:
        yield RawBinary(image)

    @property
    def score(self) -> int:
        return 0

    def iterSections(self) -> Iterator[Section]:
        return iter(())

    def iterEntryPoints(self) -> Iterator[EntryPoint]:
        return iter(())


# Build a dictionary of binary formats using introspection.
_formatsByName = {
    obj.name: obj
    for obj in locals().values()
    if isinstance(obj, type)
    and issubclass(obj, BinaryFormat)
    and obj is not BinaryFormat
}


def iterBinaryFormatNames() -> Iterable[str]:
    """
    Iterates through the names of supported binary formats, in no particular order.
    """
    return _formatsByName.keys()


def getBinaryFormat(name: str) -> type[BinaryFormat]:
    """
    Looks up a binary format by its name attribute.
    Returns the binary format with the given value for its name attribute.
    Raises KeyError if there is no match.
    """
    return _formatsByName[name]


def _detectBinaryFormats(
    image: Image, names: Collection[str], extMatches: bool
) -> BinaryFormat | None:
    logger.debug(
        "Binary format autodetection, extension %s:",
        "matches" if extMatches else "does not match",
    )
    if len(names) == 0:
        logger.debug("  no formats")
        return None

    boost = 100 if extMatches else 0
    bestMatch = None
    bestScore = -1000
    for name in names:
        binfmtClass = _formatsByName[name]
        binfmt = binfmtClass.autodetect(image)
        if binfmt is None:
            logger.debug('  format "%s": rejected by autodetection', name)
        else:
            score = min(1000, binfmt.score)
            logger.debug('  format "%s": score %d', name, score)
            if score > bestScore:
                bestScore = score
                bestMatch = binfmt

    if bestScore + boost > 0:
        assert bestMatch is not None
        logger.debug("Best match: %s", bestMatch.name)
        return bestMatch
    else:
        logger.debug("No match")
        return None


def detectBinaryFormat(
    image: Image, fileName: str | None = None
) -> BinaryFormat | None:
    """
    Attempt to autodetect the binary format of the given image.

    If a file name is given, its extension will be used to first test the
    formats matching that extension, as well as considering those formats
    to be more likely matches.
    Returns a binary format on success, None on failure.
    """
    names = set(_formatsByName.keys())

    if fileName is not None:
        # First try formats for which the file name extension matches.
        binPath = PurePath(fileName)
        ext = binPath.suffix.lstrip(".").lower()
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


def _unpackStruct(image: Image, offset: int, struct: Struct) -> tuple[Any, ...] | None:
    """
    Unpacks the given struct from the given offset of the given image.

    Returns the unpacked data, or None if the image did not contain enough
    data at the given offset.
    """
    end = offset + struct.size
    return None if end > len(image) else struct.unpack(image[offset:end])
