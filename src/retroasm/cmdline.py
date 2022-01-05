from __future__ import annotations

from importlib.abc import Traversable
from logging import DEBUG, INFO, Logger, StreamHandler, getLogger
from mmap import ACCESS_READ, mmap
from pathlib import Path
from typing import IO, Iterable, Iterator, NoReturn, Sequence, cast
import sys

from click import (
    BadParameter,
    Context,
    Option,
    ParamType,
    Parameter,
    Path as PathArg,
    argument,
    command,
    get_current_context,
    group,
    option,
    version_option,
)

from .asm_directives import DataDirective, OriginDirective
from .asm_formatter import Formatter
from .asm_parser import read_source
from .binfmt import (
    BinaryFormat,
    EntryPoint,
    Image,
    detectBinaryFormat,
    getBinaryFormat,
    iterBinaryFormatNames,
)
from .disasm import Instruction, disassemble, formatAsm
from .fetch import ImageFetcher
from .instr import (
    builtinInstructionSetPath,
    builtinInstructionSets,
    loadInstructionSet,
    loadInstructionSetByName,
)
from .instrset import InstructionSet
from .linereader import DelayedError, LineReaderFormatter
from .section import ByteOrder, CodeSection, Section, SectionMap, StructuredDataSection
from .types import Unlimited, unlimited


def setupLogging(rootLevel: int) -> Logger:
    handler = StreamHandler()
    formatter = LineReaderFormatter()
    handler.setFormatter(formatter)
    logger = getLogger()
    logger.addHandler(handler)
    logger.setLevel(rootLevel)
    return logger


@command()
@option("-i", "--instr", required=True, help="Instruction set.")
@argument("source", type=PathArg(exists=True))
def asm(instr: str, source: str) -> None:
    """Assembler using the RetroAsm toolkit."""

    logger = setupLogging(INFO)

    instrSet = loadInstructionSetByName(instr, logger, wantSemantics=False)
    if instrSet is None:
        get_current_context().exit(1)

    sourcePath = Path(source)
    try:
        read_source(sourcePath, instrSet)
    except OSError as ex:
        logger.error('Failed to read source "%s": %s', ex.filename, ex.strerror)
        get_current_context().exit(1)
    except DelayedError:
        get_current_context().exit(1)


def dumpDecoders(instrSet: InstructionSet, submodes: bool) -> None:

    flagCombos = sorted(sorted(flags) for flags in instrSet.decodeFlagCombinations)
    for flags in flagCombos:
        print()
        if flags:
            print(
                f"with decode flag{'' if len(flags) == 1 else 's'} {', '.join(flags)}:"
            )
            print()
        instrSet.getDecoder(frozenset(flags)).dump(submodes=submodes)


@command()
@option(
    "--dump-decoders",
    is_flag=True,
    help="Dump the instruction decoder tree, excluding submodes.",
)
@option(
    "--dump-decoders-subs",
    is_flag=True,
    help="Dump the instruction decoder tree, including submodes.",
)
@argument("instr", nargs=-1, type=str)
def checkdef(
    instr: Iterable[str], dump_decoders: bool, dump_decoders_subs: bool
) -> NoReturn:
    """
    Check instruction set definition files.

    INSTR can be one or more files, directories or built-in instruction set names.
    """

    files: list[Traversable] = []
    errors = False
    for name in instr:
        path = Path(name)
        if path.is_dir():
            files_in_dir = list(path.glob("**/*.instr"))
            if files_in_dir:
                files += files_in_dir
            else:
                print(
                    "No definition files (*.instr) in directory:", path, file=sys.stderr
                )
        elif path.is_file():
            files.append(path)
        elif "/" not in name and "\\" not in name and "." not in name:
            files.append(builtinInstructionSetPath(name))
        else:
            print("Instruction set not found:", name, file=sys.stderr)
            errors = True

    if files:
        logger = setupLogging(INFO)
        for instr_file in files:
            logger.info("checking: %s", instr_file)
            instr_set = loadInstructionSet(instr_file, logger)
            if instr_set is None:
                errors = True
                continue
            if dump_decoders:
                dumpDecoders(instr_set, False)
            if dump_decoders_subs:
                dumpDecoders(instr_set, True)
    else:
        print("No files to check", file=sys.stderr)
    get_current_context().exit(1 if errors else 0)


def determineBinaryFormat(
    image: Image, fileName: str, formatName: str | None, logger: Logger
) -> BinaryFormat | None:
    """
    Determines the right binary format for the given open file object.

    If the given format name is None, autodetection will be used.
    Returns a binary format subclass on success, or None if the format could
    not be determined.
    """

    if formatName is None:
        binfmt = detectBinaryFormat(image, fileName)
        if binfmt is None:
            logger.error(
                "Detection of binary format failed, " "please specify one with --binfmt"
            )
        else:
            logger.info(
                "Detected binary format: %s (%s)", binfmt.name, binfmt.description
            )
    else:
        try:
            binfmtClass = getBinaryFormat(formatName)
        except KeyError:
            logger.error("Unknown binary format: %s", formatName)
            binfmt = None
        else:
            logger.debug(
                "User-specified binary format: %s (%s)",
                binfmtClass.name,
                binfmtClass.description,
            )
            binfmt = binfmtClass.autodetect(image)
    return binfmt


def disassembleBinary(
    binary: BinaryFormat,
    userSections: Iterable[Section],
    userEntryPoints: Iterable[EntryPoint],
    out: IO[str],
    logger: Logger,
) -> None:

    image = binary.image

    # Merge user-defined sections with sections from binary format.
    sections = []
    for section in userSections:
        logger.debug("user-defined section: %s", section)
        sections.append(section)
    for section in binary.iterSections():
        logger.debug("binfmt-defined section: %s", section)
        sections.append(section)
    if len(sections) == 0:
        logger.warning(
            "No sections; you can manually define them using the --section " "argument"
        )
        return
    try:
        sectionMap = SectionMap(sections)
    except ValueError as ex:
        logger.error("Invalid section map: %s", ex)
        return

    # Merge user-defined entry points with entry points from binary format.
    entryPoints = []
    for entryPoint in userEntryPoints:
        logger.debug("user-defined entry: %s", entryPoint)
        entryPoints.append(entryPoint)
    for entryPoint in binary.iterEntryPoints():
        logger.debug("binfmt-defined entry: %s", entryPoint)
        entryPoints.append(entryPoint)
    if len(entryPoints) == 0:
        logger.warning(
            "No entry points; you can manually define them using the --entry "
            "argument"
        )
        return

    # Disassemble.
    logger.info("Disassembling...")
    decoded: dict[CodeSection, Sequence[tuple[int, DataDirective | Instruction]]] = {}
    labels: dict[int, str] = {}
    for entryPoint in entryPoints:
        offset = entryPoint.offset

        # Find section.
        entrySection = sectionMap.sectionAt(offset)
        if entrySection is None:
            logger.warning(
                "Skipping disassembly of offset 0x%x because it does not "
                "belong to any section",
                offset,
            )
            continue

        # Find instruction set.
        if not isinstance(entrySection, CodeSection):
            logger.warning(
                "Skipping disassembly of offset 0x%x because its section does "
                "not specify an instruction set",
                offset,
            )
            continue
        instrSetName = entrySection.instrSetName
        instrSet = builtinInstructionSets[instrSetName]
        if instrSet is None:
            logger.warning(
                "Skipping disassembly of offset 0x%x due to unknown "
                'instruction set "%s"',
                offset,
                instrSetName,
            )
            continue

        # Find end point.
        end = min(entrySection.end, len(image))
        assert isinstance(end, int), end
        if offset >= end:
            logger.warning(
                "Skipping disassembly of offset 0x%x because it is outside "
                "of the image (size 0x%x)",
                offset,
                len(image),
            )
            continue

        # Remember label.
        addr = entrySection.base + offset - entrySection.start
        label = entryPoint.label
        if label is not None:
            labels[addr] = label

        # TODO: For now, we disassemble full sections instead of tracing execution
        #       from the entry points.
        offset = entrySection.start
        addr = entrySection.base

        # Create instruction fetcher.
        try:
            instrWidth = instrSet.encodingWidth
            if instrWidth is None:
                raise ValueError("unknown instruction width")
            fetcherFactory = ImageFetcher.factory(instrWidth, entrySection.byteOrder)
        except ValueError as ex:
            logger.warning(
                "Skipping disassembly of offset 0x%x because no instruction fetcher "
                "could be created: %s",
                offset,
                ex,
            )
            continue
        fetcher = fetcherFactory(image, offset, end)

        if entrySection not in decoded:
            decoded[entrySection] = tuple(disassemble(instrSet, fetcher, addr))

    # Output assembly.
    logger.info("Writing output...")
    formatter = Formatter()
    imageOffsetWidth = len(image).bit_length()
    print(formatter.comment("Disassembled by RetroAsm"), file=out)
    for section, data in _iterImageSections(image, sectionMap):
        print(file=out)
        print(
            formatter.comment(
                f"{section.description.title()} section: "
                f"{formatter.hexRange(section.start, section.end, imageOffsetWidth)}"
            ),
            file=out,
        )
        print(file=out)
        if section in decoded:
            assert isinstance(section, CodeSection), section
            instrSet = builtinInstructionSets[section.instrSetName]
            assert instrSet is not None
            org = OriginDirective.from_int(section.base, instrSet.addrType)
            print(formatter.origin(org), file=out)
            print(file=out)
            for line in formatAsm(formatter, decoded[section], labels):
                print(line, file=out)
        elif isinstance(section, StructuredDataSection):
            for directive in section.data.directives:
                print(formatter.data(directive), file=out)
        else:
            for line in formatter.raw(data):
                print(line, file=out)


def _iterImageSections(
    image: Image, sections: SectionMap
) -> Iterator[tuple[Section, bytes]]:
    """
    Iterate through the sections and corresponding data in the given image.

    Gaps in the section map will be wrapped in dummy sections.
    Sections that are entirely outside of the image will be ignored.
    The last section could be partially outside of the image.
    """
    imageEnd = len(image)
    offset = 0
    for section in sections:
        prevEnd = min(section.start, imageEnd)
        if offset < prevEnd:
            yield Section(offset, prevEnd, "gap"), image[offset:prevEnd]
        if prevEnd == imageEnd:
            break
        end = cast(int, min(section.end, imageEnd))
        yield section, image[prevEnd:end]
        offset = end
    if offset < imageEnd:
        yield Section(offset, imageEnd, "gap"), image[offset:imageEnd]


def _parse_number(number: str) -> int:
    if number.startswith("0x"):
        return int(number[2:], 16)
    else:
        return int(number)


class EntryPointParamType(ParamType):
    """Parameter type for entry points."""

    name = "entry point"

    def get_metavar(self, param: Parameter) -> str:
        return "OFFSET[,LABEL]"

    def convert(
        self, value: str, param: Parameter | None, ctx: Context | None
    ) -> EntryPoint:

        label: str | None
        if "," in value:
            offsetStr, label = value.split(",")
        else:
            offsetStr, label = value, None

        try:
            offset = _parse_number(offsetStr)
        except ValueError as ex:
            raise BadParameter(f'Bad entry point definition "{value}": {ex}') from ex
        else:
            return EntryPoint(offset, label)


ENTRY_POINT = EntryPointParamType()


class SectionParamType(ParamType):
    """Parameter type for sections."""

    name = "section"

    def get_metavar(self, param: Parameter) -> str:
        return "OPT1:...:OPTn"

    def convert(
        self, value: str, param: Parameter | None, ctx: Context | None
    ) -> Section:

        instrSetName: str | None = None
        byteorder = ByteOrder.undefined
        start = 0
        end: int | Unlimited = unlimited
        base = 0
        for opt in value.split(":"):
            if not opt:
                pass
            elif ".." in opt:
                startStr, endStr = opt.split("..")
                try:
                    start = _parse_number(startStr)
                except ValueError as ex:
                    raise BadParameter(f'Bad section start "{startStr}": {ex}') from ex
                try:
                    end = _parse_number(endStr)
                except ValueError as ex:
                    raise BadParameter(f'Bad section end "{endStr}": {ex}') from ex
            elif opt[0].isdigit():
                try:
                    base = _parse_number(opt)
                except ValueError as ex:
                    raise BadParameter(
                        f'Bad section base address "{opt}": {ex}'
                    ) from ex
            else:
                if "," in opt:
                    instrSetName, byteorderStr = opt.split(",")
                    if byteorderStr == "be":
                        byteorder = ByteOrder.big
                    elif byteorderStr == "le":
                        byteorder = ByteOrder.little
                    else:
                        raise BadParameter(f'Unknown byte order "{byteorderStr}"')
                else:
                    instrSetName = opt

        if instrSetName is None:
            return Section(start, end)
        else:
            return CodeSection(start, end, base, instrSetName, byteorder)


SECTION = SectionParamType()


def listSupported(
    ctx: Context,
    param: Parameter | Option,  # pylint: disable=unused-argument
    value: bool,
) -> None:

    if not value or ctx.resilient_parsing:
        return

    print(disasm.__doc__)
    print("")

    print("Binary formats:")
    names = sorted(iterBinaryFormatNames())
    lineFormatter = f"  %-{max(len(name) for name in names)}s : %s"
    for name in names:
        binfmt = getBinaryFormat(name)
        print(lineFormatter % (name, binfmt.description))
    print("")

    print("Instruction sets:")
    for name in sorted(builtinInstructionSets):
        print(f"  {name}")
    print("")

    ctx.exit()


# TODO: Support reading options from a simple text file.


@command(
    epilog="""
Section options: (separated by ":")

\b
START..END   offsets within binary  [default: entire file]
             start is inclusive, end is exclusive
ADDR         address of section start  [default: 0]
INSTR[,ORD]  instruction set  [required for code segment]
             ORD picks a byte order for instructions:
             'le' for little endian, 'be' for big endian

example:     0..0x1000:0x80000000:mips-i,le
"""
)
@option(
    "-l",
    "--list",
    is_flag=True,
    is_eager=True,
    expose_value=False,
    callback=listSupported,
    help="List available binary formats and instruction sets, then exit.",
)
@option(
    "-b", "--binfmt", metavar="FORMAT", help="Binary format.  [default: autodetect]"
)
@option(
    "entries",
    "-e",
    "--entry",
    multiple=True,
    type=ENTRY_POINT,
    help="Code entry point. Can be passed multiple times.",
)
@option(
    "sections",
    "-s",
    "--section",
    multiple=True,
    type=SECTION,
    help="Code or data section. See below for sections syntax. "
    "Can be passed multiple times.",
)
@option(
    "-v",
    "--verbose",
    count=True,
    help="Increase amount of logging. Can be passed multiple times.",
)
@argument("binary", type=PathArg(exists=True))
def disasm(
    binary: str,
    entries: Iterable[EntryPoint],
    sections: Iterable[Section],
    verbose: int,
    binfmt: str | None = None,
) -> None:
    """Disassembler using the RetroAsm toolkit."""

    # Set logging level.
    setupLogging(INFO if verbose < 2 else DEBUG)
    logger = getLogger("disasm")
    if verbose > 0:
        logger.setLevel(DEBUG)
        getLogger("binfmt").setLevel(DEBUG)

    # Disassemble binary file.
    try:
        with open(binary, "rb") as binFile:
            with mmap(binFile.fileno(), 0, access=ACCESS_READ) as image:
                binaryFormat = determineBinaryFormat(image, binary, binfmt, logger)
                if binaryFormat is None:
                    get_current_context().exit(1)
                disassembleBinary(binaryFormat, sections, entries, sys.stdout, logger)
    except OSError as ex:
        if ex.filename == binary:
            logger.error('Failed to read binary "%s": %s', binary, ex.strerror)
        else:
            logger.error("OS error: %s", ex.strerror)
        get_current_context().exit(1)


@group()
@version_option(prog_name="RetroAsm", message="%(prog)s version %(version)s")
def main() -> None:
    """Command line interface to the RetroAsm assembly toolkit."""


main.add_command(asm)
main.add_command(checkdef)
main.add_command(disasm)
