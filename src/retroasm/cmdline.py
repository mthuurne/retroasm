from __future__ import annotations

import sys
from collections.abc import Iterable, Iterator, Sequence
from importlib.resources.abc import Traversable
from logging import DEBUG, INFO, Logger, StreamHandler, getLogger
from mmap import ACCESS_READ, mmap
from pathlib import Path
from typing import IO, NoReturn, cast, override

from click import (
    BadParameter,
    Context,
    Option,
    Parameter,
    ParamType,
    argument,
    command,
    get_current_context,
    group,
    option,
    version_option,
)
from click import Path as PathArg

from .asm._mnem_parser import get_instruction_parser
from .asm.directives import DataDirective, OriginDirective
from .asm.formatter import Formatter
from .asm.parser import read_sources
from .asm.source import Instruction
from .binfmt import (
    BinaryFormat,
    EntryPoint,
    Image,
    detect_binary_format,
    get_binary_format,
    iter_binary_format_names,
)
from .disasm import disassemble, format_asm
from .fetch import ImageFetcher
from .input import LocationFormatter, ProblemCounter
from .instr import (
    builtin_instruction_set_path,
    builtin_instruction_sets,
    load_instruction_set,
    load_instruction_set_by_name,
)
from .instrset import InstructionSet
from .parser.expression_nodes import NumberNode
from .section import ByteOrder, CodeSection, Section, SectionMap, StructuredDataSection
from .types import Unlimited, unlimited


def setup_logging(root_level: int) -> Logger:
    handler = StreamHandler()
    formatter = LocationFormatter()
    handler.setFormatter(formatter)
    logger = getLogger()
    logger.addHandler(handler)
    logger.setLevel(root_level)
    return logger


@command()
@option("-i", "--instr", required=True, help="Instruction set.")
@argument("sources", nargs=-1, type=PathArg(exists=True))
def asm(instr: str, sources: tuple[str, ...]) -> None:
    """Assembler using the RetroAsm toolkit."""

    logger = setup_logging(INFO)

    instr_set = load_instruction_set_by_name(instr, logger, want_semantics=False)
    if instr_set is None:
        get_current_context().exit(1)

    parsed = read_sources((Path(source) for source in sources), instr_set)

    problems = ProblemCounter()
    for source in parsed.values():
        problems += source.problem_counter
    logger.log(problems.level, "%s in %d source files", problems, len(parsed))
    if problems.num_errors > 0:
        get_current_context().exit(1)


def print_decoders(instr_set: InstructionSet, submodes: bool) -> None:
    flag_combos = sorted(sorted(flags) for flags in instr_set.decode_flag_combinations)
    for flags in flag_combos:
        print()
        if flags:
            print(f"with decode flag{'' if len(flags) == 1 else 's'} {', '.join(flags)}:")
            print()
        instr_set.get_decoder(frozenset(flags)).dump(submodes=submodes)


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
@option(
    "--dump-mnemonics",
    is_flag=True,
    help="Dump the mnemonic matching tree.",
)
@argument("instr", nargs=-1, type=str)
def checkdef(
    instr: Iterable[str], dump_decoders: bool, dump_decoders_subs: bool, dump_mnemonics: bool
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
                print("No definition files (*.instr) in directory:", path, file=sys.stderr)
        elif path.is_file():
            files.append(path)
        elif "/" not in name and "\\" not in name and "." not in name:
            files.append(builtin_instruction_set_path(name))
        else:
            print("Instruction set not found:", name, file=sys.stderr)
            errors = True

    if files:
        logger = setup_logging(INFO)
        for instr_file in files:
            logger.info("checking: %s", instr_file)
            instr_set = load_instruction_set(instr_file, logger)
            if instr_set is None:
                errors = True
                continue
            if dump_decoders:
                print_decoders(instr_set, False)
                print()
            if dump_decoders_subs:
                print_decoders(instr_set, True)
                print()
            if dump_mnemonics:
                get_instruction_parser(instr_set).dump("")
                print()
    else:
        print("No files to check", file=sys.stderr)
    get_current_context().exit(1 if errors else 0)


def determine_binary_format(
    image: Image, file_name: str, format_name: str | None, logger: Logger
) -> BinaryFormat | None:
    """
    Determines the right binary format for the given open file object.

    If the given format name is None, autodetection will be used.
    Returns a binary format subclass on success, or None if the format could
    not be determined.
    """

    if format_name is None:
        binfmt = detect_binary_format(image, file_name)
        if binfmt is None:
            logger.error("Detection of binary format failed, please specify one with --binfmt")
        else:
            logger.info("Detected binary format: %s (%s)", binfmt.name, binfmt.description)
    else:
        try:
            binfmt_class = get_binary_format(format_name)
        except KeyError:
            logger.error("Unknown binary format: %s", format_name)
            binfmt = None
        else:
            logger.debug(
                "User-specified binary format: %s (%s)",
                binfmt_class.name,
                binfmt_class.description,
            )
            binfmt = binfmt_class.autodetect(image)
    return binfmt


def disassemble_binary(
    binary: BinaryFormat,
    user_sections: Iterable[Section],
    user_entry_points: Iterable[EntryPoint],
    out: IO[str],
    logger: Logger,
) -> None:
    image = binary.image

    # Merge user-defined sections with sections from binary format.
    sections = []
    for section in user_sections:
        logger.debug("user-defined section: %s", section)
        sections.append(section)
    for section in binary.iter_sections():
        logger.debug("binfmt-defined section: %s", section)
        sections.append(section)
    if len(sections) == 0:
        logger.warning("No sections; you can manually define them using the --section argument")
        return
    try:
        section_map = SectionMap(sections)
    except ValueError as ex:
        logger.error("Invalid section map: %s", ex)
        return

    # Merge user-defined entry points with entry points from binary format.
    entry_points = []
    for entry_point in user_entry_points:
        logger.debug("user-defined entry: %s", entry_point)
        entry_points.append(entry_point)
    for entry_point in binary.iter_entry_points():
        logger.debug("binfmt-defined entry: %s", entry_point)
        entry_points.append(entry_point)
    if len(entry_points) == 0:
        logger.warning(
            "No entry points; you can manually define them using the --entry argument"
        )
        return

    # Disassemble.
    logger.info("Disassembling...")
    decoded: dict[CodeSection, Sequence[tuple[int, DataDirective | Instruction]]] = {}
    labels: dict[int, str] = {}
    for entry_point in entry_points:
        offset = entry_point.offset

        # Find section.
        entry_section = section_map.section_at(offset)
        if entry_section is None:
            logger.warning(
                "Skipping disassembly of offset 0x%x because it does not belong to any section",
                offset,
            )
            continue

        # Find instruction set.
        if not isinstance(entry_section, CodeSection):
            logger.warning(
                "Skipping disassembly of offset 0x%x because its section does "
                "not specify an instruction set",
                offset,
            )
            continue
        instr_set_name = entry_section.instr_set_name
        instr_set = builtin_instruction_sets[instr_set_name]
        if instr_set is None:
            logger.warning(
                'Skipping disassembly of offset 0x%x due to unknown instruction set "%s"',
                offset,
                instr_set_name,
            )
            continue

        # Find end point.
        end = min(entry_section.end, len(image))
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
        addr = entry_section.base + offset - entry_section.start
        label = entry_point.label
        if label is not None:
            labels[addr] = label

        # TODO: For now, we disassemble full sections instead of tracing execution
        #       from the entry points.
        offset = entry_section.start
        addr = entry_section.base

        # Create instruction fetcher.
        try:
            instr_width = instr_set.encoding_width
            if instr_width is None:
                raise ValueError("unknown instruction width")
            fetcher_factory = ImageFetcher.factory(instr_width, entry_section.byte_order)
        except ValueError as ex:
            logger.warning(
                "Skipping disassembly of offset 0x%x because no instruction fetcher "
                "could be created: %s",
                offset,
                ex,
            )
            continue
        fetcher = fetcher_factory(image, offset, end)

        if entry_section not in decoded:
            decoded[entry_section] = tuple(disassemble(instr_set, fetcher, addr))

    # Output assembly.
    logger.info("Writing output...")
    formatter = Formatter()
    image_offset_width = len(image).bit_length()
    print(formatter.comment("Disassembled by RetroAsm"), file=out)
    for section, data in _iter_image_sections(image, section_map):
        print(file=out)
        print(
            formatter.comment(
                f"{section.description.title()} section: "
                f"{formatter.hex_range(section.start, section.end, image_offset_width)}"
            ),
            file=out,
        )
        print(file=out)
        if section in decoded:
            assert isinstance(section, CodeSection), section
            instr_set = builtin_instruction_sets[section.instr_set_name]
            assert instr_set is not None
            org_addr = NumberNode(section.base, instr_set.addr_type.width)
            org = OriginDirective(org_addr)
            print(formatter.origin(org), file=out)
            print(file=out)
            for line in format_asm(formatter, decoded[section], labels):
                print(line, file=out)
        elif isinstance(section, StructuredDataSection):
            for directive in section.data.directives:
                print(formatter.data(directive), file=out)
        else:
            for line in formatter.raw(data):
                print(line, file=out)


def _iter_image_sections(image: Image, sections: SectionMap) -> Iterator[tuple[Section, bytes]]:
    """
    Iterate through the sections and corresponding data in the given image.

    Gaps in the section map will be wrapped in dummy sections.
    Sections that are entirely outside of the image will be ignored.
    The last section could be partially outside of the image.
    """
    image_end = len(image)
    offset = 0
    for section in sections:
        prev_end = min(section.start, image_end)
        if offset < prev_end:
            yield Section(offset, prev_end, "gap"), image[offset:prev_end]
        if prev_end == image_end:
            break
        end = cast(int, min(section.end, image_end))
        yield section, image[prev_end:end]
        offset = end
    if offset < image_end:
        yield Section(offset, image_end, "gap"), image[offset:image_end]


def _parse_number(number: str) -> int:
    if number.startswith("0x"):
        return int(number[2:], 16)
    else:
        return int(number)


class EntryPointParamType(ParamType):
    """Parameter type for entry points."""

    name = "entry point"

    @override
    def get_metavar(self, param: Parameter, ctx: Context) -> str:
        return "OFFSET[,LABEL]"

    @override
    def convert(self, value: str, param: Parameter | None, ctx: Context | None) -> EntryPoint:
        label: str | None
        if "," in value:
            offset_str, label = value.split(",")
        else:
            offset_str, label = value, None

        try:
            offset = _parse_number(offset_str)
        except ValueError as ex:
            raise BadParameter(f'Bad entry point definition "{value}": {ex}') from ex
        else:
            return EntryPoint(offset, label)


ENTRY_POINT = EntryPointParamType()


class SectionParamType(ParamType):
    """Parameter type for sections."""

    name = "section"

    @override
    def get_metavar(self, param: Parameter, ctx: Context) -> str:
        return "OPT1:...:OPTn"

    @override
    def convert(self, value: str, param: Parameter | None, ctx: Context | None) -> Section:
        instr_set_name: str | None = None
        byteorder = ByteOrder.undefined
        start = 0
        end: int | Unlimited = unlimited
        base = 0
        for opt in value.split(":"):
            if not opt:
                pass
            elif ".." in opt:
                start_str, end_str = opt.split("..")
                try:
                    start = _parse_number(start_str)
                except ValueError as ex:
                    raise BadParameter(f'Bad section start "{start_str}": {ex}') from ex
                try:
                    end = _parse_number(end_str)
                except ValueError as ex:
                    raise BadParameter(f'Bad section end "{end_str}": {ex}') from ex
            elif opt[0].isdigit():
                try:
                    base = _parse_number(opt)
                except ValueError as ex:
                    raise BadParameter(f'Bad section base address "{opt}": {ex}') from ex
            else:
                if "," in opt:
                    instr_set_name, byteorder_str = opt.split(",")
                    if byteorder_str == "be":
                        byteorder = ByteOrder.big
                    elif byteorder_str == "le":
                        byteorder = ByteOrder.little
                    else:
                        raise BadParameter(f'Unknown byte order "{byteorder_str}"')
                else:
                    instr_set_name = opt

        if instr_set_name is None:
            return Section(start, end)
        else:
            return CodeSection(start, end, base, instr_set_name, byteorder)


SECTION = SectionParamType()


def list_supported(
    ctx: Context,
    param: Parameter | Option,  # pylint: disable=unused-argument
    value: bool,
) -> None:
    if not value or ctx.resilient_parsing:
        return

    print(disasm.__doc__)
    print()

    print("Binary formats:")
    names = sorted(iter_binary_format_names())
    line_formatter = f"  %-{max(len(name) for name in names)}s : %s"
    for name in names:
        binfmt = get_binary_format(name)
        print(line_formatter % (name, binfmt.description))
    print()

    print("Instruction sets:")
    for name in sorted(builtin_instruction_sets):
        print(f"  {name}")
    print()

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
    callback=list_supported,
    help="List available binary formats and instruction sets, then exit.",
)
@option(
    "-b",
    "--binfmt",
    metavar="FORMAT",
    help="Binary format.  [default: autodetect]",
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
    help="Code or data section. See below for sections syntax. Can be passed multiple times.",
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
    setup_logging(INFO if verbose < 2 else DEBUG)
    logger = getLogger("retroasm.disasm")
    if verbose > 0:
        logger.setLevel(DEBUG)
        getLogger("retroasm.binfmt").setLevel(DEBUG)

    # Disassemble binary file.
    try:
        with open(binary, "rb") as bin_file:
            with mmap(bin_file.fileno(), 0, access=ACCESS_READ) as image:
                binary_format = determine_binary_format(image, binary, binfmt, logger)
                if binary_format is None:
                    get_current_context().exit(1)
                disassemble_binary(binary_format, sections, entries, sys.stdout, logger)
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
