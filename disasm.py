#!/usr/bin/env python3

from retroasm.instrset_parser import parseInstrSet
from retroasm.binfmt import (
    EntryPoint, detectBinaryFormat, getBinaryFormat, iterBinaryFormatNames
    )
from retroasm.disasm import (
    BigEndianFetcher, ByteFetcher, LittleEndianFetcher, disassemble
    )
from retroasm.linereader import LineReaderFormatter
from retroasm.section import ByteOrder, CodeSection, Section, SectionMap

from logging import DEBUG, INFO, StreamHandler, getLogger
from mmap import ACCESS_READ, mmap
from pathlib import Path

def setupLogging(rootLevel):
    handler = StreamHandler()
    formatter = LineReaderFormatter()
    handler.setFormatter(formatter)
    logger = getLogger()
    logger.addHandler(handler)
    logger.setLevel(rootLevel)

def listSupported(logger):
    logger.info('')

    logger.info('Binary formats:')
    names = sorted(iterBinaryFormatNames())
    lineFormatter = '  %%-%ds : %%s' % max(len(name) for name in names)
    for name in names:
        binfmt = getBinaryFormat(name)
        logger.info(lineFormatter, name, binfmt.description)
    logger.info('')

    logger.info('Instruction sets:')
    instrDir = Path('defs/instr/')
    names = sorted(
        p.stem
        for p in instrDir.glob('*.instr')
        )
    for name in names:
        logger.info('  %s', name)
    logger.info('')

def determineBinaryFormat(image, fileName, formatName, logger):
    '''Determines the right binary format for the given open file object.
    If the given format name is None, autodetection will be used.
    Returns a BinaryFormat subclass on success, or None if the format could
    not be determined.
    '''
    if formatName is None:
        binfmt = detectBinaryFormat(image, fileName)
        if binfmt is None:
            logger.error(
                'Detection of binary format failed, '
                'please specify one with --binfmt'
                )
        else:
            logger.info(
                'Detected binary format: %s (%s)',
                binfmt.name, binfmt.description
                )
    else:
        try:
            binfmt = getBinaryFormat(formatName)
        except KeyError:
            logger.error('Unknown binary format: %s', formatName)
            binfmt = None
        else:
            logger.debug(
                'User-specified binary format: %s (%s)',
                binfmt.name, binfmt.description
                )
    return binfmt

def disassembleBinary(binary, sectionDefs, entryDefs, logger):
    image = binary.image

    # Merge user-defined sections with sections from binary format.
    sections = []
    for sectionDef in sectionDefs:
        start, end = sectionDef.get('range', (0, image.size()))
        instrSetName = sectionDef.get('instr')
        if instrSetName is None:
            section = Section(start, end)
        else:
            base = sectionDef.get('addr', 0)
            byteorder = sectionDef.get('byteorder', ByteOrder.undefined)
            section = CodeSection(start, end, base, instrSetName, byteorder)
        logger.debug('user-defined section: %s', section)
        sections.append(section)
    for section in binary.iterSections():
        logger.debug('binfmt-defined section: %s', section)
        sections.append(section)
    if len(sections) == 0:
        logger.warning(
            'No sections; you can manually define them using the --section '
            'argument'
            )
        return
    try:
        sectionMap = SectionMap(sections)
    except ValueError as ex:
        logger.error('Invalid section map: %s', ex)
        return

    # Load instruction set definitions.
    instrSets = {}
    for section in sectionMap:
        instrSetName = getattr(section, 'instrSetName', None)
        if instrSetName is not None and instrSetName not in instrSets:
            logger.info('Loading instruction set: %s', instrSetName)
            instrPath = 'defs/instr/%s.instr' % instrSetName
            try:
                instrSets[instrSetName] = parseInstrSet(
                    instrPath, wantSemantics=False
                    )
            except OSError as ex:
                logger.error(
                    'Failed to read instruction set "%s": %s',
                    ex.filename, ex.strerror
                    )
                instrSets[instrSetName] = None

    # Merge user-defined entry points with entry points from binary format.
    entryPoints = []
    for entryDef in entryDefs:
        offset = entryDef['offset']
        label = entryDef.get('label')
        entryPoint = EntryPoint(offset, label)
        logger.debug('user-defined entry: %s', entryPoint)
        entryPoints.append(entryPoint)
    for entryPoint in binary.iterEntryPoints():
        logger.debug('binfmt-defined entry: %s', entryPoint)
        entryPoints.append(entryPoint)
    if len(entryPoints) == 0:
        logger.warning(
            'No entry points; you can manually define them using the --entry '
            'argument'
            )
        return

    # Disassemble.
    for entryPoint in entryPoints:
        offset = entryPoint.offset

        # Find section.
        section = sectionMap.sectionAt(offset)
        if section is None:
            logger.warning(
                'Skipping disassembly of offset 0x%x because it does not '
                'belong to any section', offset
                )
            continue

        # Find instruction set.
        instrSetName = getattr(section, 'instrSetName', None)
        if instrSetName is None:
            logger.warning(
                'Skipping disassembly of offset 0x%x because its section does '
                'not specify an instruction set', offset,
                )
            continue
        instrSet = instrSets[instrSetName]
        if instrSet is None:
            logger.warning(
                'Skipping disassembly of offset 0x%x due to unknown '
                'instruction set "%s"', offset, instrSetName
                )
            continue

        # Find end point.
        end = min(section.end, image.size())
        if offset >= end:
            logger.warning(
                'Skipping disassembly of offset 0x%x because it is outside '
                'of the image (size 0x%x)',
                offset, image.size()
                )
            continue

        # Create instruction fetcher.
        byteOrder = section.byteOrder
        instrWidth = instrSet.encodingWidth
        if instrWidth == 8:
            fetcher = ByteFetcher(image, offset, end)
        else:
            if byteOrder is ByteOrder.undefined:
                logger.warning(
                    'Skipping disassembly of offset 0x%x due to unknown '
                    'instruction byte order', offset
                    )
                continue
            elif byteOrder is ByteOrder.big:
                fetcher = BigEndianFetcher(image, offset, end, instrWidth)
            elif byteOrder is ByteOrder.little:
                fetcher = LittleEndianFetcher(image, offset, end, instrWidth)
            else:
                assert False, byteOrder

        addr = section.base + offset - section.start
        disassemble(instrSet, fetcher, addr)

def _parseNumber(number):
    if number.startswith('0x'):
        return int(number[2:], 16)
    else:
        return int(number)

def _parseSectionDef(sectionArg):
    sectionDef = {}
    def addDef(name, value):
        if name in sectionDef:
            raise ValueError('multiple definitions of "%s"' % name)
        else:
            sectionDef[name] = value
    for option in sectionArg.split(':'):
        if not option:
            pass
        elif '..' in option:
            start, end = option.split('..')
            addDef('range', (_parseNumber(start), _parseNumber(end)))
        elif option[0].isdigit():
            addDef('addr', _parseNumber(option))
        else:
            if ',' in option:
                option, byteorder = option.split(',')
                addDef('instr', option)
                if byteorder == 'be':
                    addDef('byteorder', ByteOrder.big)
                elif byteorder == 'le':
                    addDef('byteorder', ByteOrder.little)
                else:
                    raise ValueError('unknown byte order: %s' % byteorder)
            else:
                addDef('instr', option)
    return sectionDef

def _parseEntryDef(entryArg):
    entryDef = {}
    if ',' in entryArg:
        offset, label = entryArg.split(',')
        entryDef['offset'] = _parseNumber(offset)
        entryDef['label'] = label
    else:
        entryDef['offset'] = _parseNumber(entryArg)
    return entryDef

def main():
    from argparse import ArgumentParser, RawTextHelpFormatter
    from textwrap import dedent

    parser = ArgumentParser(
        fromfile_prefix_chars='@',
        formatter_class=RawTextHelpFormatter,
        description=dedent('''\
            Disassembler using the RetroAsm toolkit.
            '''),
        epilog=dedent('''\
            Arguments can also be read from a text file using @<file>.
            ''')
        )
    parser.add_argument(
        '-b', '--binfmt',
        help='binary format (default: autodetect)'
        )
    parser.add_argument(
        '-e', '--entry', action='append', metavar='OFFSET[,LABEL]',
        help=dedent('''\
            defines a code entry point
            use multiple --entry arguments to define multiple entry points
            ''')
        )
    parser.add_argument(
        '-l', '--list', action='store_true',
        help='list available binary formats and instruction sets, then exit'
        )
    parser.add_argument(
        '-s', '--section', action='append', metavar='OPT1:...:OPTn',
        help=dedent('''\
            defines a section, using options:
              START..END   offsets within binary (default: entire file)
                           start is inclusive, end is exclusive
              ADDR         address of section start (default: 0)
              INSTR[,ORD]  instruction set (required for code segment)
                           ORD picks a byte order for instructions:
                           'le' for little endian, 'be' for big endian
            example: --section 0..0x1000:0x80000000:mips-i,le
            use multiple --section arguments to define multiple sections
            ''')
        )
    parser.add_argument(
        '-v', '--verbose', action='count', default=0,
        help='increase amount of logging, can be passed multiple times'
        )
    parser.add_argument(
        'binary', nargs='?',
        help='file containing code to disassemble'
        )
    args = parser.parse_args()

    verbosity = args.verbose
    setupLogging(INFO if verbosity < 2 else DEBUG)
    logger = getLogger('disasm')
    if verbosity > 0:
        logger.setLevel(DEBUG)
        getLogger('binfmt').setLevel(DEBUG)

    # Handle options that don't require a binary.
    if args.list:
        print(parser.description)
        listSupported(logger)
        exit(0)

    # Parse section definitions.
    sectionDefs = []
    sectionArgs = args.section
    if sectionArgs is not None:
        for sectionArg in sectionArgs:
            try:
                sectionDef = _parseSectionDef(sectionArg)
            except ValueError as ex:
                logger.error('Bad section definition "%s": %s', sectionArg, ex)
                exit(2)
            else:
                sectionDefs.append(sectionDef)

    # Parse entry point definitions.
    entryDefs = []
    entryArgs = args.entry
    if entryArgs is not None:
        for entryArg in entryArgs:
            try:
                entryDef = _parseEntryDef(entryArg)
            except ValueError as ex:
                logger.error(
                    'Bad entry point definition "%s": %s', entryArg, ex
                    )
                exit(2)
            else:
                entryDefs.append(entryDef)

    # Open binary file.
    if args.binary is None:
        logger.error('No input file (binary) specified')
        print()
        parser.print_help()
        exit(2)
    try:
        with open(args.binary, 'rb') as binFile:
            with mmap(binFile.fileno(), 0, access=ACCESS_READ) as image:
                binfmt = determineBinaryFormat(
                    image, args.binary, args.binfmt, logger
                    )
                if binfmt is None:
                    exit(1)
                binary = binfmt(image)
                disassembleBinary(binary, sectionDefs, entryDefs, logger)
    except OSError as ex:
        logger.error(
            'Failed to read binary "%s": %s', ex.filename, ex.strerror
            )
        exit(1)

if __name__ == '__main__':
    main()
