#!/usr/bin/env python3

from retroasm.instrset_parser import parseInstrSet
from retroasm.binfmt import (
    detectBinaryFormat, getBinaryFormat, iterBinaryFormatNames
    )
from retroasm.disasm import disassemble
from retroasm.linereader import DelayedError, LineReaderFormatter

from mmap import ACCESS_READ, mmap
from pathlib import Path
import logging

def setupLogging(level):
    handler = logging.StreamHandler()
    formatter = LineReaderFormatter()
    handler.setFormatter(formatter)
    logger = logging.getLogger()
    logger.addHandler(handler)
    logger.setLevel(level)
    return logger

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

def main():
    from argparse import ArgumentParser

    parser = ArgumentParser(
        description='Disassembler using the RetroAsm toolkit.'
        )
    parser.add_argument(
        '-b', '--binfmt',
        help='binary format (default: autodetect)'
        )
    parser.add_argument(
        '-l', '--list', action='store_true',
        help='list available binary formats and instruction sets, then exit'
        )
    parser.add_argument(
        '-v', '--verbose', action='store_const',
        const=logging.DEBUG, default=logging.INFO,
        help='list available binary formats and instruction sets, then exit'
        )
    parser.add_argument(
        'binary', nargs='?',
        help='file containing code to disassemble'
        )
    args = parser.parse_args()

    logger = setupLogging(args.verbose)

    # Handle options that don't require a binary.
    if args.list:
        print(parser.description)
        listSupported(logger)
        exit(0)

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

                # Load instruction set definitions.
                entryPoints = list(binary.iterEntryPoints())
                instrSets = {}
                for entryPoint in entryPoints:
                    name = entryPoint.instrSetName
                    if name not in instrSets:
                        logger.info('Loading instruction set: %s', name)
                        instrPath = 'defs/instr/%s.instr' % name
                        try:
                            instrSets[name] = parseInstrSet(
                                instrPath, wantSemantics=False
                                )
                        except OSError as ex:
                            logger.error(
                                'Failed to read instruction set "%s": %s',
                                ex.filename, ex.strerror
                                )
                            instrSets[name] = None

                # Disassemble.
                for entryPoint in entryPoints:
                    instrSet = instrSets[entryPoint.instrSetName]
                    if instrSet is None:
                        logger.warning(
                            'Skipping disassembly of offset $%x due to unknown '
                            'instruction set "%s"',
                            entryPoint.offset, entryPoint.instrSetName
                            )
                        continue
                    offset = entryPoint.offset
                    size = entryPoint.size
                    if size is None:
                        size = image.size() - offset
                    disassemble(instrSet, image, offset, entryPoint.addr, size)
    except OSError as ex:
        logger.error(
            'Failed to read binary "%s": %s', ex.filename, ex.strerror
            )
        exit(1)

    if False:
        instrPath = 'defs/instr/%s.instr' % args.instr
        try:
            instrSet = parseInstrSet(instrPath, wantSemantics=False)
        except OSError as ex:
            logger.error(
                'Failed to read instruction set "%s": %s', ex.filename, ex.strerror
                )
            exit(1)
        if instrSet is None:
            exit(1)

    if False:
        try:
            readSource(args.source, instrSet)
        except OSError as ex:
            logger.error(
                'Failed to read source "%s": %s', ex.filename, ex.strerror
                )
            exit(1)
        except DelayedError as ex:
            exit(1)

if __name__ == '__main__':
    main()
