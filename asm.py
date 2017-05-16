#!/usr/bin/env python3

from retroasm.asm_parser import readSource
from retroasm.instrset_parser import parseInstrSet
from retroasm.linereader import DelayedError, LineReaderFormatter

import logging

def setupLogging():
    handler = logging.StreamHandler()
    formatter = LineReaderFormatter()
    handler.setFormatter(formatter)
    logger = logging.getLogger()
    logger.addHandler(handler)
    logger.setLevel(logging.INFO)
    return logger

def main():
    from argparse import ArgumentParser

    parser = ArgumentParser(description='Assembler using the RetroAsm toolkit.')
    parser.add_argument(
        '-i', '--instr', required=True,
        help='instruction set (required)'
        )
    parser.add_argument(
        'source',
        help='file containing source code to assemble'
        )
    args = parser.parse_args()

    logger = setupLogging()

    instrPath = 'defs/instr/%s.instr' % args.instr
    try:
        instrSet = parseInstrSet(instrPath)
    except OSError as ex:
        logger.error(
            'Failed to read instruction set "%s": %s', ex.filename, ex.strerror
            )
        exit(1)
    if instrSet is None:
        exit(1)

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
