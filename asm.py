#!/usr/bin/env python3

from retroasm.asm_parser import readSource
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
        'source',
        help='file containing source code to assemble'
        )
    args = parser.parse_args()

    logger = setupLogging()

    try:
        readSource(args.source)
    except OSError as ex:
        logger.error(
            'Failed to read source "%s": %s', ex.filename, ex.strerror
            )
        exit(1)
    except DelayedError as ex:
        exit(1)

if __name__ == '__main__':
    main()
