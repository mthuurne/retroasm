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
    from sys import argv, stderr
    if len(argv) == 2:
        path = argv[1]

        logger = setupLogging()

        try:
            readSource(path)
        except OSError as ex:
            logger.error(
                'Failed to read source "%s": %s', ex.filename, ex.strerror
                )
            exit(1)
        except DelayedError as ex:
            exit(1)
    else:
        print('usage: asm.py [source]\n', file=stderr)
        print('Assemble a given source file.\n', file=stderr)
        exit(2)

if __name__ == '__main__':
    main()
