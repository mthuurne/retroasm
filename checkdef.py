#!/usr/bin/env python3

import logging
import sys

sys.path.append('src')

from retroasm.instrset_parser import parseInstrSet
from retroasm.linereader import LineReaderFormatter

def setupLogging():
    handler = logging.StreamHandler()
    formatter = LineReaderFormatter()
    handler.setFormatter(formatter)
    logger = logging.getLogger()
    logger.addHandler(handler)
    logger.setLevel(logging.INFO)
    return logger

def dumpDecoders(instrSet, submodes):
    flagCombos = sorted(
        sorted(flags)
        for flags in instrSet.decodeFlagCombinations
        )
    for flags in flagCombos:
        print()
        if flags:
            print('with decode flag%s %s:' % (
                '' if len(flags) == 1 else 's', ', '.join(flags)
                ))
            print()
        instrSet.getDecoder(flags).dump(submodes=submodes)

def checkInstrSet(path, dumpNoSubs, dumpSubs, logger):
    logger.info('checking: %s', path)
    try:
        instrSet = parseInstrSet(path, logger)
    except OSError as ex:
        logger.error('Could not load instruction set: %s', ex)
        return 1

    if instrSet is None:
        return 1

    if dumpNoSubs:
        dumpDecoders(instrSet, False)
    if dumpSubs:
        dumpDecoders(instrSet, True)
    return 0

def main():
    from argparse import ArgumentParser
    from pathlib import Path
    from sys import stderr

    parser = ArgumentParser(
        description='Check instruction set definition files.'
        )
    parser.add_argument(
        '--dump-decoders', action='store_true',
        help='dump the instruction decoder tree, excluding submodes'
        )
    parser.add_argument(
        '--dump-decoders-subs', action='store_true',
        help='dump the instruction decoder tree, including submodes'
        )
    parser.add_argument(
        'instr', nargs='+',
        help='file or directory containing instruction set definitions'
        )
    args = parser.parse_args()

    files = []
    for pathName in args.instr:
        path = Path(pathName)
        if path.is_dir():
            files += path.glob('**/*.instr')
        else:
            files.append(path)
    if not files:
        print('No definition files found (*.instr)', file=stderr)
        exit(1)

    logger = setupLogging()
    exit(max(
        checkInstrSet(path, args.dump_decoders, args.dump_decoders_subs, logger)
        for path in files
        ))

if __name__ == '__main__':
    main()
