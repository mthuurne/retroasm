#!/usr/bin/env python3

from retroasm.instrset_parser import parseInstrSet
from retroasm.linereader import LineReaderFormatter

import logging

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

def checkInstrSet(pathname, dumpNoSubs, dumpSubs, logger):
    logger.info('checking: %s', pathname)
    instrSet = parseInstrSet(pathname, logger)

    if instrSet is not None:
        if dumpNoSubs:
            dumpDecoders(instrSet, False)
        if dumpSubs:
            dumpDecoders(instrSet, True)

def main():
    from argparse import ArgumentParser
    from os import walk
    from os.path import exists, isdir, isfile
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
    dirs = []
    for path in args.instr:
        if not exists(path):
            print('No such file or directory:', path, file=stderr)
            exit(1)
        elif isdir(path):
            dirs.append(path)
        elif isfile(path):
            files.append(path)
        else:
            print('Not a regular file or directory:', path, file=stderr)
            exit(1)
    if dirs:
        for dirName in dirs:
            for dirPath, subdirList, fileList in walk(dirName):
                for fileName in fileList:
                    if fileName.endswith('.instr'):
                        files.append(dirPath + '/' + fileName)
    if not files:
        print('No definition files found (*.instr)', file=stderr)
        exit(1)

    logger = setupLogging()

    for path in files:
        checkInstrSet(path, args.dump_decoders, args.dump_decoders_subs, logger)

if __name__ == '__main__':
    main()
