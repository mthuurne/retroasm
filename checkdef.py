#!/usr/bin/env python3

from retroasm.instrset_parser import checkInstrSet
from retroasm.linereader import LineReaderFormatter

import logging
from sys import stderr

def setupLogging():
    handler = logging.StreamHandler()
    formatter = LineReaderFormatter()
    handler.setFormatter(formatter)
    logger = logging.getLogger()
    logger.addHandler(handler)
    logger.setLevel(logging.INFO)
    return logger

def main():
    from os import walk
    from os.path import exists, isdir, isfile
    from sys import argv
    if len(argv) > 1:
        files = []
        dirs = []
        for arg in argv[1:]:
            if not exists(arg):
                print('No such file or directory:', arg, file=stderr)
                exit(1)
            elif isdir(arg):
                dirs.append(arg)
            elif isfile(arg):
                files.append(arg)
            else:
                print('Not a regular file or directory:', arg, file=stderr)
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
            checkInstrSet(path, logger)
    else:
        print('usage: checkdef.py [file|dir]+\n', file=stderr)
        print('Check instruction set definition files.\n', file=stderr)
        exit(2)

if __name__ == '__main__':
    main()
