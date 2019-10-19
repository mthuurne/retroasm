from logging import INFO, Logger, StreamHandler, getLogger
from pathlib import Path
from sys import stderr
from typing import Iterable, List, NoReturn

from click import Path as PathArg, argument, command, group, option

from .asm_parser import readSource
from .instrset import InstructionSet
from .instrset_parser import parseInstrSet
from .linereader import DelayedError, LineReaderFormatter


def setupLogging() -> Logger:
    handler = StreamHandler()
    formatter = LineReaderFormatter()
    handler.setFormatter(formatter)
    logger = getLogger()
    logger.addHandler(handler)
    logger.setLevel(INFO)
    return logger

@command()
@option('-i', '--instr', required=True, help='Instruction set.')
@argument('source', type=PathArg(exists=True))
def asm(instr: str, source: str) -> None:
    """Assembler using the RetroAsm toolkit."""

    logger = setupLogging()

    instrPath = Path(f'defs/instr/{instr}.instr')
    try:
        instrSet = parseInstrSet(instrPath, wantSemantics=False)
    except OSError as ex:
        logger.error(
            'Failed to read instruction set "%s": %s', ex.filename, ex.strerror
            )
        exit(1)
    if instrSet is None:
        exit(1)

    sourcePath = Path(source)
    try:
        readSource(sourcePath, instrSet)
    except OSError as ex:
        logger.error(
            'Failed to read source "%s": %s', ex.filename, ex.strerror
            )
        exit(1)
    except DelayedError:
        exit(1)

def dumpDecoders(instrSet: InstructionSet, submodes: bool) -> None:

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
        instrSet.getDecoder(frozenset(flags)).dump(submodes=submodes)

def checkInstrSet(
        path: Path,
        dumpNoSubs: bool,
        dumpSubs: bool,
        logger: Logger
        ) -> int:

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

@command()
@option('--dump-decoders', is_flag=True,
        help='Dump the instruction decoder tree, excluding submodes.')
@option('--dump-decoders-subs', is_flag=True,
        help='Dump the instruction decoder tree, including submodes.')
@argument('instr', nargs=-1, type=PathArg(exists=True))
def checkdef(
        instr: Iterable[str],
        dump_decoders: bool,
        dump_decoders_subs: bool
        ) -> NoReturn:
    """Check instruction set definition files."""

    files: List[Path] = []
    for pathName in instr:
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
        checkInstrSet(path, dump_decoders, dump_decoders_subs, logger)
        for path in files
        ))

@group()
def main() -> None:
    """Command line interface to the RetroAsm assembly toolkit."""
    pass

main.add_command(asm)
main.add_command(checkdef)
