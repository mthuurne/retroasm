from logging import INFO, Logger, StreamHandler, getLogger
from pathlib import Path

from click import Path as PathArg, argument, command, group, option

from .asm_parser import readSource
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

@group()
def main() -> None:
    """Command line interface to the RetroAsm assembly toolkit."""
    pass

main.add_command(asm)
