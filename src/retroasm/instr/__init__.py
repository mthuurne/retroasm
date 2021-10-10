from __future__ import annotations

from importlib.abc import Traversable
from importlib.resources import files
from logging import Logger

from ..instrset import InstructionSet
from ..instrset_parser import parseInstrSet
from . import defs


def loadInstructionSet(
    path: Traversable, logger: Logger, wantSemantics: bool = True
) -> InstructionSet | None:
    try:
        return parseInstrSet(path, wantSemantics=wantSemantics)
    except OSError as ex:
        logger.error("%s: Failed to read instruction set: %s", path, ex.strerror)
        return None


def loadInstructionSetByName(
    name: str, logger: Logger, wantSemantics: bool = True
) -> InstructionSet | None:
    logger.info("Loading instruction set: %s", name)
    path = files(defs) / f"{name}.instr"
    return loadInstructionSet(path, logger, wantSemantics)
