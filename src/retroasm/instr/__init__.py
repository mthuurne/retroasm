from __future__ import annotations

from importlib.abc import Traversable
from importlib.resources import files
from logging import Logger, getLogger

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


class InstructionSetProvider:
    """
    Abstract base class for providers that can look up instruction sets by name.
    """

    def __getitem__(self, name: str) -> InstructionSet | None:
        """
        Return the instruction set with the given name, or `None` if it is not
        available for any reason.
        """
        raise NotImplementedError


class InstructionSetDirectory(InstructionSetProvider):
    """
    Instruction set provider that loads and cachces definitions from a directory.

    Instruction sets will be loaded including semantics.
    """

    def __init__(self, path: Traversable, logger: Logger):
        self._path = path
        self._logger = logger
        self._cache: dict[str, InstructionSet | None] = {}

    def __getitem__(self, name: str) -> InstructionSet | None:
        try:
            return self._cache[name]
        except KeyError:
            logger = self._logger
            logger.info("Loading instruction set: %s", name)
            path = self._path / f"{name}.instr"
            instrSet = loadInstructionSet(path, logger)
            self._cache[name] = instrSet
            return instrSet


builtinInstructionSets = InstructionSetDirectory(files(defs), getLogger(__name__))
"""
Provider for instruction sets from the RetroAsm installation.
"""