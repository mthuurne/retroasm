from __future__ import annotations

from collections.abc import Iterator
from importlib.resources import files
from importlib.resources.abc import Traversable
from logging import ERROR, Logger, getLogger

from ..instrset import InstructionSet
from ..parser.instrset_parser import InstructionSetParser
from . import defs


def builtin_instruction_set_path(name: str) -> Traversable:
    return files(defs) / f"{name}.instr"


def load_instruction_set(
    path: Traversable, logger: Logger | None = None, want_semantics: bool = True
) -> InstructionSet | None:
    """
    Load an instruction set definition from file.

    The best logging is achieved if `LineReaderFormatter` or another formatter
    that incorporates the `location` extra is used.

    Returns the instruction set, or None if it could not be loaded.
    """
    if logger is None:
        logger = getLogger(__name__)
    # Log only errors, to avoid confusing the user with informational messages
    # from the instruction set parser when for example assembling.
    parser_logger = logger.getChild("parser")
    parser_logger.setLevel(ERROR)

    try:
        return InstructionSetParser.parse_file(
            path, parser_logger, want_semantics=want_semantics
        )
    except OSError as ex:
        logger.error("%s: Failed to read instruction set: %s", path, ex.strerror)
        return None


def load_instruction_set_by_name(
    name: str, logger: Logger | None = None, want_semantics: bool = True
) -> InstructionSet | None:
    """
    Load the named built-in instruction set definition.

    The best logging is achieved if `LineReaderFormatter` or another formatter
    that incorporates the `location` extra is used.

    Returns the instruction set, or None if it could not be loaded.
    """
    if logger is None:
        logger = getLogger(__name__)
    logger.info("Loading instruction set: %s", name)
    path = builtin_instruction_set_path(name)
    return load_instruction_set(path, logger, want_semantics)


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

    def __iter__(self) -> Iterator[str]:
        """Iterate through the names of the instruction sets that can be provided."""
        raise NotImplementedError


class InstructionSetDirectory(InstructionSetProvider):
    """
    Instruction set provider that loads and caches definitions from a directory.

    Instruction sets will be loaded including semantics.
    """

    def __init__(self, path: Traversable, logger: Logger | None = None):
        self._path = path
        self._logger = logger or getLogger(__name__)
        self._cache: dict[str, InstructionSet | None] = {}

    def __getitem__(self, name: str) -> InstructionSet | None:
        try:
            return self._cache[name]
        except KeyError:
            logger = self._logger
            logger.info("Loading instruction set: %s", name)
            path = self._path / f"{name}.instr"
            instr_set = load_instruction_set(path, logger)
            self._cache[name] = instr_set
            return instr_set

    def __iter__(self) -> Iterator[str]:
        for path in self._path.iterdir():
            if path.is_file():
                name = path.name
                if name.endswith(".instr"):
                    yield name[:-6]


builtin_instruction_sets = InstructionSetDirectory(files(defs))
"""
Provider for instruction sets from the RetroAsm installation.
"""
