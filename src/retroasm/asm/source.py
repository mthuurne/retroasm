from __future__ import annotations

from collections.abc import Iterable, Iterator, Sequence
from pathlib import Path
from typing import override

from ..input import ProblemCounter
from ..reference import FixedValueReference
from .directives import Directive, SourceIncludeDirective


class Instruction:
    @property
    def mnemonic(self) -> Sequence[str | FixedValueReference]:
        return self._mnemonic

    def __init__(self, mnemonic: Iterable[str | FixedValueReference]):
        self._mnemonic = tuple(mnemonic)

    @override
    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self._mnemonic})"


class AsmSource:
    """
    The parsed contents of a single assembly source file.

    The contents may be incomplete if any errors were encountered during parsing.
    """

    def __init__(self) -> None:
        self._statements: list[Directive | Instruction] = []
        self.problem_counter = ProblemCounter()

    def __iter__(self) -> Iterator[Directive | Instruction]:
        return iter(self._statements)

    def add_directive(self, directive: Directive) -> None:
        self._statements.append(directive)

    def add_instruction(self, instruction: Instruction) -> None:
        self._statements.append(instruction)

    def iter_source_includes(self) -> Iterator[Path]:
        """
        Iterate through the unresolved paths of source files included by this
        assembly file.

        As the paths are unresolved, it is possible the files do not exist or
        that the same file is referenced through different paths.
        """
        for statement in self._statements:
            if isinstance(statement, SourceIncludeDirective):
                yield statement.path
