from __future__ import annotations

from collections.abc import Iterable, Iterator, Sequence
from dataclasses import dataclass
from typing import override

from ..input import InputLocation
from ..reference import FixedValueReference


class Instruction:
    @property
    def mnemonic(self) -> Sequence[str | FixedValueReference]:
        return self._mnemonic

    def __init__(self, mnemonic: Iterable[str | FixedValueReference]):
        self._mnemonic = tuple(mnemonic)

    @override
    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self._mnemonic})"


# TODO: Union of actual operand members.
type Operand = object


class AsmStatement:
    """
    An assembly instruction or directive.

    TODO: Prefix keywords are not supported yet.
    """

    def __init__(self, keyword: InputLocation, operands: Iterable[Operand]):
        self._keyword = keyword
        self._operands = tuple(operands)

    @override
    def __str__(self) -> str:
        return f"{self._keyword.text} {','.join(str(op) for op in self._operands)}"


@dataclass(frozen=True)
class AsmLine:
    """
    A single assembly source line.
    """

    label: InputLocation | None = None
    statement: AsmStatement | None = None
    comment: InputLocation | None = None

    @override
    def __str__(self) -> str:
        label_str = "" if (label := self.label) is None else f"{label}:"
        comment_str = "" if (comment := self.comment) is None else f"  # {comment}"
        return f"{label_str:24}{self.statement}{comment_str}"


class AsmSource:
    """
    A collection of assembly source lines.
    """

    def __init__(self) -> None:
        self._lines: list[AsmLine] = []

    def __iter__(self) -> Iterator[AsmLine]:
        return iter(self._lines)

    def append_line(self, line: AsmLine) -> None:
        self._lines.append(line)
