"""
This module contains classes for all supported directives.

Directives are represented in a syntax-neutral way: only when parsing or formatting
assembly do we care about syntax.
"""

from __future__ import annotations

from abc import abstractmethod
from collections.abc import Iterator
from dataclasses import dataclass
from pathlib import Path
from typing import Protocol, TypeAlias, override

from ..parser.expression_nodes import IdentifierNode, NumberNode, ParseNode
from ..types import Width


class DataDirective:
    """Data definition directive like DB, DW etc."""

    __slots__ = ("_data", "_width")

    @classmethod
    def symbol(cls, width: Width, *names: str) -> DataDirective:
        return cls(width, *(IdentifierNode(name) for name in names))

    @classmethod
    def literal(cls, width: Width, *values: int) -> DataDirective:
        return cls(width, *(NumberNode(value, width) for value in values))

    @property
    def width(self) -> Width:
        return self._width

    @property
    def data(self) -> tuple[ParseNode, ...]:
        return self._data

    def __init__(self, width: Width, *data: ParseNode):
        self._width = width
        self._data = data

    @override
    def __str__(self) -> str:
        args_str = ", ".join(str(elem) for elem in self._data)
        return f"def{self._width} {args_str}"

    @override
    def __repr__(self) -> str:
        args_str = ", ".join(repr(ref) for ref in self._data)
        return f"{self.__class__.__name__}({args_str})"


class StringDirective:
    """Data definition directive that can contain bytestrings."""

    @property
    def data(self) -> tuple[ParseNode | bytes, ...]:
        return self._data

    def __init__(self, *data: ParseNode | bytes):
        self._data = data

    @override
    def __str__(self) -> str:
        args_str = ", ".join(
            str(item) if isinstance(item, ParseNode) else repr(item)
            for item in self._data
        )
        return f"defb {args_str}"

    @override
    def __repr__(self) -> str:
        args_str = ", ".join(repr(item) for item in self._data)
        return f"{self.__class__.__name__}({args_str})"


@dataclass(frozen=True, slots=True)
class SpaceDirective:
    """Data directive that fills an area with a single byte value."""

    size: ParseNode
    value: ParseNode | None = None

    @override
    def __str__(self) -> str:
        if self.value is None:
            return f"defs {self.size}"
        else:
            return f"defs {self.size},{self.value}"


@dataclass(frozen=True, slots=True)
class BinaryIncludeDirective:
    """Data definition directive that inserts a binary file in the output as-is."""

    path: Path

    @override
    def __str__(self) -> str:
        return f"incbin {self.path}"


@dataclass(frozen=True, slots=True)
class SourceIncludeDirective:
    """Directive that inserts the contents of one assembly source file into another."""

    path: Path

    @override
    def __str__(self) -> str:
        return f"include {self.path}"


class StructuredData(Protocol):
    """Protocol for data areas with a struct-like layout."""

    @property
    @abstractmethod
    def encoded(self) -> bytes:
        """The data in encoded form."""

    @property
    @abstractmethod
    def directives(self) -> Iterator[DataDirective | StringDirective]:
        """Yield the data directives that define this data area."""

    def __len__(self) -> int:
        return len(self.encoded)


@dataclass(frozen=True, slots=True)
class ConditionalDirective:
    """
    Marks the start of a block of statements that may or may not be assembled
    depending on a condition.
    """

    cond: ParseNode | None
    """
    Condition that must evaluate to true (non-zero) for the following block
    to be assembled, or None for an unconditional final clause (ELSE).
    """

    chain: bool
    """
    Is this a follow-up condition (ELSEIF/ELSE) in a chain?
    """

    @override
    def __str__(self) -> str:
        cond = self.cond
        if cond is None:
            return "else"
        elif self.chain:
            return f"elseif {cond}"
        else:
            return f"if {cond}"


class ConditionalEnd:
    """Marks the end of a conditional block."""

    @override
    def __str__(self) -> str:
        return "endif"


@dataclass(frozen=True, slots=True)
class OriginDirective:
    """Defines the address that code is expected to execute at."""

    addr: ParseNode

    @override
    def __str__(self) -> str:
        return f"org {self.addr}"


@dataclass(frozen=True, slots=True)
class LabelDirective:
    """Defines a label."""

    name: str
    value: ParseNode | None

    @override
    def __str__(self) -> str:
        if self.value is None:
            return f"{self.name}:"
        else:
            return f"{self.name}: equ {self.value}"


class DummyDirective:
    @override
    def __str__(self) -> str:
        return "(not implemented yet)"


Directive: TypeAlias = (
    DataDirective
    | StringDirective
    | SpaceDirective
    | OriginDirective
    | LabelDirective
    | BinaryIncludeDirective
    | SourceIncludeDirective
    | ConditionalDirective
    | ConditionalEnd
    | DummyDirective
)
