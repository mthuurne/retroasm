"""
This module contains classes for all supported directives.

Directives are represented in a syntax-neutral way: only when parsing or formatting
assembly do we care about syntax.
"""

from __future__ import annotations

from collections.abc import Iterator
from dataclasses import dataclass
from pathlib import Path
from typing import Protocol

from .reference import FixedValueReference, int_reference, symbol_reference
from .types import IntType, Width


class DataDirective:
    """Data definition directive like DB, DW etc."""

    __slots__ = ("_data",)

    @classmethod
    def symbol(cls, typ: IntType, *names: str) -> DataDirective:
        return cls(*(symbol_reference(name, typ) for name in names))

    @classmethod
    def literal(cls, typ: IntType, *values: int) -> DataDirective:
        return cls(*(int_reference(value, typ) for value in values))

    @classmethod
    def u8(cls, *values: int) -> DataDirective:
        return cls.literal(IntType.u(8), *values)

    @classmethod
    def u16(cls, *values: int) -> DataDirective:
        return cls.literal(IntType.u(16), *values)

    @classmethod
    def u32(cls, *values: int) -> DataDirective:
        return cls.literal(IntType.u(32), *values)

    @classmethod
    def u64(cls, *values: int) -> DataDirective:
        return cls.literal(IntType.u(64), *values)

    @property
    def width(self) -> Width:
        return self._data[0].width

    @property
    def data(self) -> tuple[FixedValueReference, ...]:
        return self._data

    def __init__(self, *data: FixedValueReference):
        widths = {ref.width for ref in data}
        if len(widths) != 1:
            if widths:
                widths_str = ", ".join(str(width for width in sorted(widths)))
                raise ValueError(f"inconsistent widths: {widths_str}")
            else:
                raise ValueError("no data")
        self._data = data

    def __str__(self) -> str:
        args_str = ", ".join(str(ref.bits) for ref in self._data)
        return f"def{self.width} {args_str}"

    def __repr__(self) -> str:
        args_str = ", ".join(repr(ref) for ref in self._data)
        return f"{self.__class__.__name__}({args_str})"


class StringDirective:
    """Data definition directive that can contain bytestrings."""

    @property
    def data(self) -> tuple[FixedValueReference | bytes, ...]:
        return self._data

    def __init__(self, *data: FixedValueReference | bytes):
        self._data = data

    def __str__(self) -> str:
        args_str = ", ".join(
            str(item.bits) if isinstance(item, FixedValueReference) else repr(item)
            for item in self._data
        )
        return f"defb {args_str}"

    def __repr__(self) -> str:
        args_str = ", ".join(repr(item) for item in self._data)
        return f"{self.__class__.__name__}({args_str})"


@dataclass(frozen=True, slots=True)
class BinaryIncludeDirective:
    """Data definition directive that inserts a binary file in the output as-is."""

    path: Path

    def __str__(self) -> str:
        return f"incbin {self.path}"


@dataclass(frozen=True, slots=True)
class SourceIncludeDirective:
    """Directive that inserts the contents of one assembly source file into another."""

    path: Path

    def __str__(self) -> str:
        return f"include {self.path}"


class StructuredData(Protocol):
    """Protocol for data areas with a struct-like layout."""

    @property
    def encoded(self) -> bytes:
        """The data in encoded form."""
        raise NotImplementedError

    @property
    def directives(self) -> Iterator[DataDirective | StringDirective]:
        """Yield the data directives that define this data area."""
        raise NotImplementedError

    def __len__(self) -> int:
        return len(self.encoded)


class OriginDirective:
    """Defines the address that code is expected to execute at."""

    __slots__ = ("_addr",)

    @property
    def addr(self) -> FixedValueReference:
        return self._addr

    @classmethod
    def from_int(cls, addr: int, typ: IntType) -> OriginDirective:
        return cls(int_reference(addr, typ))

    def __init__(self, addr: FixedValueReference):
        self._addr = addr

    def __str__(self) -> str:
        return f"org {self._addr}"

    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self._addr!r})"
