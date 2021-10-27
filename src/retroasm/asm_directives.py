"""
This module contains classes for all supported directives.

Directives are represented in a syntax-neutral way: only when parsing or formatting
assembly do we care about syntax.
"""

from __future__ import annotations

from typing import Iterator, Protocol

from .expression import IntLiteral
from .reference import FixedValue, Reference
from .types import IntType, Width


class DataDirective:
    """Data definition directive like DB, DW etc."""

    __slots__ = ("_data",)

    @classmethod
    def unsigned(cls, width: Width, *values: int) -> DataDirective:
        valueType = IntType.u(width)
        data = []
        for value in values:
            if value < 0:
                raise ValueError(f"negative value: {value}")
            if value.bit_length() > width:
                raise ValueError(f"more than {width} bits: {value}")
            data.append(Reference(FixedValue(IntLiteral(value), width), valueType))
        return cls(*data)

    @classmethod
    def u8(cls, *values: int) -> DataDirective:
        return cls.unsigned(8, *values)

    @classmethod
    def u16(cls, *values: int) -> DataDirective:
        return cls.unsigned(16, *values)

    @classmethod
    def u32(cls, *values: int) -> DataDirective:
        return cls.unsigned(32, *values)

    @classmethod
    def u64(cls, *values: int) -> DataDirective:
        return cls.unsigned(64, *values)

    @property
    def width(self) -> Width:
        return self._data[0].width

    @property
    def data(self) -> tuple[Reference, ...]:
        return self._data

    def __init__(self, *data: Reference):
        widths = {ref.width for ref in data}
        if len(widths) != 1:
            if widths:
                widthsStr = ", ".join(str(width for width in sorted(widths)))
                raise ValueError(f"inconsistent widths: {widthsStr}")
            else:
                raise ValueError("no data")
        self._data = data

    def __str__(self) -> str:
        argsStr = ", ".join(str(ref.bits) for ref in self._data)
        return f"def{self.width} {argsStr}"

    def __repr__(self) -> str:
        argsStr = ", ".join(repr(ref) for ref in self._data)
        return f"{self.__class__.__name__}({argsStr})"


class StringDirective:
    """Data definition directive that can contain bytestrings."""

    @property
    def data(self) -> tuple[Reference | bytes, ...]:
        return self._data

    def __init__(self, *data: Reference | bytes):
        self._data = data

    def __str__(self) -> str:
        argsStr = ", ".join(
            str(item.bits) if isinstance(item, Reference) else repr(item)
            for item in self._data
        )
        return f"defb {argsStr}"

    def __repr__(self) -> str:
        argsStr = ", ".join(repr(item) for item in self._data)
        return f"{self.__class__.__name__}({argsStr})"


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
    def addr(self) -> Reference:
        return self._addr

    @classmethod
    def fromInt(cls, addr: int, typ: IntType) -> OriginDirective:
        return cls(Reference(FixedValue(IntLiteral(addr), typ.width), typ))

    def __init__(self, addr: Reference):
        self._addr = addr

    def __str__(self) -> str:
        return f"org {self._addr}"

    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self._addr!r})"
