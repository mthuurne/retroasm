"""
This module contains classes for all supported directives.

Directives are represented in a syntax-neutral way: only when parsing or formatting
assembly do we care about syntax.
"""

from __future__ import annotations

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
