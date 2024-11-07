"""
This module contains support for symbols in assembly sources.

A symbol is a name which can be used in expressions.

Two kinds of symbols are supported:
- labels, which contain an address that corresponds to a certain source location
- constants, which are computed values defined via a directive
"""

from __future__ import annotations

from typing import override

from .expression import Expression
from .types import Width, mask_for_width
from .utils import SingletonFromABC


class SymbolValue(Expression):
    """
    A symbol when its value is used as part of an expression.

    When assembling, the expression that computes the actual value can be substituted
    for this once it is known.
    """

    __slots__ = ("_name", "_width")

    @property
    @override
    def mask(self) -> int:
        return mask_for_width(self._width)

    @property
    def name(self) -> str:
        """The name of this symbol."""
        return self._name

    @property
    def width(self) -> Width:
        """The width of this symbol's value."""
        return self._width

    def __init__(self, name: str, width: Width):
        super().__init__()
        self._name = name
        self._width = width

    @override
    def _ctorargs(self) -> tuple[str, Width]:
        return (self._name, self._width)

    @override
    def _equals(self, other: SymbolValue) -> bool:
        return self._name == other._name

    @override
    def __str__(self) -> str:
        return self._name

    @property
    @override
    def complexity(self) -> int:
        return 1


class CurrentAddress(Expression, metaclass=SingletonFromABC):
    """
    A placeholder for the current program counter value at a certain location in
    the program.
    """

    __slots__ = ()

    @property
    @override
    def mask(self) -> int:
        return -1

    @override
    def _ctorargs(self) -> tuple[()]:
        return ()

    @override
    def _equals(self, other: CurrentAddress) -> bool:
        # Depending on the location in the program, the value will differ.
        return False

    @override
    def __str__(self) -> str:
        return "$"

    @property
    @override
    def complexity(self) -> int:
        return 1
