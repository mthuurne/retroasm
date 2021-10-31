"""
This module contains support for symbols in assembly sources.

A symbol is a name which can be used in expressions.

Two kinds of symbols are supported:
- labels, which contain an address that corresponds to a certain source location
- constants, which are compuated values defined via a directive
"""

from __future__ import annotations

from .expression import Expression
from .types import Width, maskForWidth


class SymbolValue(Expression):
    """
    A symbol when its value is used as part of an expression.

    When assembling, the expression that computes the actual value can be substituted
    for this once it is known.

    Symbol names are case-preserving: they compare case-insensitive, but stick to
    the casing that they were constructed with.
    """

    __slots__ = ("_name", "_width")

    @property
    def mask(self) -> int:
        return maskForWidth(self._width)

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

    def _ctorargs(self) -> tuple[str, Width]:
        return (self._name, self._width)

    def _equals(self, other: SymbolValue) -> bool:
        return self._name.casefold() == other._name.casefold()

    def __str__(self) -> str:
        return self._name

    @property
    def complexity(self) -> int:
        return 1
