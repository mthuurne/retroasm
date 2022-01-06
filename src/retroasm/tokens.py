from __future__ import annotations

from enum import Enum, EnumMeta
from typing import TYPE_CHECKING, Any, Iterator, Pattern, TypeVar, cast
import re

from .linereader import InputLocation


class TokenMeta(EnumMeta):
    """Metaclass for `TokenEnum`."""

    if TYPE_CHECKING:
        pattern: Pattern[str]

    def __new__(
        # pylint: disable=arguments-differ
        cls,
        name: str,
        bases: tuple[type, ...],
        namespace: dict[str, Any],
    ) -> TokenMeta:
        newClass = cast(type["TokenEnum"], super().__new__(cls, name, bases, namespace))
        newClass.pattern = newClass.compilePattern()
        return newClass


class TokenEnum(Enum, metaclass=TokenMeta):
    """
    Base class for token types.

    Each member should have as its value the regular expression for
    matching that kind of token.
    """

    def __init__(self, regex: str):
        self.regex = regex

    @classmethod
    def compilePattern(cls) -> Pattern[str]:
        patterns = [r"(\s+)"]
        patterns += (
            f"(?P<{name}>{token.regex})" for name, token in cls.__members__.items()
        )
        return re.compile("|".join(patterns))

    @classmethod
    def scan(cls: type[TokenT], location: InputLocation) -> Tokenizer[TokenT]:
        """Splits an input string into tokens."""
        return Tokenizer(cls, location)


TokenT = TypeVar("TokenT", bound=TokenEnum)


class Tokenizer(Iterator[tuple[TokenT, InputLocation]]):

    _kind: TokenT | None
    _location: InputLocation

    @property
    def end(self) -> bool:
        """Has the end of the input been reached?"""
        return self._kind is None

    @property
    def kind(self) -> TokenT:
        """
        The token kind of the current token.
        Raise `ValueError` if called at end of input.
        """
        kind = self._kind
        if kind is None:
            raise ValueError("out of tokens")
        return kind

    @property
    def value(self) -> str:
        """The text of the current token."""
        return self._location.text

    @property
    def location(self) -> InputLocation:
        """The input location of the current token."""
        return self._location

    def __init__(self, tokenClass: type[TokenT], location: InputLocation):
        self._tokenClass = tokenClass
        self._fullLocation = location
        self._tokens = tuple(
            match
            for match in location.findMatches(tokenClass.pattern)
            if match.groupName is not None  # skip whitespace
        )
        self._tokenIndex = 0
        self._advance()

    def __next__(self) -> tuple[TokenT, InputLocation]:
        kind = self._kind
        if kind is None:
            raise StopIteration
        location = self._location
        self._advance()
        return kind, location

    def _advance(self) -> None:
        index = self._tokenIndex
        if index == len(self._tokens):
            self._kind = None
            self._location = self._fullLocation.endLocation
        else:
            match = self._tokens[index]
            self._tokenIndex = index + 1
            name = match.groupName
            assert name is not None
            self._kind = self._tokenClass[name]
            matchLocation = match.group(name)
            assert matchLocation is not None
            self._location = matchLocation

    def peek(self, kind: TokenT, value: str | None = None) -> bool:
        """
        Check whether the current token matches the given kind and,
        if specified, also the given value.
        Return True for a match, False otherwise.
        """
        return self._kind is kind and (value is None or self.value == value)

    def eat(self, kind: TokenT, value: str | None = None) -> InputLocation | None:
        """
        Consume the current token if it matches the given kind and,
        if specified, also the given value.
        Return the token's input location if the token was consumed,
        or None if no match was found.
        """
        found = self.peek(kind, value)
        if found:
            location = self._location
            self._advance()
            return location
        else:
            return None
