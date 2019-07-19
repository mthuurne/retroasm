from __future__ import annotations

from enum import Enum, EnumMeta
from typing import (
    TYPE_CHECKING, Any, Dict, Iterator, Optional, Pattern, Tuple, Type,
    TypeVar, cast
)
import re

from .linereader import InputLocation


class TokenMeta(EnumMeta):
    """Metaclass for `TokenEnum`."""

    if TYPE_CHECKING:
        pattern: Pattern[str]

    def __new__(cls,
                name: str,
                bases: Tuple[type, ...],
                namespace: Dict[str, Any]
                ) -> TokenMeta:
        newClass = cast(Type['TokenEnum'],
                        super().__new__(cls, name, bases, namespace))
        newClass.pattern = newClass.compilePattern()
        return newClass

class TokenEnum(Enum, metaclass=TokenMeta):
    """Base class for token types.

    Each member should have as its value the regular expression for
    matching that kind of token.
    """

    def __init__(self, regex: str):
        self.regex = regex

    @classmethod
    def compilePattern(cls) -> Pattern[str]:
        patterns = [r'(\s+)']
        patterns += (
            f'(?P<{name}>{token.regex})'
            for name, token in cls.__members__.items()
            )
        return re.compile('|'.join(patterns))

    @classmethod
    def scan(cls: Type[TokenT], location: InputLocation) -> Tokenizer[TokenT]:
        """Splits an input string into tokens."""
        return Tokenizer(cls, location)

TokenT = TypeVar('TokenT', bound=TokenEnum)

class Tokenizer(Iterator[Tuple[TokenT, InputLocation]]):

    _kind: Optional[TokenT]
    _location: InputLocation

    @property
    def end(self) -> bool:
        """Has the end of the input been reached?"""
        return self._kind is None

    @property
    def kind(self) -> TokenT:
        """The token kind of the current token.
        Raise `ValueError` if called at end of input.
        """
        kind = self._kind
        if kind is None:
            raise ValueError('out of tokens')
        return kind

    @property
    def value(self) -> str:
        """The text of the current token."""
        return self._location.text

    @property
    def location(self) -> InputLocation:
        """The input location of the current token."""
        return self._location

    def __init__(self, tokenClass: Type[TokenT], location: InputLocation):
        self._tokens = location.findMatches(tokenClass.pattern)
        self._tokenClass = tokenClass
        self._fullLocation = location
        self._advance()

    def __next__(self) -> Tuple[TokenT, InputLocation]:
        kind = self._kind
        if kind is None:
            raise StopIteration
        location = self._location
        self._advance()
        return kind, location

    def _advance(self) -> None:
        while True:
            try:
                match = next(self._tokens)
            except StopIteration:
                kind = None
                location = self._fullLocation.endLocation
            else:
                name = match.groupName
                if name is None:
                    # Skip whitespace.
                    continue
                kind = self._tokenClass[name]
                matchLocation = match.group(name)
                assert matchLocation is not None
                location = matchLocation
            break
        self._kind = kind
        self._location = location

    def peek(self, kind: TokenT, value: Optional[str] = None) -> bool:
        """Check whether the current token matches the given kind and,
        if specified, also the given value.
        Return True for a match, False otherwise.
        """
        return self._kind is kind and (value is None or self.value == value)

    def eat(self,
            kind: TokenT,
            value: Optional[str] = None
            ) -> Optional[InputLocation]:
        """Consume the current token if it matches the given kind and,
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
