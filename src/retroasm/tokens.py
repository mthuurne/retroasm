from __future__ import annotations

from collections.abc import Iterable, Iterator
from enum import Enum, EnumMeta
from re import Pattern
from typing import TYPE_CHECKING, Any, TypeVar
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
        new_class = super().__new__(
            cls,
            name,
            bases,
            # Work around typeshed using a private type for 'namespace'.
            namespace,  # type: ignore[arg-type]
        )
        new_class.pattern = new_class._compile_pattern()
        return new_class

    def _compile_pattern(cls) -> Pattern[str]:
        raise NotImplementedError


class TokenEnum(Enum, metaclass=TokenMeta):
    """
    Base class for token types.

    Each member should have as its value the regular expression for
    matching that kind of token.
    """

    def __init__(self, regex: str):
        self.regex = regex

    @classmethod
    def _compile_pattern(cls) -> Pattern[str]:
        patterns = [r"(\s+)"]
        patterns += (
            f"(?P<{name}>{token.regex})" for name, token in cls.__members__.items()
        )
        return re.compile("|".join(patterns))

    @classmethod
    def scan(cls: type[TokenT], location: InputLocation) -> Tokenizer[TokenT]:
        """Split an input string into tokens."""
        return Tokenizer(cls._iter_tokens(location))

    @classmethod
    def _iter_tokens(
        cls: type[TokenT], location: InputLocation
    ) -> Iterator[tuple[TokenT | None, InputLocation]]:
        for match in location.findMatches(cls.pattern):
            if match.groupName is None:
                # Skip whitespace.
                continue
            name = match.groupName
            assert name is not None
            match_location = match.group(name)
            assert match_location is not None
            yield cls[name], match_location
        # Sentinel.
        yield None, location.endLocation


TokenT = TypeVar("TokenT", bound=TokenEnum)


class Tokenizer(Iterator[tuple[TokenT, InputLocation]]):
    """
    Specialized iterator for tokenized text.

    Can be used like any other Python iterator, but the `eat()` method is often
    more convenient to check for and consume expected tokens.
    """

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

    def __init__(
        self, tokens: Iterable[tuple[TokenT | None, InputLocation]], start: int = 0
    ):
        """Use `TokenEnum.scan()` instead of calling this directly."""
        self._tokens = tuple(tokens)
        self._token_index = start
        self._advance()

    def copy(self) -> Tokenizer[TokenT]:
        """
        Make a copy of this tokenizer at its current position.

        The original and the copy can be used independently.
        """
        return Tokenizer(self._tokens, self._token_index - 1)

    def __next__(self) -> tuple[TokenT, InputLocation]:
        kind = self._kind
        if kind is None:
            raise StopIteration
        location = self._location
        self._advance()
        return kind, location

    def _advance(self) -> None:
        index = self._token_index
        self._kind, self._location = self._tokens[index]
        if index != len(self._tokens):
            self._token_index = index + 1

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
