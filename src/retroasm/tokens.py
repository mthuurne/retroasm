from __future__ import annotations

from collections.abc import Iterable, Iterator
from enum import Enum, EnumMeta
from re import Pattern
from typing import Any, Callable, TypeAlias, TypeVar, cast
import re

from .linereader import InputLocation


class TokenMeta(EnumMeta):
    """Metaclass for `TokenEnum`."""

    pattern: Pattern[str]

    def __new__(
        mcs,
        name: str,
        bases: tuple[type, ...],
        namespace: dict[str, Any],
        **kwargs: Any,
    ) -> TokenMeta:
        new_class = super().__new__(
            mcs,
            name,
            bases,
            # Work around typeshed using a private type for 'namespace'.
            namespace,  # type: ignore[arg-type]
            **kwargs,
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
    def _iter_tokens(cls: type[TokenT], location: InputLocation) -> TokenStream[TokenT]:
        for match in location.find_matches(cls.pattern):
            name = match.group_name
            if name is None:
                # Skip whitespace.
                continue
            match_location = match.group(name)
            assert match_location is not None
            yield cls[name], match_location
        # Sentinel.
        yield None, location.end_location


T = TypeVar("T")
TokenT = TypeVar("TokenT", bound=TokenEnum)
TokenStream: TypeAlias = Iterable[tuple[TokenT | None, InputLocation]]


class Tokenizer(Iterator[tuple[TokenT, InputLocation]]):
    """
    Specialized iterator for tokenized text.

    Can be used like any other Python iterator, but the `eat()` method is often
    more convenient to check for and consume expected tokens.
    """

    _token_class: type[TokenT]

    def __class_getitem__(cls, item: type[TokenT]) -> type[Tokenizer[TokenT]]:
        class SpecializedTokenizer(
            super().__class_getitem__(item)  # type: ignore[misc]
        ):
            _token_class = item

        return SpecializedTokenizer

    @classmethod
    def get_token_class(cls) -> type[TokenT]:
        try:
            return cls._token_class
        except AttributeError:
            raise TypeError(
                "Tokenizer must be specialized first, "
                "for example Tokenizer[MyTokenEnum]"
            ) from None

    @classmethod
    def scan(cls: type[T], location: InputLocation) -> T:
        """Split an input string into tokens."""
        token_class = cast(Tokenizer[TokenT], cls).get_token_class()
        constructor = cast(Callable[[TokenStream[TokenT]], T], cls)
        return constructor(token_class._iter_tokens(location))

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
        """Use `scan()` instead of calling this directly."""
        self._tokens = tuple(tokens)
        self._token_index = start
        self._advance()

    def copy(self: T) -> T:
        """
        Make a copy of this tokenizer at its current position.

        The original and the copy can be used independently.
        """
        constructor = cast(Callable[[TokenStream[TokenT], int], T], self.__class__)
        assert isinstance(self, Tokenizer)
        return constructor(self._tokens, self._token_index - 1)

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
