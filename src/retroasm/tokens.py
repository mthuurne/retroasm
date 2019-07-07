from __future__ import annotations

from enum import Enum, EnumMeta
from typing import (
    TYPE_CHECKING, Any, Dict, Generic, Iterator, Optional, Pattern, Tuple,
    Type, TypeVar, cast
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
    """Base class for token types."""

    def __init__(self, regex: Optional[str]):
        self.regex = regex

    @classmethod
    def compilePattern(cls) -> Pattern[str]:
        return re.compile(
            '|'.join(
                '(?P<%s>%s)' % (name, token.regex)
                for name, token in cls.__members__.items()
                if token is not None
                )
            )

    @classmethod
    def scan(cls: Type[TokenT], inp: InputLocation) -> Iterator[Token[TokenT]]:
        """Splits an input string into tokens."""
        for match in inp.findMatches(cls.pattern):
            name = match.groupName
            assert name is not None
            kind = cls[name]
            location = match.group(name)
            assert location is not None
            yield Token(kind, location.text, location)
        endToken = getattr(cls, 'end', None)
        if endToken is not None:
            yield Token(endToken, '', inp.endLocation)

TokenT = TypeVar('TokenT', bound=TokenEnum)

class Token(Generic[TokenT]):

    def __init__(self, kind: TokenT, value: str, location: InputLocation):
        self.kind = kind
        self.value = value
        self.location = location

    def check(self, kind: TokenT, value: Optional[str] = None) -> bool:
        """Check whether this token is of a particular kind
        and optionally check the value as well.
        Return True for a match, False otherwise.
        """
        if self.kind is kind:
            if value is None or self.value.casefold() == value:
                return True
        return False
