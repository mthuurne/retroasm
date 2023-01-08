from __future__ import annotations

from pathlib import Path

from pytest import raises

from retroasm.asm_parser import AsmTokenizer, parse_value
from retroasm.expression_nodes import ParseError
from retroasm.linereader import InputLocation


def create_location(text: str) -> InputLocation:
    return InputLocation(Path("test.asm"), 1, text, (0, len(text)))


def tokenize(text: str) -> AsmTokenizer:
    location = create_location(text)
    return AsmTokenizer.scan(location)


def test_parse_number_bad_suffix() -> None:
    """The value "123q" is reported as a bad number suffix with the "q" as its span."""
    tokens = tokenize("123q")
    with raises(ParseError, match=r'^bad number suffix "q"$') as exc_info:
        parse_value(tokens)
    (location,) = exc_info.value.locations
    assert location.text == "q", location
