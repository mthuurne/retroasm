from __future__ import annotations

from pathlib import Path

from pytest import raises

from retroasm.asm_parser import AsmTokenizer, parse_value
from retroasm.parser.expression_nodes import NumberNode, ParseError
from retroasm.parser.linereader import InputLocation
from retroasm.types import Width, unlimited


def create_location(text: str) -> InputLocation:
    return InputLocation(Path("test.asm"), 1, text, (0, len(text)))


def tokenize(text: str) -> AsmTokenizer:
    location = create_location(text)
    return AsmTokenizer.scan(location)


def check_number(text: str, value: int, width: Width) -> None:
    tokens = tokenize(text)
    number = parse_value(tokens)
    assert isinstance(number, NumberNode), number
    assert number.value == value
    assert number.width == width
    assert number.location is not None
    assert number.location.text == text


def test_parse_number_decimal() -> None:
    """The value "12345" is parsed as a decimal number."""
    check_number("12345", 12345, unlimited)


def test_parse_number_zero() -> None:
    """
    The value "0" is parsed as a decimal number.

    This is an important special case as "0" also occurs in some prefixes.
    """
    check_number("0", 0, unlimited)


def test_parse_number_bin_prefix_percent() -> None:
    """The value "%00111000" is parsed as an 8-bit binary number."""
    check_number("%00111000", 56, 8)


def test_parse_number_hex_prefix_dollar() -> None:
    """The value "$C9" is parsed as an 8-bit binary number."""
    check_number("$C9", 201, 8)


def test_parse_number_bin_prefix_0b() -> None:
    """The value "0b00111000" is parsed as an 8-bit binary number."""
    check_number("0b00111000", 56, 8)


def test_parse_number_hex_prefix_0x() -> None:
    """The value "0xC9" is parsed as an 8-bit binary number."""
    check_number("0xC9", 201, 8)


def test_parse_number_bin_suffix() -> None:
    """The value "00111000b" is parsed as an 8-bit binary number."""
    check_number("00111000b", 56, 8)


def test_parse_number_hex_suffix_low() -> None:
    """The value "6Dh" is parsed as a 8-bit hexadecimal number."""
    check_number("6Dh", 109, 8)


def test_parse_number_hex_suffix_high() -> None:
    """
    The value "0C9h" is parsed as a 12-bit hexadecimal number.

    In this case, the user may have intended to write an 8-bit number,
    but the leading zero is required to tell this apart from a label.
    """
    check_number("0C9h", 201, 12)


def test_parse_number_bad_decimal() -> None:
    """The value "123q456" is reported as a bad decimal number."""
    tokens = tokenize("123q456")
    with raises(ParseError, match=r"^bad decimal number: 123q456$") as exc_info:
        parse_value(tokens)
    (location,) = exc_info.value.locations
    assert location.text == "123q456", location


def test_parse_number_bad_suffix() -> None:
    """The value "123q" is reported as a bad number suffix with the "q" as its span."""
    tokens = tokenize("123q")
    with raises(ParseError, match=r'^bad number suffix "q"$') as exc_info:
        parse_value(tokens)
    (location,) = exc_info.value.locations
    assert location.text == "q", location
