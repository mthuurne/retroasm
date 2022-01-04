from __future__ import annotations

from logging import getLogger
from pathlib import Path
from typing import Iterable, Iterator

from .expression_nodes import NumberNode, parseDigits
from .instrset import InstructionSet
from .linereader import DelayedError, InputLocation, LineReader
from .tokens import TokenEnum, Tokenizer
from .types import unlimited

logger = getLogger("parse-asm")


class AsmToken(TokenEnum):
    number = r"\$\w+|%\w+|\d\w*"
    word = r"[\w.]+"
    string = r'"[^"]*"|\'[^\']*\''
    comment = r";.*$"
    symbol = r"."


Token = tuple[AsmToken, InputLocation]


def parse_number(location: InputLocation) -> NumberNode:
    """
    Parse a numeric literal in one of several formats.
    Raise `ValueError` if the location does not contain a valid number.
    """

    value = location.text
    if value[0] == "$":
        digits = value[1:]
        digit_width = 4
    elif value[0] == "%":
        digits = value[1:]
        digit_width = 1
    elif value[0] == "0" and len(value) >= 2 and value[1] in "xXbB":
        digits = value[2:]
        digit_width = 4 if value[1] in "xX" else 1
    elif value[-1].isdigit():
        # Decimal numbers have no integer per-digit width.
        return NumberNode(parseDigits(value, 10), unlimited, location)
    else:
        digits = value[:-1]
        try:
            digit_width = {"b": 1, "h": 4}[value[-1].casefold()]
        except KeyError:
            raise ValueError(f'bad number suffix "{value[-1]}"') from None

    return NumberNode(
        parseDigits(digits, 1 << digit_width), len(digits) * digit_width, location
    )


def create_match_sequence(
    name: InputLocation, tokens: Iterable[Token]
) -> Iterator[type[int] | int | str]:
    """Convert tokens to a match sequence."""
    yield name.text
    for kind, location in tokens:
        if kind is AsmToken.number or kind is AsmToken.string:
            yield int
        elif kind is AsmToken.word or kind is AsmToken.symbol:
            yield location.text
        else:
            assert kind is AsmToken.comment, kind


def parse_instruction(
    name: InputLocation, tokens: Tokenizer[AsmToken], reader: LineReader
) -> None:
    token_list = list(tokens)

    try:
        with reader.checkErrors():
            for kind, location in token_list:
                # TODO: Use the parsed number for something.
                # pylint: disable=unused-variable
                if kind is AsmToken.number:
                    # Convert to int.
                    try:
                        number = parse_number(location)
                    except ValueError as ex:
                        reader.error("%s", ex, location=location)
                elif kind is AsmToken.string:
                    # Arbitrary strings are not allowed as instruction
                    # operands, but single characters should be replaced
                    # by their character numbers.
                    value = location.text
                    assert len(value) >= 2, value
                    assert value[0] == value[-1], value
                    if len(value) == 2:
                        reader.error(
                            "empty string in instruction operand", location=location
                        )
                    elif len(value) == 3:
                        number = NumberNode(ord(value[1]), 8, location)
                    else:
                        reader.error(
                            "multi-character string in instruction operand",
                            location=location,
                        )
    except DelayedError:
        return

    match_seq = tuple(create_match_sequence(name, token_list))

    reader.info(
        "instruction %s", " ".join(str(elem) for elem in match_seq), location=name
    )


def parse_directive(
    name: InputLocation, tokens: Tokenizer[AsmToken], reader: LineReader
) -> None:
    directive = [name]
    for kind, location in tokens:
        if kind is not AsmToken.comment:
            directive.append(location)
    reader.info(
        "directive: %s",
        " ".join(location.text for location in directive),
        location=name,
    )


def parse_asm(reader: LineReader, instr_set: InstructionSet) -> None:
    instr_set.dumpMnemonicTree()
    instruction_names = instr_set.instructionNames

    for line in reader:
        tokens = AsmToken.scan(line)

        # Look for a label.
        label = None
        first_word = tokens.eat(AsmToken.word)
        if first_word is not None and (
            # explicit label declaration
            tokens.eat(AsmToken.symbol, ":") is not None
            # EQU directive
            or (tokens.peek(AsmToken.word) and tokens.value.casefold() == "equ")
            # local label
            or first_word.text.startswith(".")
        ):
            label = first_word.text
            first_word = tokens.eat(AsmToken.word)
        if label is not None:
            reader.info("label: %s", label)

        # Look for a directive or instruction.
        if first_word is not None:
            if first_word.text.casefold() in instruction_names:
                parse_instruction(first_word, tokens, reader)
            else:
                parse_directive(first_word, tokens, reader)
        elif tokens.eat(AsmToken.comment) is not None:
            assert tokens.end, tokens.kind
        elif not tokens.end:
            reader.error(
                "expected directive or instruction, got %s",
                tokens.kind.name,
                location=tokens.location,
            )


def read_source(path: Path, instr_set: InstructionSet) -> None:
    with LineReader.open(path, logger) as reader:
        with reader.checkErrors():
            parse_asm(reader, instr_set)
            reader.summarize()
