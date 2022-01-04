from __future__ import annotations

from logging import getLogger
from pathlib import Path
from typing import Iterable, Iterator

from .asm_directives import DataDirective, OriginDirective
from .expression import Expression, IntLiteral
from .expression_nodes import IdentifierNode, NumberNode, ParseError, parseDigits
from .instrset import InstructionSet
from .linereader import DelayedError, InputLocation, LineReader
from .reference import FixedValueReference
from .tokens import TokenEnum, Tokenizer
from .types import IntType, unlimited
from .utils import bad_type

logger = getLogger("parse-asm")


class AsmToken(TokenEnum):
    number = r"\$\w+|%\w+|\d\w*|0[xXbB]\w+"
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
    name: InputLocation, nodes: Iterable[IdentifierNode | NumberNode]
) -> Iterator[type[int] | str]:
    """Convert tokens to a match sequence."""
    yield name.text
    for node in nodes:
        if isinstance(node, IdentifierNode):
            yield node.name
        elif isinstance(node, NumberNode):
            yield int
        else:
            bad_type(node)


def parse_instruction(
    tokens: Tokenizer[AsmToken], reader: LineReader
) -> Iterator[IdentifierNode | NumberNode]:
    for kind, location in tokens:
        if kind is AsmToken.word:
            yield IdentifierNode(location.text, location)
        elif kind is AsmToken.symbol:
            # TODO: Treating symbols as identifiers is weird, but it works for now.
            yield IdentifierNode(location.text, location)
        elif kind is AsmToken.number:
            try:
                yield parse_number(location)
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
                reader.error("empty string in instruction operand", location=location)
            elif len(value) == 3:
                yield NumberNode(ord(value[1]), 8, location)
            else:
                reader.error(
                    "multi-character string in instruction operand",
                    location=location,
                )
        elif kind is AsmToken.comment:
            pass
        else:
            assert False, kind


def build_instruction(
    name: InputLocation, tokens: Tokenizer[AsmToken], reader: LineReader
) -> None:
    try:
        with reader.checkErrors():
            match_seq = tuple(
                create_match_sequence(name, parse_instruction(tokens, reader))
            )
    except DelayedError:
        return

    reader.info(
        "instruction %s", " ".join(str(elem) for elem in match_seq), location=name
    )


def parse_value(tokens: Tokenizer[AsmToken]) -> Expression:
    if (location := tokens.eat(AsmToken.number)) is not None:
        number = parse_number(location)
        return IntLiteral(number.value)
    elif tokens.end:
        raise ParseError("missing value", tokens.location)
    else:
        # TODO: Implement.
        raise ParseError.withText(
            "unexpected token; expression parsing not implemented yet", tokens.location
        )


_data_widths = {
    "db": 8,
    "defb": 8,
    "dw": 16,
    "defw": 16,
    "dd": 32,
    "defd": 32,
    "dq": 64,
    "defq": 64,
}


def parse_directive(
    name: InputLocation, tokens: Tokenizer[AsmToken], instr_set: InstructionSet
) -> DataDirective | OriginDirective:
    # TODO: It would be good to store the expression locations, so we can print
    #       a proper error report if we later discover the value is bad.
    keyword = name.text.casefold()
    if (width := _data_widths.get(keyword)) is not None:
        data_type = IntType.u(width)
        data = []
        while True:
            value = parse_value(tokens)
            data.append(FixedValueReference(value, data_type))
            if tokens.end:
                break
            if tokens.eat(AsmToken.symbol, ",") is None:
                raise ParseError.withText(
                    "unexpected token after value", tokens.location
                )
        return DataDirective(*data)
    elif keyword == "org":
        addr = parse_value(tokens)
        if tokens.end:
            return OriginDirective(FixedValueReference(addr, instr_set.addrType))
        else:
            raise ParseError.withText("unexpected token after value", tokens.location)
    else:
        raise ParseError.withText(
            "statement is not a known instruction or directive", name
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
                build_instruction(first_word, tokens, reader)
            else:
                try:
                    directive = parse_directive(first_word, tokens, instr_set)
                except ParseError as ex:
                    reader.error("%s", ex, location=ex.locations)
                else:
                    reader.info("directive: %s", directive, location=first_word)
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
