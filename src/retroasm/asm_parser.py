from __future__ import annotations

from collections.abc import Iterable, Iterator
from logging import getLogger
from pathlib import Path

from .asm_directives import DataDirective, OriginDirective, StringDirective
from .expression import Expression, IntLiteral, truncate
from .expression_nodes import IdentifierNode, NumberNode, ParseError, parseDigits
from .instrset import InstructionSet
from .linereader import DelayedError, InputLocation, LineReader
from .reference import FixedValueReference
from .symbol import SymbolValue
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


class AsmTokenizer(Tokenizer[AsmToken]):
    def eat_string(self) -> InputLocation | None:
        """
        Consume a string token and return the quoted value,
        or `None` if the current token is not a string.
        """
        token = self.eat(AsmToken.string)
        return None if token is None else token[1:-1]


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
        return NumberNode(location, parseDigits(value, 10), unlimited)
    else:
        digits = value[:-1]
        try:
            digit_width = {"b": 1, "h": 4}[value[-1].casefold()]
        except KeyError:
            raise ValueError(f'bad number suffix "{value[-1]}"') from None

    return NumberNode(
        location, parseDigits(digits, 1 << digit_width), len(digits) * digit_width
    )


def create_match_sequence(
    nodes: Iterable[IdentifierNode | NumberNode],
) -> Iterator[type[int] | str]:
    """Convert tokens to a match sequence."""
    for node in nodes:
        match node:
            case IdentifierNode(name=name):
                yield name
            case NumberNode():
                yield int
            case node:
                bad_type(node)


def parse_instruction(
    tokens: AsmTokenizer, reader: LineReader
) -> Iterator[IdentifierNode | NumberNode]:
    for kind, location in tokens:
        if kind is AsmToken.word:
            yield IdentifierNode(location, location.text)
        elif kind is AsmToken.symbol:
            # TODO: Treating symbols as identifiers is weird, but it works for now.
            yield IdentifierNode(location, location.text)
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
                yield NumberNode(location, ord(value[1]), 8)
            else:
                reader.error(
                    "multi-character string in instruction operand",
                    location=location,
                )
        elif kind is AsmToken.comment:
            pass
        else:
            assert False, kind


def build_instruction(tokens: AsmTokenizer, reader: LineReader) -> None:
    name = tokens.location
    try:
        with reader.check_errors():
            match_seq = tuple(create_match_sequence(parse_instruction(tokens, reader)))
    except DelayedError:
        return

    reader.info(
        "instruction %s", " ".join(str(elem) for elem in match_seq), location=name
    )


def parse_value(tokens: AsmTokenizer) -> Expression:
    if (location := tokens.eat(AsmToken.number)) is not None:
        number = parse_number(location)
        return IntLiteral(number.value)
    elif (location := tokens.eat(AsmToken.word)) is not None:
        # We don't know at this stage whether a symbol is a label or a constant,
        # so assume the width is unlimited.
        return SymbolValue(location.text, unlimited)
    elif tokens.end:
        raise ParseError("missing value", tokens.location)
    else:
        # TODO: Implement.
        raise ParseError.with_text(
            "unexpected token; expression parsing not implemented yet", tokens.location
        )


_data_widths = {
    "db": 8,
    "defb": 8,
    "byt": 8,
    "byte": 8,
    "dw": 16,
    "defw": 16,
    "word": 16,
    "dd": 32,
    "defd": 32,
    "dword": 32,
    "dq": 64,
    "defq": 64,
    "qword": 64,
    "quad": 64,
}


def parse_directive(
    tokens: AsmTokenizer, instr_set: InstructionSet
) -> DataDirective | OriginDirective | StringDirective:
    # TODO: It would be good to store the expression locations, so we can print
    #       a proper error report if we later discover the value is bad.
    name = tokens.eat(AsmToken.word)
    assert name is not None
    keyword = name.text.casefold()
    if keyword[0] == ".":
        keyword = keyword[1:]
    if (width := _data_widths.get(keyword)) is not None:
        data_type = IntType.u(width)
        data_class: type[DataDirective | StringDirective] = DataDirective
        data: list[FixedValueReference | bytes] = []
        while True:
            if (location := tokens.eat_string()) is not None:
                if width != 8:
                    raise ParseError(
                        f'the "{keyword}" directive does not support string literals',
                        location,
                    )
                data_class = StringDirective
                # TODO: Support other encodings?
                try:
                    data.append(location.text.encode("ascii"))
                except UnicodeError as ex:
                    raise ParseError(
                        f"string literal is not pure ASCII: {ex}", location
                    ) from None
            else:
                value = parse_value(tokens)
                # TODO: I don't like the use of truncation here, since it might silence
                #       errors.
                #       One alternative would be to add an expression node that performs
                #       a range check. Perhaps this could also store the location (see
                #       TODO at the top of this function).
                #       Another alternative would be to not use FixedValueReference for
                #       storing the values. Instead, we could store expressions (ignore
                #       width, since it's implied by the directive) or we could store
                #       ASTs (preserves more of the original code when reformatting).
                data.append(FixedValueReference(truncate(value, width), data_type))
            if tokens.end:
                break
            if tokens.eat(AsmToken.symbol, ",") is None:
                raise ParseError.with_text(
                    "unexpected token after value", tokens.location
                )
        return data_class(*data)  # type: ignore[arg-type]
    elif keyword == "org":
        addr = parse_value(tokens)
        if tokens.end:
            return OriginDirective(FixedValueReference(addr, instr_set.addrType))
        else:
            raise ParseError.with_text("unexpected token after value", tokens.location)
    else:
        raise ParseError.with_text(
            "statement is not a known instruction or directive", name
        )


def parse_label(tokens: AsmTokenizer) -> InputLocation | None:
    """Consume and return a label if one is defined at the start of this line."""
    lookahead = tokens.copy()
    label = lookahead.eat(AsmToken.word)
    if label is None:
        return None
    elif lookahead.peek(AsmToken.symbol, ":"):
        # Explicit label declaration.
        tokens.eat(AsmToken.word)
        tokens.eat(AsmToken.symbol)
        return label
    elif lookahead.peek(AsmToken.word) and tokens.value.lower() == "equ":
        # EQU directive.
        tokens.eat(AsmToken.word)
        return label
    else:
        return None


def parse_asm(reader: LineReader, instr_set: InstructionSet) -> None:
    instruction_names = instr_set.instructionNames

    for line in reader:
        tokens = AsmTokenizer.scan(line)

        # Look for a label.
        label = parse_label(tokens)
        if label is not None:
            reader.info("label: %s", label.text, location=label)

        # Look for a directive or instruction.
        if tokens.peek(AsmToken.word):
            if tokens.value.casefold() in instruction_names:
                build_instruction(tokens, reader)
            else:
                location = tokens.location
                try:
                    directive = parse_directive(tokens, instr_set)
                except ParseError as ex:
                    reader.error("%s", ex, location=ex.locations)
                else:
                    reader.info("directive: %s", directive, location=location)
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
        with reader.check_errors():
            parse_asm(reader, instr_set)
            reader.summarize()
