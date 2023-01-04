from __future__ import annotations

from collections.abc import Iterable, Iterator
from logging import getLogger
from pathlib import Path
from typing import TypeAlias

from .asm_directives import (
    BinaryIncludeDirective,
    ConditionalDirective,
    ConditionalEnd,
    DataDirective,
    LabelDirective,
    OriginDirective,
    SourceIncludeDirective,
    SpaceDirective,
    StringDirective,
)
from .expression import Expression, IntLiteral, truncate
from .expression_nodes import IdentifierNode, NumberNode, ParseError, parseDigits
from .instrset import InstructionSet
from .linereader import DelayedError, InputLocation, LineReader, ProblemCounter
from .reference import FixedValueReference
from .symbol import CurrentAddress, SymbolValue
from .tokens import TokenEnum, Tokenizer
from .types import IntType, unlimited
from .utils import bad_type

logger = getLogger("parse-asm")


class AsmToken(TokenEnum):
    # In theory there could be confusion whether a '%' character is a binary prefix
    # or the modulo operator, but as 'modulo 0' is undefined and 'modulo 1' is
    # useless, we can assume that '%0' and '%1' are always the start of a number.
    number = r"\$\w+|%[01]+|\d\w*|0[xXbB]\w+"
    word = r"[\w.]+|\$"
    string = r'"[^"]*"|\'[^\']*\''
    operator = r"<<|>>|!=|<=|>=|&&|\|\||[<>=&|\^+\-*/%~!]"
    bracket = r"[\[\]()]"
    separator = r"[:,]"
    comment = r";.*$"
    other = r"."


class AsmTokenizer(Tokenizer[AsmToken]):
    @property
    def end_of_statement(self) -> bool:
        """
        Has the end of the statement been reached?

        A statement ends at end-of-line or at a comment.
        """
        return self.end or self.kind is AsmToken.comment

    def eat_remainder(self) -> None:
        """
        Consume and discard all tokens until end-of-statement has been reached.
        """
        while not self.end_of_statement:
            self._advance()

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
    while not tokens.end_of_statement:
        kind, location = next(tokens)
        match kind:
            case AsmToken.word:
                yield IdentifierNode(location, location.text)
            case AsmToken.number:
                try:
                    yield parse_number(location)
                except ValueError as ex:
                    reader.error("%s", ex, location=location)
            case AsmToken.string:
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
                    yield NumberNode(location, ord(value[1]), 8)
                else:
                    reader.error(
                        "multi-character string in instruction operand",
                        location=location,
                    )
            case _:
                # TODO: Treating symbols etc. as identifiers is weird,
                #       but it works for now.
                yield IdentifierNode(location, location.text)


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
        location = tokens.location
        tokens.eat_remainder()
        raise ParseError.with_text(
            "unexpected token; expression parsing not implemented yet", location
        )


_data_widths = {
    "db": 8,
    "defb": 8,
    "dm": 8,
    "defm": 8,
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


def parse_data_directive(
    tokens: AsmTokenizer, data_type: IntType, allow_strings: bool = False
) -> DataDirective | StringDirective:
    data_class: type[DataDirective | StringDirective] = DataDirective
    data: list[FixedValueReference | bytes] = []
    width = data_type.width
    while True:
        if (location := tokens.eat_string()) is not None:
            if not allow_strings:
                raise ParseError(
                    "string literals are not supported by this directive",
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
        if tokens.end_of_statement:
            break
        if tokens.eat(AsmToken.separator, ",") is None:
            raise ParseError.with_text("unexpected token after value", tokens.location)
    return data_class(*data)  # type: ignore[arg-type]


def parse_space_directive(tokens: AsmTokenizer) -> SpaceDirective:
    size = parse_value(tokens)
    if tokens.eat(AsmToken.separator, ",") is None:
        return SpaceDirective(size)
    else:
        value = parse_value(tokens)
        return SpaceDirective(size, value)


class DummyDirective:
    def __str__(self) -> str:
        return "(not implemented yet)"


Directive: TypeAlias = (
    DataDirective
    | StringDirective
    | SpaceDirective
    | OriginDirective
    | LabelDirective
    | BinaryIncludeDirective
    | SourceIncludeDirective
    | ConditionalDirective
    | ConditionalEnd
    | DummyDirective
)


def parse_directive(tokens: AsmTokenizer, instr_set: InstructionSet) -> Directive:
    # TODO: It would be good to store the expression locations, so we can print
    #       a proper error report if we later discover the value is bad.
    name = tokens.eat(AsmToken.word)
    assert name is not None
    keyword = name.text.casefold()
    if keyword[0] == ".":
        keyword = keyword[1:]
    if (width := _data_widths.get(keyword)) is not None:
        return parse_data_directive(tokens, IntType.u(width), width == 8)
    elif keyword == "addr":
        return parse_data_directive(tokens, instr_set.addrType)
    elif keyword in ("ds", "defs"):
        return parse_space_directive(tokens)
    elif keyword == "incbin":
        if (location := tokens.eat_string()) is not None:
            return BinaryIncludeDirective(Path(location.text))
        else:
            raise ParseError.with_text("expected file path", tokens.location)
    elif keyword == "include":
        if (location := tokens.eat_string()) is not None:
            return SourceIncludeDirective(Path(location.text))
        else:
            raise ParseError.with_text("expected file path", tokens.location)
    elif keyword == "if":
        return ConditionalDirective(parse_value(tokens), False)
    elif keyword == "elseif":
        return ConditionalDirective(parse_value(tokens), True)
    elif keyword == "else":
        return ConditionalDirective(IntLiteral(1), True)
    elif keyword == "endif":
        return ConditionalEnd()
    elif keyword == "org":
        addr = parse_value(tokens)
        if tokens.end:
            return OriginDirective(FixedValueReference(addr, instr_set.addrType))
        else:
            raise ParseError.with_text("unexpected token after value", tokens.location)
    elif keyword in ("export", "import", "global"):
        return DummyDirective()
    elif keyword in ("segment", "code", "data", "rodata", "bss"):
        return DummyDirective()
    else:
        raise ParseError.with_text(
            "statement is not a known instruction or directive", name
        )


def parse_label(tokens: AsmTokenizer) -> LabelDirective | None:
    """Consume and return a label if one is defined at the start of this line."""
    lookahead = tokens.copy()
    name = lookahead.eat(AsmToken.word)
    if name is None:
        return None
    elif lookahead.peek(AsmToken.separator, ":"):
        tokens.eat(AsmToken.word)
        tokens.eat(AsmToken.separator)
        if tokens.peek(AsmToken.word) and tokens.value.casefold() == "equ":
            # EQU directive with colon.
            tokens.eat(AsmToken.word)
            value = parse_value(tokens)
        else:
            # Label for current address.
            value = CurrentAddress()
        return LabelDirective(name.text, value)
    elif lookahead.peek(AsmToken.word) and lookahead.value.casefold() == "equ":
        # EQU directive without colon.
        tokens.eat(AsmToken.word)
        tokens.eat(AsmToken.word)
        value = parse_value(tokens)
        return LabelDirective(name.text, value)
    else:
        return None


class AsmSource:
    """
    The parsed contents of a single assembly source file.

    The contents may be incomplete if any errors were encountered during parsing.
    """

    def __init__(self) -> None:
        self._statements: list[Directive] = []
        self.problem_counter = ProblemCounter()

    def __iter__(self) -> Iterator[Directive]:
        return iter(self._statements)

    def add_directive(self, directive: Directive) -> None:
        self._statements.append(directive)

    def iter_source_includes(self) -> Iterator[Path]:
        """
        Iterate through the unresolved paths of source files included by this
        assembly file.

        As the paths are unresolved, it is possible the files do not exist or
        that the same file is referenced through different paths.
        """
        for statement in self._statements:
            if isinstance(statement, SourceIncludeDirective):
                yield statement.path


def parse_asm(reader: LineReader, instr_set: InstructionSet) -> AsmSource:
    source = AsmSource()
    instruction_names = instr_set.instructionNames

    for line in reader:
        tokens = AsmTokenizer.scan(line)

        # Look for a label.
        location = tokens.location
        try:
            label = parse_label(tokens)
        except ParseError as ex:
            reader.error("error parsing label: %s", ex, location=ex.locations)
        else:
            if label is not None:
                reader.info("label: %s", label, location=location)
                source.add_directive(label)

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
                    source.add_directive(directive)
        elif tokens.eat(AsmToken.comment) is not None:
            assert tokens.end, tokens.kind
        elif not tokens.end:
            reader.error(
                "expected directive or instruction, got %s",
                tokens.kind.name,
                location=tokens.location,
            )

    return source


def read_source(path: Path, instr_set: InstructionSet) -> AsmSource:
    """
    Parse the given source file.

    Errors will be logged and counted; no exceptions will be raised.
    Inspect the `problem_counter.num_errors` on the returned `AsmSource` object
    so know whether the source is complete.
    """
    try:
        with LineReader.open(path, logger) as reader:
            source = parse_asm(reader, instr_set)
            source.problem_counter += reader.problem_counter
            reader.summarize()
    except OSError as ex:
        logger.error("%s: Error reading source: %s", path, ex)
        source = AsmSource()
        source.problem_counter.num_errors += 1
    return source


def read_sources(
    paths: Iterable[Path], instr_set: InstructionSet
) -> dict[Path, AsmSource]:
    """
    Parse the given source files, plus any other source files included by them.

    Errors will be logged and counted; no exceptions will be raised.
    Inspect the `problem_counter.num_errors` on the returned `AsmSource` objects
    so know whether the sources are complete.

    Returns a dictionary mapping a resolved path to its parsed contents.
    """
    paths_remaining = {path.resolve() for path in paths}
    paths_done = {}
    while paths_remaining:
        source_path = paths_remaining.pop()
        source = read_source(source_path, instr_set)
        paths_done[source_path] = source
        for path in source.iter_source_includes():
            path = source_path.parent.joinpath(path).resolve()
            if path not in paths_done:
                paths_remaining.add(path)
    return paths_done
