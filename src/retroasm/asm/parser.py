from __future__ import annotations

from collections.abc import Iterable, Iterator
from logging import getLogger
from pathlib import Path
from typing import TypeAlias

from ..instrset import InstructionSet
from ..parser.expression_nodes import (
    IdentifierNode,
    NumberNode,
    Operator,
    OperatorNode,
    ParseError,
    ParseNode,
    parse_digits,
)
from ..parser.linereader import DelayedError, InputLocation, LineReader, ProblemCounter
from ..parser.tokens import TokenEnum, Tokenizer
from ..types import IntType, Width, unlimited
from .directives import (
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

logger = getLogger("parse-asm")


class AsmToken(TokenEnum):
    # In theory there could be confusion whether a '%' character is a binary prefix
    # or the modulo operator, but as 'modulo 0' is undefined and 'modulo 1' is
    # useless, we can assume that '%0' and '%1' are always the start of a number.
    number = r"\$\w+|%[01]+|\d\w*|0[xXbB]\w+"
    word = r"[\w.]+'?|\$"
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


def create_match_sequence(nodes: Iterable[ParseNode]) -> Iterator[type[int] | str]:
    """Convert tokens to a match sequence."""
    for node in nodes:
        match node:
            case IdentifierNode(name=name):
                yield name
            case NumberNode():
                yield int
            case OperatorNode():
                yield int
            case _:
                assert False, node


def parse_instruction(tokens: AsmTokenizer, reader: LineReader) -> Iterator[ParseNode]:
    # TODO: Treating keywords and separators as identifiers is weird,
    #       but it works for now.
    instr_name = tokens.eat(AsmToken.word)
    assert instr_name is not None
    yield IdentifierNode(instr_name.text, location=instr_name)

    while not tokens.end_of_statement:
        try:
            yield parse_value(tokens)
        except ParseError as ex:
            reader.error("error parsing operand: %s", ex, location=ex.locations)
            tokens.eat_remainder()
            return
        if (separator := tokens.eat(AsmToken.separator)) is not None:
            yield IdentifierNode(separator.text, location=separator)


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


def parse_value(tokens: AsmTokenizer) -> ParseNode:
    def bad_token_kind(where: str, expected: str) -> ParseError:
        if tokens.end_of_statement:
            got_desc = "end of statement"
        else:
            got_desc = f'{tokens.kind.name} "{tokens.value}"'
        msg = f"bad {where} expression: expected {expected}, got {got_desc}"
        return ParseError(msg, tokens.location)

    def parse_or() -> ParseNode:
        expr = parse_xor()
        if (location := tokens.eat(AsmToken.operator, "|")) is not None:
            return OperatorNode(
                Operator.bitwise_or, (expr, parse_or()), location=location
            )
        return expr

    def parse_xor() -> ParseNode:
        expr = parse_and()
        if (location := tokens.eat(AsmToken.operator, "^")) is not None:
            return OperatorNode(
                Operator.bitwise_xor, (expr, parse_xor()), location=location
            )
        return expr

    def parse_and() -> ParseNode:
        expr = parse_equal()
        if (location := tokens.eat(AsmToken.operator, "&")) is not None:
            return OperatorNode(
                Operator.bitwise_and, (expr, parse_and()), location=location
            )
        return expr

    def parse_equal() -> ParseNode:
        expr = parse_compare()
        if (location := tokens.eat(AsmToken.operator, "=")) is not None:
            return OperatorNode(
                Operator.equal, (expr, parse_equal()), location=location
            )
        if (location := tokens.eat(AsmToken.operator, "!=")) is not None:
            return OperatorNode(
                Operator.unequal, (expr, parse_equal()), location=location
            )
        return expr

    def parse_compare() -> ParseNode:
        expr = parse_shift()
        if (location := tokens.eat(AsmToken.operator, "<")) is not None:
            return OperatorNode(
                Operator.lesser, (expr, parse_compare()), location=location
            )
        if (location := tokens.eat(AsmToken.operator, "<=")) is not None:
            return OperatorNode(
                Operator.lesser_equal, (expr, parse_compare()), location=location
            )
        if (location := tokens.eat(AsmToken.operator, ">=")) is not None:
            return OperatorNode(
                Operator.greater_equal, (expr, parse_compare()), location=location
            )
        if (location := tokens.eat(AsmToken.operator, ">")) is not None:
            return OperatorNode(
                Operator.greater, (expr, parse_compare()), location=location
            )
        return expr

    def parse_shift() -> ParseNode:
        expr = parse_add_sub()
        if (location := tokens.eat(AsmToken.operator, "<<")) is not None:
            return OperatorNode(
                Operator.shift_left, (expr, parse_shift()), location=location
            )
        if (location := tokens.eat(AsmToken.operator, ">>")) is not None:
            return OperatorNode(
                Operator.shift_right, (expr, parse_shift()), location=location
            )
        return expr

    def parse_add_sub(expr: ParseNode | None = None) -> ParseNode:
        if expr is None:
            expr = parse_mul_div()
        if (location := tokens.eat(AsmToken.operator, "+")) is not None:
            return parse_add_sub(
                OperatorNode(Operator.add, (expr, parse_mul_div()), location=location)
            )
        if (location := tokens.eat(AsmToken.operator, "-")) is not None:
            return parse_add_sub(
                OperatorNode(Operator.sub, (expr, parse_mul_div()), location=location)
            )
        return expr

    def parse_mul_div(expr: ParseNode | None = None) -> ParseNode:
        if expr is None:
            expr = parse_unary()
        if (location := tokens.eat(AsmToken.operator, "*")) is not None:
            return parse_mul_div(
                OperatorNode(
                    Operator.multiply, (expr, parse_unary()), location=location
                )
            )
        if (location := tokens.eat(AsmToken.operator, "/")) is not None:
            return parse_mul_div(
                OperatorNode(Operator.divide, (expr, parse_unary()), location=location)
            )
        if (location := tokens.eat(AsmToken.operator, "%")) is not None:
            return parse_mul_div(
                OperatorNode(Operator.modulo, (expr, parse_unary()), location=location)
            )
        return expr

    def parse_unary() -> ParseNode:
        if (location := tokens.eat(AsmToken.operator, "-")) is not None:
            return OperatorNode(
                Operator.complement, (parse_unary(),), location=location
            )
        if (location := tokens.eat(AsmToken.operator, "!")) is not None:
            return OperatorNode(Operator.negation, (parse_unary(),), location=location)
        if (location := tokens.eat(AsmToken.operator, "~")) is not None:
            return OperatorNode(
                Operator.bitwise_complement, (parse_unary(),), location=location
            )
        return parse_indexed()

    def parse_indexed() -> ParseNode:
        expr = parse_group()
        while True:
            open_location = tokens.eat(AsmToken.bracket, "[")
            if open_location is None:
                return expr

            start: ParseNode | None
            sep_location = tokens.eat(AsmToken.separator, ":")
            if sep_location is None:
                start = parse_expr_top()
                sep_location = tokens.eat(AsmToken.separator, ":")
            else:
                start = None

            end: ParseNode | None
            close_location = tokens.eat(AsmToken.bracket, "]")
            if sep_location is None:
                if close_location is None:
                    raise bad_token_kind("slice/lookup", '":" or "]"')
                expr = OperatorNode(
                    Operator.lookup,
                    (expr, start),
                    location=InputLocation.merge_span(open_location, close_location),
                )
            else:
                if close_location is None:
                    end = parse_expr_top()
                    close_location = tokens.eat(AsmToken.bracket, "]")
                    if close_location is None:
                        raise bad_token_kind("slice", '"]"')
                else:
                    end = None
                expr = OperatorNode(
                    Operator.slice,
                    (expr, start, end),
                    location=InputLocation.merge_span(open_location, close_location),
                )

    def parse_group() -> ParseNode:
        if tokens.eat(AsmToken.bracket, "(") is not None:
            expr = parse_expr_top()
            if tokens.eat(AsmToken.bracket, ")") is not None:
                return expr
            raise bad_token_kind("parenthesized", ")")

        if tokens.peek(AsmToken.word):
            return parse_ident()

        if tokens.peek(AsmToken.number):
            return parse_number()

        if (location := tokens.eat_string()) is not None:
            # Arbitrary strings are not allowed as instruction
            # operands, but single characters should be replaced
            # by their character numbers.
            try:
                value = ord(location.text)
            except TypeError:
                desc = "empty" if len(location) == 0 else "multi-character"
                raise ParseError(f"{desc} string in expression", location) from None
            else:
                return NumberNode(value, 8, location=location)

        raise bad_token_kind("innermost", "identifier or number")

    def parse_ident() -> IdentifierNode | OperatorNode:
        location = tokens.eat(AsmToken.word)
        assert location is not None, tokens.location

        return IdentifierNode(location.text, location=location)

    def parse_number() -> NumberNode:
        location = tokens.eat(AsmToken.number)
        assert location is not None, tokens.location

        value = location.text
        width: Width
        if value[0] == "$":
            digits = value[1:]
            radix = 16
            width = 4 * len(digits)
        elif value[0] == "%":
            digits = value[1:]
            radix = 2
            width = len(digits)
        elif value[0] == "0" and len(value) >= 2 and value[1] in "xXbB":
            digits = value[2:]
            digit_width = 4 if value[1] in "xX" else 1
            radix = 1 << digit_width
            width = digit_width * len(digits)
        elif value[-1].isdigit():
            digits = value
            radix = 10
            # Decimal numbers have no integer per-digit width.
            width = unlimited
        else:
            digits = value[:-1]
            try:
                digit_width = {"b": 1, "h": 4}[value[-1].casefold()]
            except KeyError:
                raise ParseError(
                    f'bad number suffix "{value[-1]}"', location[-1:]
                ) from None
            radix = 1 << digit_width
            width = digit_width * len(digits)

        try:
            num_val = parse_digits(digits, radix)
        except ValueError as ex:
            # TODO: Have the span point to the first offending character.
            raise ParseError(f"{ex}", location) from None
        else:
            return NumberNode(num_val, width, location=location)

    parse_expr_top = parse_or

    return parse_expr_top()


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
    data: list[ParseNode | bytes] = []
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
            data.append(parse_value(tokens))
        if tokens.end_of_statement:
            break
        if tokens.eat(AsmToken.separator, ",") is None:
            raise ParseError.with_text("unexpected token after value", tokens.location)
    return data_class(width, *data)  # type: ignore[arg-type]


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
        return parse_data_directive(tokens, instr_set.addr_type)
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
        return ConditionalDirective(None, True)
    elif keyword == "endif":
        return ConditionalEnd()
    elif keyword == "org":
        return OriginDirective(parse_value(tokens))
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
            return LabelDirective(name.text, value)
        else:
            # Label for current address.
            return LabelDirective(name.text, None)
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
    instruction_names = instr_set.instruction_names

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
        elif not tokens.end_of_statement:
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
