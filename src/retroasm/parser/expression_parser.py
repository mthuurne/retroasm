from __future__ import annotations

from collections.abc import Callable, Iterable
from typing import TypeVar

from ..input import BadInput, InputLocation
from .expression_nodes import (
    AssignmentNode,
    BranchNode,
    DeclarationKind,
    DeclarationNode,
    DefinitionNode,
    EmptyNode,
    FlagTestNode,
    IdentifierNode,
    LabelNode,
    MultiMatchNode,
    NumberNode,
    Operator,
    OperatorNode,
    ParseNode,
    parse_int,
)
from .tokens import TokenEnum, Tokenizer

type DefDeclNode = DeclarationNode | DefinitionNode

_T = TypeVar("_T")


class ExprToken(TokenEnum):
    keyword = r"var|def|branch|nop"
    multimatch = r"[A-Za-z_][A-Za-z0-9_]*@"
    identifier = r"[A-Za-z_][A-Za-z0-9_]*'?"
    label = r"@[A-Za-z_][A-Za-z0-9_]*'?"
    flagtest = r"\?[A-Za-z_][A-Za-z0-9_]*'?"
    number = r"[%$0-9]\w*"
    operator = r"<<|>>|==|!=|<=|>=|[<>&|\^\*+\-~!;]"
    bracket = r"[\[\]()]"
    assignment = r":="
    definition = r"="
    separator = r"[:,]"
    other = r"."


class ExprTokenizer(Tokenizer[ExprToken]):
    pass


def _bad_token_kind(tokens: ExprTokenizer, where: str, expected: str) -> BadInput:
    if tokens.end:
        got_desc = "end of input"
    else:
        got_desc = f'{tokens.kind.name} "{tokens.value}"'
    msg = f"bad {where} expression: expected {expected}, got {got_desc}"
    return BadInput(msg, tokens.location)


def _parse_statement_top(tokens: ExprTokenizer) -> ParseNode:
    if tokens.peek(ExprToken.label):
        return _parse_label(tokens)
    if (location := tokens.eat(ExprToken.keyword, "branch")) is not None:
        cond = (
            NumberNode(1, 1, location=location)
            if tokens.peek(ExprToken.label)
            else _parse_expr_top(tokens)
        )
        target = _parse_label(tokens)
        return BranchNode(cond, target, location=location)
    if (location := tokens.eat(ExprToken.keyword, "nop")) is not None:
        return EmptyNode(location=location)
    return _parse_assign(tokens)


def _parse_label(tokens: ExprTokenizer) -> LabelNode:
    if (location := tokens.eat(ExprToken.label)) is not None:
        return LabelNode(location.text[1:], location=location)
    raise _bad_token_kind(tokens, "label", '"@<name>"')


def _parse_assign(tokens: ExprTokenizer) -> ParseNode:
    expr = _parse_expr_top(tokens)
    if (location := tokens.eat(ExprToken.assignment, ":=")) is not None:
        return AssignmentNode(expr, _parse_expr_top(tokens), location=location)
    return expr


def _parse_list(tokens: ExprTokenizer) -> Iterable[ParseNode]:
    exprs = []
    while True:
        exprs.append(_parse_expr_top(tokens))
        if tokens.eat(ExprToken.separator, ",") is None:
            return exprs


def _parse_encoding_top(tokens: ExprTokenizer) -> Iterable[ParseNode]:
    exprs = []
    while True:
        node: ParseNode
        if (location := tokens.eat(ExprToken.flagtest)) is not None:
            node = FlagTestNode(location.text[1:], location=location)
        else:
            node = _parse_expr_top(tokens)
        exprs.append(node)
        if tokens.eat(ExprToken.separator, ",") is None:
            return exprs


def _parse_context_top(tokens: ExprTokenizer) -> Iterable[DefDeclNode]:
    elems = []
    while True:
        node: DefDeclNode
        if tokens.peek(ExprToken.identifier):
            node = _parse_decl(tokens, "ctx", tokens.location)
            if (location := tokens.eat(ExprToken.definition)) is not None:
                node = DefinitionNode(node, _parse_expr_top(tokens), location=location)
        else:
            raise _bad_token_kind(tokens, "context element", "placeholder declaration")
        elems.append(node)
        if tokens.eat(ExprToken.separator, ",") is None:
            return elems


def _parse_or(tokens: ExprTokenizer) -> ParseNode:
    expr = _parse_xor(tokens)
    if (location := tokens.eat(ExprToken.operator, "|")) is not None:
        return OperatorNode(
            Operator.bitwise_or, (expr, _parse_or(tokens)), location=location
        )
    return expr


_parse_expr_top = _parse_or


def _parse_xor(tokens: ExprTokenizer) -> ParseNode:
    expr = _parse_and(tokens)
    if (location := tokens.eat(ExprToken.operator, "^")) is not None:
        return OperatorNode(
            Operator.bitwise_xor, (expr, _parse_xor(tokens)), location=location
        )
    return expr


def _parse_and(tokens: ExprTokenizer) -> ParseNode:
    expr = _parse_equal(tokens)
    if (location := tokens.eat(ExprToken.operator, "&")) is not None:
        return OperatorNode(
            Operator.bitwise_and, (expr, _parse_and(tokens)), location=location
        )
    return expr


def _parse_equal(tokens: ExprTokenizer) -> ParseNode:
    expr = _parse_compare(tokens)
    if (location := tokens.eat(ExprToken.operator, "==")) is not None:
        return OperatorNode(
            Operator.equal, (expr, _parse_equal(tokens)), location=location
        )
    if (location := tokens.eat(ExprToken.operator, "!=")) is not None:
        return OperatorNode(
            Operator.unequal, (expr, _parse_equal(tokens)), location=location
        )
    return expr


def _parse_compare(tokens: ExprTokenizer) -> ParseNode:
    expr = _parse_shift(tokens)
    if (location := tokens.eat(ExprToken.operator, "<")) is not None:
        return OperatorNode(
            Operator.lesser, (expr, _parse_compare(tokens)), location=location
        )
    if (location := tokens.eat(ExprToken.operator, "<=")) is not None:
        return OperatorNode(
            Operator.lesser_equal, (expr, _parse_compare(tokens)), location=location
        )
    if (location := tokens.eat(ExprToken.operator, ">=")) is not None:
        return OperatorNode(
            Operator.greater_equal, (expr, _parse_compare(tokens)), location=location
        )
    if (location := tokens.eat(ExprToken.operator, ">")) is not None:
        return OperatorNode(
            Operator.greater, (expr, _parse_compare(tokens)), location=location
        )
    return expr


def _parse_shift(tokens: ExprTokenizer) -> ParseNode:
    expr = _parse_add_sub(tokens)
    if (location := tokens.eat(ExprToken.operator, "<<")) is not None:
        return OperatorNode(
            Operator.shift_left, (expr, _parse_shift(tokens)), location=location
        )
    if (location := tokens.eat(ExprToken.operator, ">>")) is not None:
        return OperatorNode(
            Operator.shift_right, (expr, _parse_shift(tokens)), location=location
        )
    return expr


def _parse_add_sub(tokens: ExprTokenizer, expr: ParseNode | None = None) -> ParseNode:
    if expr is None:
        expr = _parse_mult(tokens)
    if (location := tokens.eat(ExprToken.operator, "+")) is not None:
        return _parse_add_sub(
            tokens,
            OperatorNode(Operator.add, (expr, _parse_mult(tokens)), location=location),
        )
    if (location := tokens.eat(ExprToken.operator, "-")) is not None:
        return _parse_add_sub(
            tokens,
            OperatorNode(Operator.sub, (expr, _parse_mult(tokens)), location=location),
        )
    return expr


def _parse_mult(tokens: ExprTokenizer) -> ParseNode:
    expr = _parse_concat(tokens)
    if (location := tokens.eat(ExprToken.operator, "*")) is not None:
        return OperatorNode(
            Operator.multiply, (expr, _parse_mult(tokens)), location=location
        )
    return expr


def _parse_concat(tokens: ExprTokenizer) -> ParseNode:
    expr = _parse_unary(tokens)
    if (location := tokens.eat(ExprToken.operator, ";")) is not None:
        return OperatorNode(
            Operator.concatenation, (expr, _parse_concat(tokens)), location=location
        )
    return expr


def _parse_unary(tokens: ExprTokenizer) -> ParseNode:
    if (location := tokens.eat(ExprToken.operator, "-")) is not None:
        return OperatorNode(
            Operator.complement, (_parse_unary(tokens),), location=location
        )
    if (location := tokens.eat(ExprToken.operator, "!")) is not None:
        return OperatorNode(
            Operator.negation, (_parse_unary(tokens),), location=location
        )
    if (location := tokens.eat(ExprToken.operator, "~")) is not None:
        return OperatorNode(
            Operator.bitwise_complement, (_parse_unary(tokens),), location=location
        )
    return _parse_indexed(tokens)


def _parse_indexed(tokens: ExprTokenizer) -> ParseNode:
    expr = _parse_group(tokens)
    while True:
        open_location = tokens.eat(ExprToken.bracket, "[")
        if open_location is None:
            return expr

        start: ParseNode | None
        sep_location = tokens.eat(ExprToken.separator, ":")
        if sep_location is None:
            start = _parse_expr_top(tokens)
            sep_location = tokens.eat(ExprToken.separator, ":")
        else:
            start = None

        end: ParseNode | None
        close_location = tokens.eat(ExprToken.bracket, "]")
        if sep_location is None:
            if close_location is None:
                raise _bad_token_kind(tokens, "slice/lookup", '":" or "]"')
            expr = OperatorNode(
                Operator.lookup,
                (expr, start),
                location=InputLocation.merge_span(open_location, close_location),
            )
        else:
            if close_location is None:
                end = _parse_expr_top(tokens)
                close_location = tokens.eat(ExprToken.bracket, "]")
                if close_location is None:
                    raise _bad_token_kind(tokens, "slice", '"]"')
            else:
                end = None
            expr = OperatorNode(
                Operator.slice,
                (expr, start, end),
                location=InputLocation.merge_span(open_location, close_location),
            )


def _parse_group(tokens: ExprTokenizer) -> ParseNode:
    if tokens.eat(ExprToken.bracket, "(") is not None:
        expr = _parse_expr_top(tokens)
        if tokens.eat(ExprToken.bracket, ")") is not None:
            return expr
        raise _bad_token_kind(tokens, "parenthesized", ")")

    if tokens.peek(ExprToken.keyword, "var") or tokens.peek(ExprToken.keyword, "def"):
        return _parse_definition(tokens)

    if tokens.peek(ExprToken.identifier):
        return _parse_ident(tokens)

    if tokens.peek(ExprToken.number):
        return _parse_number(tokens)

    if (multimatch_location := tokens.eat(ExprToken.multimatch)) is not None:
        value = multimatch_location.text
        assert value[-1] == "@", multimatch_location
        return MultiMatchNode(value[:-1], location=multimatch_location)

    raise _bad_token_kind(tokens, "innermost", "identifier, number or function call")


def _parse_definition(tokens: ExprTokenizer) -> DefDeclNode:
    # Keyword.
    keyword_location = tokens.eat(ExprToken.keyword)
    assert keyword_location is not None, tokens.location

    # Declaration.
    decl_node = _parse_decl(tokens, keyword_location.text, keyword_location)

    # Value.
    def_location = tokens.eat(ExprToken.definition)
    if decl_node.kind is DeclarationKind.variable:
        if def_location is not None:
            raise BadInput(
                "variables can only get values through assignment "
                '(use ":=" instead of "=")',
                def_location,
            )
        return decl_node
    else:
        if def_location is None:
            raise _bad_token_kind(tokens, f"{decl_node.kind.name} value", '"="')
        return DefinitionNode(decl_node, _parse_expr_top(tokens), location=def_location)


def _parse_regs_top(tokens: ExprTokenizer) -> Iterable[DefDeclNode]:
    defs: list[DefDeclNode] = []
    type_node: IdentifierNode | None = None
    while True:
        start_location = tokens.location

        # Parse type (optional) or name.
        # If type declaration is omitted, the previous type is re-used.
        location = tokens.eat(ExprToken.identifier)
        if location is None:
            raise _bad_token_kind(
                tokens,
                "register definition",
                "type name" if type_node is None else "type or register name",
            )

        # Merge reference indicator '&' into type.
        if tokens.location.span[0] == location.span[1]:
            amp_location = tokens.eat(ExprToken.operator, "&")
            if amp_location is not None:
                location = InputLocation.merge_span(location, amp_location)

        name_location = tokens.eat(ExprToken.identifier)
        if name_location is None:
            # No second identifier; assume omitted type declaration.
            name_location = location
            if type_node is None:
                raise _bad_token_kind(tokens, "register definition", "type name")
            if name_location.text.endswith("&"):
                raise _bad_token_kind(tokens, "register definition", "register name")
        else:
            # Second identifier; first identifier is a type declaration.
            type_node = IdentifierNode(location.text, location=location)

        name_node = IdentifierNode(name_location.text, location=name_location)

        # Complete the declaration node.
        if tokens.peek(ExprToken.definition):
            if type_node.name.endswith("&"):
                kind = DeclarationKind.reference
            else:
                kind = DeclarationKind.constant
        else:
            kind = DeclarationKind.variable
        decl_node = DeclarationNode(kind, type_node, name_node, location=start_location)

        # Finish definition.
        def_location = tokens.eat(ExprToken.definition)
        defs.append(
            decl_node
            if def_location is None
            else DefinitionNode(
                decl_node, _parse_expr_top(tokens), location=def_location
            )
        )

        if tokens.eat(ExprToken.separator, ",") is None:
            return defs


def _parse_decl(
    tokens: ExprTokenizer, keyword: str, start_location: InputLocation
) -> DeclarationNode:
    kind = {
        "ctx": DeclarationKind.constant,
        "def": DeclarationKind.constant,
        "var": DeclarationKind.variable,
    }[keyword]

    # Type.
    type_location = tokens.eat(ExprToken.identifier)
    if type_location is None:
        kind_desc = {
            "ctx": "context",
            "def": "constant/reference",
            "var": "variable",
        }[keyword]
        raise _bad_token_kind(tokens, f"{kind_desc} definition", "type name")

    # Merge reference indicator '&' into type.
    if tokens.location.span[0] == type_location.span[1]:
        amp_location = tokens.eat(ExprToken.operator, "&")
        if amp_location is not None:
            type_location = InputLocation.merge_span(type_location, amp_location)
            if kind is DeclarationKind.variable:
                raise BadInput(
                    'references can only be defined using the "def" keyword',
                    InputLocation.merge_span(start_location, amp_location),
                )
            kind = DeclarationKind.reference

    type_node = IdentifierNode(type_location.text, location=type_location)

    # Name.
    name_location = tokens.eat(ExprToken.identifier)
    if name_location is None:
        raise _bad_token_kind(tokens, f"{kind.name} definition", f"{kind.name} name")
    name_node = IdentifierNode(name_location.text, location=name_location)

    return DeclarationNode(kind, type_node, name_node, location=start_location)


def _parse_ident(tokens: ExprTokenizer) -> IdentifierNode | OperatorNode:
    location = tokens.eat(ExprToken.identifier)
    assert location is not None, tokens.location

    identifier = IdentifierNode(location.text, location=location)
    if tokens.peek(ExprToken.bracket, "("):
        return _parse_function_call(tokens, identifier)
    else:
        return identifier


def _parse_function_call(tokens: ExprTokenizer, name: IdentifierNode) -> OperatorNode:
    open_location = tokens.eat(ExprToken.bracket, "(")
    assert open_location is not None, tokens.location

    exprs: list[ParseNode] = [name]
    close_location = tokens.eat(ExprToken.bracket, ")")
    while close_location is None:
        exprs.append(_parse_expr_top(tokens))
        close_location = tokens.eat(ExprToken.bracket, ")")
        if close_location is None:
            if tokens.eat(ExprToken.separator, ",") is None:
                raise _bad_token_kind(tokens, "function call arguments", '"," or ")"')

    location = InputLocation.merge_span(open_location, close_location)
    return OperatorNode(Operator.call, tuple(exprs), location=location)


def _parse_number(tokens: ExprTokenizer) -> NumberNode:
    location = tokens.eat(ExprToken.number)
    assert location is not None, tokens.location
    try:
        value, width = parse_int(location.text)
    except ValueError as ex:
        raise BadInput(f"{ex}", location) from ex
    else:
        return NumberNode(value, width, location=location)


def _parse(location: InputLocation, parse_top: Callable[[ExprTokenizer], _T]) -> _T:
    tokens = ExprTokenizer.scan(location)

    parsed = parse_top(tokens)

    if tokens.peek(ExprToken.other):
        raise BadInput(
            f'unexpected character "{tokens.value}" in expression', tokens.location
        )
    if not tokens.end:
        raise BadInput(
            f'found {tokens.kind.name} "{tokens.value}" in an unexpected place',
            tokens.location,
        )

    return parsed


def parse_expr(location: InputLocation) -> ParseNode:
    return _parse(location, _parse_expr_top)


def parse_expr_list(location: InputLocation) -> Iterable[ParseNode]:
    return _parse(location, _parse_list)


def parse_regs(location: InputLocation) -> Iterable[DefDeclNode]:
    return _parse(location, _parse_regs_top)


def parse_encoding(location: InputLocation) -> Iterable[ParseNode]:
    return _parse(location, _parse_encoding_top)


def parse_context(location: InputLocation) -> Iterable[DefDeclNode]:
    return _parse(location, _parse_context_top)


def parse_statement(location: InputLocation) -> ParseNode:
    return _parse(location, _parse_statement_top)
