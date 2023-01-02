from __future__ import annotations

from collections.abc import Iterable
from enum import Enum, auto
from typing import Any, TypeAlias, cast

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
    ParseError,
    ParseNode,
    parseInt,
)
from .linereader import InputLocation, merge_span
from .tokens import TokenEnum, Tokenizer

DefDeclNode: TypeAlias = DeclarationNode | DefinitionNode
ContextNode: TypeAlias = DeclarationNode | DefinitionNode | FlagTestNode


class ExprToken(TokenEnum):
    keyword = r"var|def|branch|nop"
    multimatch = r"[A-Za-z_][A-Za-z0-9_]*@"
    identifier = r"[A-Za-z_][A-Za-z0-9_]*'?"
    label = r"@[A-Za-z_][A-Za-z0-9_]*'?"
    flagtest = r"\?[A-Za-z_][A-Za-z0-9_]*'?"
    number = r"[%$0-9]\w*"
    operator = r"<<|>>|==|!=|<=|>=|[<>&|\^+\-~!;]"
    bracket = r"[\[\]()]"
    assignment = r":="
    definition = r"="
    separator = r"[:,]"
    other = r"."


class ExprTokenizer(Tokenizer[ExprToken]):
    pass


class _ParseMode(Enum):
    single = auto()
    multi = auto()
    statement = auto()
    registers = auto()
    context = auto()


def _parse(location: InputLocation, mode: _ParseMode) -> Any:
    tokens = ExprTokenizer.scan(location)

    def bad_token_kind(where: str, expected: str) -> ParseError:
        if tokens.end:
            gotDesc = "end of input"
        else:
            gotDesc = f'{tokens.kind.name} "{tokens.value}"'
        msg = f"bad {where} expression: expected {expected}, got {gotDesc}"
        return ParseError(msg, tokens.location)

    def parse_statement_top() -> ParseNode:
        if tokens.peek(ExprToken.label):
            return parse_label()
        if (location := tokens.eat(ExprToken.keyword, "branch")) is not None:
            cond = (
                NumberNode(location, 1, 1)
                if tokens.peek(ExprToken.label)
                else parse_expr_top()
            )
            target = parse_label()
            return BranchNode(location, cond, target)
        if (location := tokens.eat(ExprToken.keyword, "nop")) is not None:
            return EmptyNode(location)
        return parse_assign()

    def parse_label() -> LabelNode:
        if (location := tokens.eat(ExprToken.label)) is not None:
            return LabelNode(location, location.text[1:])
        raise bad_token_kind("label", '"@<name>"')

    def parse_assign() -> ParseNode:
        expr = parse_expr_top()
        if (location := tokens.eat(ExprToken.assignment, ":=")) is not None:
            return AssignmentNode(location, expr, parse_expr_top())
        return expr

    def parse_list() -> Iterable[ParseNode]:
        exprs = []
        while True:
            exprs.append(parse_expr_top())
            if tokens.eat(ExprToken.separator, ",") is None:
                return exprs

    def parse_context_top() -> Iterable[ContextNode]:
        elems = []
        while True:
            node: ContextNode
            if tokens.peek(ExprToken.identifier):
                node = parse_decl("ctx", tokens.location)
                if (location := tokens.eat(ExprToken.definition)) is not None:
                    node = DefinitionNode(location, node, parse_expr_top())
            elif (location := tokens.eat(ExprToken.flagtest)) is not None:
                node = FlagTestNode(location, location.text[1:])
            else:
                raise bad_token_kind(
                    "context element", "placeholder declaration or flag test"
                )
            elems.append(node)
            if tokens.eat(ExprToken.separator, ",") is None:
                return elems

    def parse_or() -> ParseNode:
        expr = parse_xor()
        if (location := tokens.eat(ExprToken.operator, "|")) is not None:
            return OperatorNode(location, Operator.bitwise_or, (expr, parse_or()))
        return expr

    def parse_xor() -> ParseNode:
        expr = parse_and()
        if (location := tokens.eat(ExprToken.operator, "^")) is not None:
            return OperatorNode(location, Operator.bitwise_xor, (expr, parse_xor()))
        return expr

    def parse_and() -> ParseNode:
        expr = parse_equal()
        if (location := tokens.eat(ExprToken.operator, "&")) is not None:
            return OperatorNode(location, Operator.bitwise_and, (expr, parse_and()))
        return expr

    def parse_equal() -> ParseNode:
        expr = parse_compare()
        if (location := tokens.eat(ExprToken.operator, "==")) is not None:
            return OperatorNode(location, Operator.equal, (expr, parse_equal()))
        if (location := tokens.eat(ExprToken.operator, "!=")) is not None:
            return OperatorNode(location, Operator.unequal, (expr, parse_equal()))
        return expr

    def parse_compare() -> ParseNode:
        expr = parse_shift()
        if (location := tokens.eat(ExprToken.operator, "<")) is not None:
            return OperatorNode(location, Operator.lesser, (expr, parse_compare()))
        if (location := tokens.eat(ExprToken.operator, "<=")) is not None:
            return OperatorNode(
                location, Operator.lesser_equal, (expr, parse_compare())
            )
        if (location := tokens.eat(ExprToken.operator, ">=")) is not None:
            return OperatorNode(
                location, Operator.greater_equal, (expr, parse_compare())
            )
        if (location := tokens.eat(ExprToken.operator, ">")) is not None:
            return OperatorNode(location, Operator.greater, (expr, parse_compare()))
        return expr

    def parse_shift() -> ParseNode:
        expr = parse_add_sub()
        if (location := tokens.eat(ExprToken.operator, "<<")) is not None:
            return OperatorNode(location, Operator.shift_left, (expr, parse_shift()))
        if (location := tokens.eat(ExprToken.operator, ">>")) is not None:
            return OperatorNode(location, Operator.shift_right, (expr, parse_shift()))
        return expr

    def parse_add_sub(expr: ParseNode | None = None) -> ParseNode:
        if expr is None:
            expr = parse_concat()
        if (location := tokens.eat(ExprToken.operator, "+")) is not None:
            return parse_add_sub(
                OperatorNode(location, Operator.add, (expr, parse_concat()))
            )
        if (location := tokens.eat(ExprToken.operator, "-")) is not None:
            return parse_add_sub(
                OperatorNode(location, Operator.sub, (expr, parse_concat()))
            )
        return expr

    def parse_concat() -> ParseNode:
        expr = parse_unary()
        if (location := tokens.eat(ExprToken.operator, ";")) is not None:
            return OperatorNode(
                location, Operator.concatenation, (expr, parse_concat())
            )
        return expr

    def parse_unary() -> ParseNode:
        if (location := tokens.eat(ExprToken.operator, "-")) is not None:
            return OperatorNode(location, Operator.complement, (parse_unary(),))
        if (location := tokens.eat(ExprToken.operator, "!")) is not None:
            return OperatorNode(location, Operator.negation, (parse_unary(),))
        if (location := tokens.eat(ExprToken.operator, "~")) is not None:
            return OperatorNode(location, Operator.bitwise_complement, (parse_unary(),))
        return parse_indexed()

    def parse_indexed() -> ParseNode:
        expr = parse_group()
        while True:
            openLocation = tokens.eat(ExprToken.bracket, "[")
            if openLocation is None:
                return expr

            start: ParseNode | None
            sepLocation = tokens.eat(ExprToken.separator, ":")
            if sepLocation is None:
                start = parse_expr_top()
                sepLocation = tokens.eat(ExprToken.separator, ":")
            else:
                start = None

            end: ParseNode | None
            closeLocation = tokens.eat(ExprToken.bracket, "]")
            if sepLocation is None:
                if closeLocation is None:
                    raise bad_token_kind("slice/lookup", '":" or "]"')
                expr = OperatorNode(
                    merge_span(openLocation, closeLocation),
                    Operator.lookup,
                    (expr, start),
                )
            else:
                if closeLocation is None:
                    end = parse_expr_top()
                    closeLocation = tokens.eat(ExprToken.bracket, "]")
                    if closeLocation is None:
                        raise bad_token_kind("slice", '"]"')
                else:
                    end = None
                expr = OperatorNode(
                    merge_span(openLocation, closeLocation),
                    Operator.slice,
                    (expr, start, end),
                )

    def parse_group() -> ParseNode:
        if tokens.eat(ExprToken.bracket, "(") is not None:
            expr = parse_expr_top()
            if tokens.eat(ExprToken.bracket, ")") is not None:
                return expr
            raise bad_token_kind("parenthesized", ")")

        if tokens.peek(ExprToken.keyword, "var") or tokens.peek(
            ExprToken.keyword, "def"
        ):
            return parse_definition()

        if tokens.peek(ExprToken.identifier):
            ident = parse_ident()
            if isinstance(ident, IdentifierNode) and ident.name == "ret":
                if (defLocation := tokens.eat(ExprToken.definition)) is not None:
                    declNode = DeclarationNode(
                        ident.location, DeclarationKind.reference, None, ident
                    )
                    return DefinitionNode(defLocation, declNode, parse_expr_top())
            return ident

        if tokens.peek(ExprToken.number):
            return parse_number()

        if (multimatchLocation := tokens.eat(ExprToken.multimatch)) is not None:
            value = multimatchLocation.text
            assert value[-1] == "@", multimatchLocation
            return MultiMatchNode(multimatchLocation, value[:-1])

        raise bad_token_kind("innermost", "identifier, number or function call")

    def parse_definition() -> DefDeclNode:
        # Keyword.
        keywordLocation = tokens.eat(ExprToken.keyword)
        assert keywordLocation is not None, tokens.location

        # Declaration.
        declNode = parse_decl(keywordLocation.text, keywordLocation)

        # Value.
        defLocation = tokens.eat(ExprToken.definition)
        if declNode.kind is DeclarationKind.variable:
            if defLocation is not None:
                raise ParseError(
                    "variables can only get values through assignment "
                    '(use ":=" instead of "=")',
                    defLocation,
                )
            return declNode
        else:
            if defLocation is None:
                raise bad_token_kind(f"{declNode.kind.name} value", '"="')
            return DefinitionNode(defLocation, declNode, parse_expr_top())

    def parse_regs_top() -> Iterable[DefDeclNode]:
        defs: list[DefDeclNode] = []
        typeNode: IdentifierNode | None = None
        while True:
            startLocation = tokens.location

            # Parse type (optional) or name.
            # If type declaration is omitted, the previous type is re-used.
            location = tokens.eat(ExprToken.identifier)
            if location is None:
                raise bad_token_kind(
                    "register definition",
                    "type name" if typeNode is None else "type or register name",
                )

            # Merge reference indicator '&' into type.
            if tokens.location.span[0] == location.span[1]:
                ampLocation = tokens.eat(ExprToken.operator, "&")
                if ampLocation is not None:
                    location = merge_span(location, ampLocation)

            nameLocation = tokens.eat(ExprToken.identifier)
            if nameLocation is None:
                # No second identifier; assume omitted type declaration.
                nameLocation = location
                if typeNode is None:
                    raise bad_token_kind("register definition", "type name")
                if nameLocation.text.endswith("&"):
                    raise bad_token_kind("register definition", "register name")
            else:
                # Second identifier; first identifier is a type declaration.
                typeNode = IdentifierNode(location, location.text)

            nameNode = IdentifierNode(nameLocation, nameLocation.text)

            # Complete the declaration node.
            if tokens.peek(ExprToken.definition):
                if typeNode.name.endswith("&"):
                    kind = DeclarationKind.reference
                else:
                    kind = DeclarationKind.constant
            else:
                kind = DeclarationKind.variable
            declNode = DeclarationNode(startLocation, kind, typeNode, nameNode)

            # Finish definition.
            defLocation = tokens.eat(ExprToken.definition)
            defs.append(
                declNode
                if defLocation is None
                else DefinitionNode(defLocation, declNode, parse_expr_top())
            )

            if tokens.eat(ExprToken.separator, ",") is None:
                return defs

    def parse_decl(keyword: str, startLocation: InputLocation) -> DeclarationNode:
        kind = {
            "ctx": DeclarationKind.constant,
            "def": DeclarationKind.constant,
            "var": DeclarationKind.variable,
        }[keyword]

        # Type.
        typeLocation = tokens.eat(ExprToken.identifier)
        if typeLocation is None:
            kindDesc = {
                "ctx": "context",
                "def": "constant/reference",
                "var": "variable",
            }[keyword]
            raise bad_token_kind(f"{kindDesc} definition", "type name")

        # Merge reference indicator '&' into type.
        if tokens.location.span[0] == typeLocation.span[1]:
            ampLocation = tokens.eat(ExprToken.operator, "&")
            if ampLocation is not None:
                typeLocation = merge_span(typeLocation, ampLocation)
                if kind is DeclarationKind.variable:
                    raise ParseError(
                        'references can only be defined using the "def" keyword',
                        merge_span(startLocation, ampLocation),
                    )
                kind = DeclarationKind.reference

        typeNode = IdentifierNode(typeLocation, typeLocation.text)

        # Name.
        nameLocation = tokens.eat(ExprToken.identifier)
        if nameLocation is None:
            raise bad_token_kind(f"{kind.name} definition", f"{kind.name} name")
        nameNode = IdentifierNode(nameLocation, nameLocation.text)

        return DeclarationNode(startLocation, kind, typeNode, nameNode)

    def parse_ident() -> IdentifierNode | OperatorNode:
        location = tokens.eat(ExprToken.identifier)
        assert location is not None, tokens.location

        identifier = IdentifierNode(location, location.text)
        if tokens.peek(ExprToken.bracket, "("):
            return parse_function_call(identifier)
        else:
            return identifier

    def parse_function_call(name: IdentifierNode) -> OperatorNode:
        openLocation = tokens.eat(ExprToken.bracket, "(")
        assert openLocation is not None, tokens.location

        exprs: list[ParseNode] = [name]
        closeLocation = tokens.eat(ExprToken.bracket, ")")
        while closeLocation is None:
            exprs.append(parse_expr_top())
            closeLocation = tokens.eat(ExprToken.bracket, ")")
            if closeLocation is None:
                if tokens.eat(ExprToken.separator, ",") is None:
                    raise bad_token_kind("function call arguments", '"," or ")"')

        location = merge_span(openLocation, closeLocation)
        return OperatorNode(location, Operator.call, tuple(exprs))

    def parse_number() -> NumberNode:
        location = tokens.eat(ExprToken.number)
        assert location is not None, tokens.location
        try:
            value, width = parseInt(location.text)
        except ValueError as ex:
            raise ParseError(f"{ex}", location) from ex
        else:
            return NumberNode(location, value, width)

    parse_expr_top = parse_or
    topForMode = {
        _ParseMode.single: parse_expr_top,
        _ParseMode.multi: parse_list,
        _ParseMode.registers: parse_regs_top,
        _ParseMode.context: parse_context_top,
        _ParseMode.statement: parse_statement_top,
    }

    expr = topForMode[mode]()
    if tokens.peek(ExprToken.other):
        raise ParseError(
            f'unexpected character "{tokens.value}" in expression', tokens.location
        )
    if not tokens.end:
        raise ParseError(
            f'found {tokens.kind.name} "{tokens.value}" in an unexpected place',
            tokens.location,
        )
    return expr


def parse_expr(location: InputLocation) -> ParseNode:
    return cast(ParseNode, _parse(location, _ParseMode.single))


def parse_expr_list(location: InputLocation) -> Iterable[ParseNode]:
    return cast(Iterable[ParseNode], _parse(location, _ParseMode.multi))


def parse_regs(location: InputLocation) -> Iterable[DefDeclNode]:
    return cast(Iterable[DefDeclNode], _parse(location, _ParseMode.registers))


def parse_context(location: InputLocation) -> Iterable[ContextNode]:
    return cast(Iterable[ContextNode], _parse(location, _ParseMode.context))


def parse_statement(location: InputLocation) -> ParseNode:
    return cast(ParseNode, _parse(location, _ParseMode.statement))
