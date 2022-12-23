from __future__ import annotations

from collections.abc import Iterable
from enum import Enum, auto
from typing import Any, TypeAlias, Union, cast

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
from .tokens import TokenEnum

DefDeclNode: TypeAlias = Union[DeclarationNode, DefinitionNode]
ContextNode: TypeAlias = Union[DeclarationNode, DefinitionNode, FlagTestNode]


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


class _ParseMode(Enum):
    single = auto()
    multi = auto()
    statement = auto()
    registers = auto()
    context = auto()


def _parse(location: InputLocation, mode: _ParseMode) -> Any:
    tokens = ExprToken.scan(location)

    def badTokenKind(where: str, expected: str) -> ParseError:
        if tokens.end:
            gotDesc = "end of input"
        else:
            gotDesc = f'{tokens.kind.name} "{tokens.value}"'
        msg = f"bad {where} expression: expected {expected}, got {gotDesc}"
        return ParseError(msg, tokens.location)

    def parseStatementTop() -> ParseNode:
        if tokens.peek(ExprToken.label):
            return parseLabel()
        if (location := tokens.eat(ExprToken.keyword, "branch")) is not None:
            cond = (
                NumberNode(location, 1, 1)
                if tokens.peek(ExprToken.label)
                else parseExprTop()
            )
            target = parseLabel()
            return BranchNode(location, cond, target)
        if (location := tokens.eat(ExprToken.keyword, "nop")) is not None:
            return EmptyNode(location)
        return parseAssign()

    def parseLabel() -> LabelNode:
        if (location := tokens.eat(ExprToken.label)) is not None:
            return LabelNode(location, location.text[1:])
        raise badTokenKind("label", '"@<name>"')

    def parseAssign() -> ParseNode:
        expr = parseExprTop()
        if (location := tokens.eat(ExprToken.assignment, ":=")) is not None:
            return AssignmentNode(location, expr, parseExprTop())
        return expr

    def parseList() -> Iterable[ParseNode]:
        exprs = []
        while True:
            exprs.append(parseExprTop())
            if tokens.eat(ExprToken.separator, ",") is None:
                return exprs

    def parseContextTop() -> Iterable[ContextNode]:
        elems = []
        while True:
            node: ContextNode
            if tokens.peek(ExprToken.identifier):
                node = parseDecl("ctx", tokens.location)
                if (location := tokens.eat(ExprToken.definition)) is not None:
                    node = DefinitionNode(location, node, parseExprTop())
            elif (location := tokens.eat(ExprToken.flagtest)) is not None:
                node = FlagTestNode(location, location.text[1:])
            else:
                raise badTokenKind(
                    "context element", "placeholder declaration or flag test"
                )
            elems.append(node)
            if tokens.eat(ExprToken.separator, ",") is None:
                return elems

    def parseOr() -> ParseNode:
        expr = parseXor()
        if (location := tokens.eat(ExprToken.operator, "|")) is not None:
            return OperatorNode(location, Operator.bitwise_or, (expr, parseOr()))
        return expr

    def parseXor() -> ParseNode:
        expr = parseAnd()
        if (location := tokens.eat(ExprToken.operator, "^")) is not None:
            return OperatorNode(location, Operator.bitwise_xor, (expr, parseXor()))
        return expr

    def parseAnd() -> ParseNode:
        expr = parseEqual()
        if (location := tokens.eat(ExprToken.operator, "&")) is not None:
            return OperatorNode(location, Operator.bitwise_and, (expr, parseAnd()))
        return expr

    def parseEqual() -> ParseNode:
        expr = parseCompare()
        if (location := tokens.eat(ExprToken.operator, "==")) is not None:
            return OperatorNode(location, Operator.equal, (expr, parseEqual()))
        if (location := tokens.eat(ExprToken.operator, "!=")) is not None:
            return OperatorNode(location, Operator.unequal, (expr, parseEqual()))
        return expr

    def parseCompare() -> ParseNode:
        expr = parseShift()
        if (location := tokens.eat(ExprToken.operator, "<")) is not None:
            return OperatorNode(location, Operator.lesser, (expr, parseCompare()))
        if (location := tokens.eat(ExprToken.operator, "<=")) is not None:
            return OperatorNode(location, Operator.lesser_equal, (expr, parseCompare()))
        if (location := tokens.eat(ExprToken.operator, ">=")) is not None:
            return OperatorNode(
                location, Operator.greater_equal, (expr, parseCompare())
            )
        if (location := tokens.eat(ExprToken.operator, ">")) is not None:
            return OperatorNode(location, Operator.greater, (expr, parseCompare()))
        return expr

    def parseShift() -> ParseNode:
        expr = parseAddSub()
        if (location := tokens.eat(ExprToken.operator, "<<")) is not None:
            return OperatorNode(location, Operator.shift_left, (expr, parseShift()))
        if (location := tokens.eat(ExprToken.operator, ">>")) is not None:
            return OperatorNode(location, Operator.shift_right, (expr, parseShift()))
        return expr

    def parseAddSub(expr: ParseNode | None = None) -> ParseNode:
        if expr is None:
            expr = parseConcat()
        if (location := tokens.eat(ExprToken.operator, "+")) is not None:
            return parseAddSub(
                OperatorNode(location, Operator.add, (expr, parseConcat()))
            )
        if (location := tokens.eat(ExprToken.operator, "-")) is not None:
            return parseAddSub(
                OperatorNode(location, Operator.sub, (expr, parseConcat()))
            )
        return expr

    def parseConcat() -> ParseNode:
        expr = parseUnary()
        if (location := tokens.eat(ExprToken.operator, ";")) is not None:
            return OperatorNode(location, Operator.concatenation, (expr, parseConcat()))
        return expr

    def parseUnary() -> ParseNode:
        if (location := tokens.eat(ExprToken.operator, "-")) is not None:
            return OperatorNode(location, Operator.complement, (parseUnary(),))
        if (location := tokens.eat(ExprToken.operator, "!")) is not None:
            return OperatorNode(location, Operator.negation, (parseUnary(),))
        if (location := tokens.eat(ExprToken.operator, "~")) is not None:
            return OperatorNode(location, Operator.bitwise_complement, (parseUnary(),))
        return parseIndexed()

    def parseIndexed() -> ParseNode:
        expr = parseGroup()
        while True:
            openLocation = tokens.eat(ExprToken.bracket, "[")
            if openLocation is None:
                return expr

            start: ParseNode | None
            sepLocation = tokens.eat(ExprToken.separator, ":")
            if sepLocation is None:
                start = parseExprTop()
                sepLocation = tokens.eat(ExprToken.separator, ":")
            else:
                start = None

            end: ParseNode | None
            closeLocation = tokens.eat(ExprToken.bracket, "]")
            if sepLocation is None:
                if closeLocation is None:
                    raise badTokenKind("slice/lookup", '":" or "]"')
                expr = OperatorNode(
                    merge_span(openLocation, closeLocation),
                    Operator.lookup,
                    (expr, start),
                )
            else:
                if closeLocation is None:
                    end = parseExprTop()
                    closeLocation = tokens.eat(ExprToken.bracket, "]")
                    if closeLocation is None:
                        raise badTokenKind("slice", '"]"')
                else:
                    end = None
                expr = OperatorNode(
                    merge_span(openLocation, closeLocation),
                    Operator.slice,
                    (expr, start, end),
                )

    def parseGroup() -> ParseNode:
        if tokens.eat(ExprToken.bracket, "(") is not None:
            expr = parseExprTop()
            if tokens.eat(ExprToken.bracket, ")") is not None:
                return expr
            raise badTokenKind("parenthesized", ")")

        if tokens.peek(ExprToken.keyword, "var") or tokens.peek(
            ExprToken.keyword, "def"
        ):
            return parseDefinition()

        if tokens.peek(ExprToken.identifier):
            ident = parseIdent()
            if isinstance(ident, IdentifierNode) and ident.name == "ret":
                if (defLocation := tokens.eat(ExprToken.definition)) is not None:
                    declNode = DeclarationNode(
                        ident.location, DeclarationKind.reference, None, ident
                    )
                    return DefinitionNode(defLocation, declNode, parseExprTop())
            return ident

        if tokens.peek(ExprToken.number):
            return parse_number()

        if (multimatchLocation := tokens.eat(ExprToken.multimatch)) is not None:
            value = multimatchLocation.text
            assert value[-1] == "@", multimatchLocation
            return MultiMatchNode(multimatchLocation, value[:-1])

        raise badTokenKind("innermost", "identifier, number or function call")

    def parseDefinition() -> DefDeclNode:
        # Keyword.
        keywordLocation = tokens.eat(ExprToken.keyword)
        assert keywordLocation is not None, tokens.location

        # Declaration.
        declNode = parseDecl(keywordLocation.text, keywordLocation)

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
                raise badTokenKind(f"{declNode.kind.name} value", '"="')
            return DefinitionNode(defLocation, declNode, parseExprTop())

    def parseRegsTop() -> Iterable[DefDeclNode]:
        defs: list[DefDeclNode] = []
        typeNode: IdentifierNode | None = None
        while True:
            startLocation = tokens.location

            # Parse type (optional) or name.
            # If type declaration is omitted, the previous type is re-used.
            location = tokens.eat(ExprToken.identifier)
            if location is None:
                raise badTokenKind(
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
                    raise badTokenKind("register definition", "type name")
                if nameLocation.text.endswith("&"):
                    raise badTokenKind("register definition", "register name")
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
                else DefinitionNode(defLocation, declNode, parseExprTop())
            )

            if tokens.eat(ExprToken.separator, ",") is None:
                return defs

    def parseDecl(keyword: str, startLocation: InputLocation) -> DeclarationNode:
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
            raise badTokenKind(f"{kindDesc} definition", "type name")

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
            raise badTokenKind(f"{kind.name} definition", f"{kind.name} name")
        nameNode = IdentifierNode(nameLocation, nameLocation.text)

        return DeclarationNode(startLocation, kind, typeNode, nameNode)

    def parseIdent() -> IdentifierNode | OperatorNode:
        location = tokens.eat(ExprToken.identifier)
        assert location is not None, tokens.location

        identifier = IdentifierNode(location, location.text)
        if tokens.peek(ExprToken.bracket, "("):
            return parseFunctionCall(identifier)
        else:
            return identifier

    def parseFunctionCall(name: IdentifierNode) -> OperatorNode:
        openLocation = tokens.eat(ExprToken.bracket, "(")
        assert openLocation is not None, tokens.location

        exprs: list[ParseNode] = [name]
        closeLocation = tokens.eat(ExprToken.bracket, ")")
        while closeLocation is None:
            exprs.append(parseExprTop())
            closeLocation = tokens.eat(ExprToken.bracket, ")")
            if closeLocation is None:
                if tokens.eat(ExprToken.separator, ",") is None:
                    raise badTokenKind("function call arguments", '"," or ")"')

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

    parseExprTop = parseOr
    topForMode = {
        _ParseMode.single: parseExprTop,
        _ParseMode.multi: parseList,
        _ParseMode.registers: parseRegsTop,
        _ParseMode.context: parseContextTop,
        _ParseMode.statement: parseStatementTop,
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


def parseExpr(location: InputLocation) -> ParseNode:
    return cast(ParseNode, _parse(location, _ParseMode.single))


def parseExprList(location: InputLocation) -> Iterable[ParseNode]:
    return cast(Iterable[ParseNode], _parse(location, _ParseMode.multi))


def parseRegs(location: InputLocation) -> Iterable[DefDeclNode]:
    return cast(Iterable[DefDeclNode], _parse(location, _ParseMode.registers))


def parseContext(location: InputLocation) -> Iterable[ContextNode]:
    return cast(Iterable[ContextNode], _parse(location, _ParseMode.context))


def parseStatement(location: InputLocation) -> ParseNode:
    return cast(ParseNode, _parse(location, _ParseMode.statement))
