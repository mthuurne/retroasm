from __future__ import annotations

from enum import Enum, auto
from typing import Any, Iterable, Iterator, Sequence, Union

from .linereader import BadInput, InputLocation, mergeSpan
from .tokens import TokenEnum
from .types import Width, cast, unlimited


class ParseError(BadInput):
    """Raised when the input text cannot be parsed into an expression.
    """

class ExprToken(TokenEnum):
    # pylint: disable=bad-whitespace
    keyword    = r'var|def|branch|nop'
    multimatch = r"[A-Za-z_][A-Za-z0-9_]*@"
    identifier = r"[A-Za-z_][A-Za-z0-9_]*'?"
    label      = r"@[A-Za-z_][A-Za-z0-9_]*'?"
    flagtest   = r"\?[A-Za-z_][A-Za-z0-9_]*'?"
    number     = r'[%$0-9]\w*'
    operator   = r'<<|>>|==|!=|<=|>=|[<>&|\^+\-~!;]'
    bracket    = r'[\[\]()]'
    assignment = r':='
    definition = r'='
    separator  = r'[:,]'
    other      = r'.'

class ParseNode:
    __slots__ = ('location', 'treeLocation')

    def __init__(self, location: InputLocation):
        self.location = location
        self.treeLocation = location
        """Location information, where the span includes to the entire tree
        under this node."""

    def __repr__(self) -> str:
        return '%s(%s)' % (
            self.__class__.__name__,
            ', '.join(
                f'{slot}={getattr(self, slot)}'
                for cls in self.__class__.__mro__[:-2] # drop ParseNode, object
                for slot in cls.__slots__
                )
            )

    def __iter__(self) -> Iterator[ParseNode]:
        yield self

class EmptyNode(ParseNode):
    __slots__ = ()

class LabelNode(ParseNode):
    __slots__ = ('name', )

    def __init__(self, name: str, location: InputLocation):
        ParseNode.__init__(self, location)
        self.name = name

class FlagTestNode(ParseNode):
    __slots__ = ('name', )

    def __init__(self, name: str, location: InputLocation):
        ParseNode.__init__(self, location)
        self.name = name

class BranchNode(ParseNode):
    __slots__ = ('cond', 'target')

    def __init__(self,
                 cond: ParseNode,
                 target: LabelNode,
                 location: InputLocation
                 ):
        ParseNode.__init__(self, location)
        self.cond = cond
        self.target = target
        self.treeLocation = mergeSpan(location, target.treeLocation)

class AssignmentNode(ParseNode):
    __slots__ = ('lhs', 'rhs')

    def __init__(self, lhs: ParseNode, rhs: ParseNode, location: InputLocation):
        ParseNode.__init__(self, location)
        self.lhs = lhs
        self.rhs = rhs
        self.treeLocation = mergeSpan(lhs.treeLocation, rhs.treeLocation)

    def __iter__(self) -> Iterator[ParseNode]:
        yield self
        yield from self.lhs
        yield from self.rhs

class Operator(Enum):
    bitwise_and = auto()
    bitwise_or = auto()
    bitwise_xor = auto()
    add = auto()
    sub = auto()
    complement = auto()
    bitwise_complement = auto()
    concatenation = auto()
    lookup = auto()
    negation = auto()
    slice = auto()
    shift_left = auto()
    shift_right = auto()
    equal = auto()
    unequal = auto()
    lesser = auto()
    lesser_equal = auto()
    greater = auto()
    greater_equal = auto()
    call = auto()

class OperatorNode(ParseNode):
    __slots__ = ('operator', 'operands')

    def __init__(self,
                 operator: Operator,
                 operands: Iterable[ParseNode | None],
                 location: InputLocation
                 ):
        ParseNode.__init__(self, location)
        self.operator = operator
        self.operands: Sequence[ParseNode | None] = tuple(operands)
        self.treeLocation = self._treeLocation()

    def __iter__(self) -> Iterator[ParseNode]:
        yield self
        for operand in self.operands:
            if operand is not None:
                yield from operand

    def _treeLocation(self) -> InputLocation:
        location = self.location
        baseLocation = location.updateSpan((0, 0))
        treeStart, treeEnd = location.span
        for operand in self.operands:
            if operand is None:
                continue
            location = operand.treeLocation
            assert location.updateSpan((0, 0)) == baseLocation
            start, end = location.span
            treeStart = min(treeStart, start)
            treeEnd = max(treeEnd, end)
        return baseLocation.updateSpan((treeStart, treeEnd))

class IdentifierNode(ParseNode):
    __slots__ = ('name',)

    def __init__(self, name: str, location: InputLocation):
        ParseNode.__init__(self, location)
        self.name = name

class MultiMatchNode(ParseNode):
    __slots__ = ('name',)

    def __init__(self, name: str, location: InputLocation):
        ParseNode.__init__(self, location)
        self.name = name

class DeclarationKind(Enum):
    variable = auto()
    constant = auto()
    reference = auto()

class DeclarationNode(ParseNode):
    __slots__ = ('kind', 'type', 'name')

    def __init__(self,
                 kind: DeclarationKind,
                 typ: IdentifierNode | None,
                 name: IdentifierNode,
                 location: InputLocation
                 ):
        ParseNode.__init__(self, location)
        self.kind = kind
        self.type = typ
        self.name = name
        self.treeLocation = mergeSpan(location, name.treeLocation)

class DefinitionNode(ParseNode):
    __slots__ = ('decl', 'value')

    def __init__(self,
                 decl: DeclarationNode,
                 value: ParseNode,
                 location: InputLocation
                 ):
        ParseNode.__init__(self, location)
        self.decl = decl
        self.value = value
        self.treeLocation = mergeSpan(decl.treeLocation, value.treeLocation)

    def __iter__(self) -> Iterator[ParseNode]:
        yield self
        yield from self.decl
        yield from self.value

class NumberNode(ParseNode):
    __slots__ = ('value', 'width')

    def __init__(self, value: int, width: Width, location: InputLocation):
        ParseNode.__init__(self, location)
        self.value = value
        self.width = width

DefDeclNode = Union[DeclarationNode, DefinitionNode]
ContextNode = Union[DeclarationNode, DefinitionNode, FlagTestNode]

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
            gotDesc = 'end of input'
        else:
            gotDesc = f'{tokens.kind.name} "{tokens.value}"'
        msg = f'bad {where} expression: expected {expected}, got {gotDesc}'
        return ParseError(msg, tokens.location)

    def parseStatementTop() -> ParseNode:
        if tokens.peek(ExprToken.label):
            return parseLabel()
        location = tokens.eat(ExprToken.keyword, 'branch')
        if location is not None:
            cond = (
                NumberNode(1, 1, location)
                if tokens.peek(ExprToken.label)
                else parseExprTop()
                )
            target = parseLabel()
            return BranchNode(cond, target, location)
        location = tokens.eat(ExprToken.keyword, 'nop')
        if location is not None:
            return EmptyNode(location)
        return parseAssign()

    def parseLabel() -> LabelNode:
        location = tokens.eat(ExprToken.label)
        if location is None:
            raise badTokenKind('label', '"@<name>"')
        return LabelNode(location.text[1:], location)

    def parseAssign() -> ParseNode:
        expr = parseExprTop()
        location = tokens.eat(ExprToken.assignment, ':=')
        if location is None:
            return expr
        return AssignmentNode(expr, parseExprTop(), location)

    def parseList() -> Iterable[ParseNode]:
        exprs = []
        while True:
            exprs.append(parseExprTop())
            if tokens.eat(ExprToken.separator, ',') is None:
                return exprs

    def parseContext() -> Iterable[ContextNode]:
        elems = []
        while True:
            node: ContextNode
            if tokens.peek(ExprToken.identifier):
                node = parseDecl('ctx', tokens.location)
                defLocation = tokens.eat(ExprToken.definition)
                if defLocation is not None:
                    node = DefinitionNode(node, parseExprTop(), defLocation)
            else:
                testLocation = tokens.eat(ExprToken.flagtest)
                if testLocation is not None:
                    node = FlagTestNode(testLocation.text[1:], testLocation)
                else:
                    raise badTokenKind(
                        'context element',
                        'placeholder declaration or flag test'
                        )
            elems.append(node)
            if tokens.eat(ExprToken.separator, ',') is None:
                return elems

    def parseOr() -> ParseNode:
        expr = parseXor()
        location = tokens.eat(ExprToken.operator, '|')
        if location is None:
            return expr
        return OperatorNode(Operator.bitwise_or, (expr, parseOr()), location)

    def parseXor() -> ParseNode:
        expr = parseAnd()
        location = tokens.eat(ExprToken.operator, '^')
        if location is None:
            return expr
        return OperatorNode(Operator.bitwise_xor, (expr, parseXor()), location)

    def parseAnd() -> ParseNode:
        expr = parseEqual()
        location = tokens.eat(ExprToken.operator, '&')
        if location is None:
            return expr
        return OperatorNode(Operator.bitwise_and, (expr, parseAnd()), location)

    def parseEqual() -> ParseNode:
        expr = parseCompare()
        location = tokens.eat(ExprToken.operator, '==')
        if location is not None:
            return OperatorNode(
                Operator.equal, (expr, parseEqual()), location
                )
        location = tokens.eat(ExprToken.operator, '!=')
        if location is not None:
            return OperatorNode(
                Operator.unequal, (expr, parseEqual()), location
                )
        return expr

    def parseCompare() -> ParseNode:
        expr = parseShift()
        location = tokens.eat(ExprToken.operator, '<')
        if location is not None:
            return OperatorNode(
                Operator.lesser, (expr, parseCompare()), location
                )
        location = tokens.eat(ExprToken.operator, '<=')
        if location is not None:
            return OperatorNode(
                Operator.lesser_equal, (expr, parseCompare()), location
                )
        location = tokens.eat(ExprToken.operator, '>=')
        if location is not None:
            return OperatorNode(
                Operator.greater_equal, (expr, parseCompare()), location
                )
        location = tokens.eat(ExprToken.operator, '>')
        if location is not None:
            return OperatorNode(
                Operator.greater, (expr, parseCompare()), location
                )
        return expr

    def parseShift() -> ParseNode:
        expr = parseAddSub()
        location = tokens.eat(ExprToken.operator, '<<')
        if location is not None:
            return OperatorNode(
                Operator.shift_left, (expr, parseShift()), location
                )
        location = tokens.eat(ExprToken.operator, '>>')
        if location is not None:
            return OperatorNode(
                Operator.shift_right, (expr, parseShift()), location
                )
        return expr

    def parseAddSub(expr: ParseNode | None = None) -> ParseNode:
        if expr is None:
            expr = parseConcat()
        location = tokens.eat(ExprToken.operator, '+')
        if location is not None:
            return parseAddSub(
                OperatorNode(Operator.add, (expr, parseConcat()), location)
                )
        location = tokens.eat(ExprToken.operator, '-')
        if location is not None:
            return parseAddSub(
                OperatorNode(Operator.sub, (expr, parseConcat()), location)
                )
        return expr

    def parseConcat() -> ParseNode:
        expr = parseUnary()
        location = tokens.eat(ExprToken.operator, ';')
        if location is not None:
            return OperatorNode(
                Operator.concatenation, (expr, parseConcat()), location
                )
        return expr

    def parseUnary() -> ParseNode:
        location = tokens.eat(ExprToken.operator, '-')
        if location is not None:
            return OperatorNode(Operator.complement, (parseUnary(),), location)
        location = tokens.eat(ExprToken.operator, '!')
        if location is not None:
            return OperatorNode(Operator.negation, (parseUnary(),), location)
        location = tokens.eat(ExprToken.operator, '~')
        if location is not None:
            return OperatorNode(
                Operator.bitwise_complement, (parseUnary(),), location
                )
        return parseIndexed()

    def parseIndexed() -> ParseNode:
        expr = parseGroup()
        while True:
            openLocation = tokens.eat(ExprToken.bracket, '[')
            if openLocation is None:
                return expr

            start: ParseNode | None
            sepLocation = tokens.eat(ExprToken.separator, ':')
            if sepLocation is None:
                start = parseExprTop()
                sepLocation = tokens.eat(ExprToken.separator, ':')
            else:
                start = None

            end: ParseNode | None
            closeLocation = tokens.eat(ExprToken.bracket, ']')
            if sepLocation is None:
                if closeLocation is None:
                    raise badTokenKind('slice/lookup', '":" or "]"')
                expr = OperatorNode(
                    Operator.lookup, (expr, start),
                    mergeSpan(openLocation, closeLocation)
                    )
            else:
                if closeLocation is None:
                    end = parseExprTop()
                    closeLocation = tokens.eat(ExprToken.bracket, ']')
                    if closeLocation is None:
                        raise badTokenKind('slice', '"]"')
                else:
                    end = None
                expr = OperatorNode(
                    Operator.slice, (expr, start, end),
                    mergeSpan(openLocation, closeLocation)
                    )

    def parseGroup() -> ParseNode:
        openLocation = tokens.eat(ExprToken.bracket, '(')
        if openLocation is not None:
            expr = parseExprTop()
            closeLocation = tokens.eat(ExprToken.bracket, ')')
            if closeLocation is None:
                raise badTokenKind('parenthesized', ')')
            expr.treeLocation = mergeSpan(openLocation, closeLocation)
            return expr

        if tokens.peek(ExprToken.keyword, 'var') \
        or tokens.peek(ExprToken.keyword, 'def'):
            return parseDefinition()

        if tokens.peek(ExprToken.identifier):
            ident = parseIdent()
            if isinstance(ident, IdentifierNode) and ident.name == 'ret':
                defLocation = tokens.eat(ExprToken.definition)
                if defLocation is not None:
                    declNode = DeclarationNode(
                        DeclarationKind.reference, None, ident, ident.location
                        )
                    return DefinitionNode(declNode, parseExprTop(), defLocation)
            return ident

        if tokens.peek(ExprToken.number):
            return parseNumber()

        multimatchLocation = tokens.eat(ExprToken.multimatch)
        if multimatchLocation is not None:
            value = multimatchLocation.text
            assert value[-1] == '@', multimatchLocation
            return MultiMatchNode(value[:-1], multimatchLocation)

        raise badTokenKind('innermost', 'identifier, number or function call')

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
                    'variables can only get values through assignment '
                    '(use ":=" instead of "=")', defLocation
                    )
            return declNode
        else:
            if defLocation is None:
                raise badTokenKind(f'{declNode.kind.name} value', '"="')
            return DefinitionNode(declNode, parseExprTop(), defLocation)

    def parseRegs() -> Iterable[DefDeclNode]:
        defs: list[DefDeclNode] = []
        typeNode: IdentifierNode | None = None
        while True:
            startLocation = tokens.location

            # Parse type (optional) or name.
            # If type declaration is omitted, the previous type is re-used.
            location = tokens.eat(ExprToken.identifier)
            if location is None:
                raise badTokenKind(
                    'register definition',
                    'type name' if typeNode is None else 'type or register name'
                    )

            # Merge reference indicator '&' into type.
            if tokens.location.span[0] == location.span[1]:
                ampLocation = tokens.eat(ExprToken.operator, '&')
                if ampLocation is not None:
                    location = mergeSpan(location, ampLocation)

            nameLocation = tokens.eat(ExprToken.identifier)
            if nameLocation is None:
                # No second identifier; assume omitted type declaration.
                nameLocation = location
                if typeNode is None:
                    raise badTokenKind('register definition', 'type name')
                if nameLocation.text.endswith('&'):
                    raise badTokenKind('register definition', 'register name')
            else:
                # Second identifier; first identifier is a type declaration.
                typeNode = IdentifierNode(location.text, location)

            nameNode = IdentifierNode(nameLocation.text, nameLocation)

            # Complete the declaration node.
            if tokens.peek(ExprToken.definition):
                if typeNode.name.endswith('&'):
                    kind = DeclarationKind.reference
                else:
                    kind = DeclarationKind.constant
            else:
                kind = DeclarationKind.variable
            declNode = DeclarationNode(kind, typeNode, nameNode, startLocation)

            # Finish definition.
            defLocation = tokens.eat(ExprToken.definition)
            defs.append(
                declNode
                if defLocation is None else
                DefinitionNode(declNode, parseExprTop(), defLocation)
                )

            if tokens.eat(ExprToken.separator, ',') is None:
                return defs

    def parseDecl(keyword: str,
                  startLocation: InputLocation
                  ) -> DeclarationNode:
        kind = {
            'ctx': DeclarationKind.constant,
            'def': DeclarationKind.constant,
            'var': DeclarationKind.variable,
            }[keyword]

        # Type.
        typeLocation = tokens.eat(ExprToken.identifier)
        if typeLocation is None:
            raise badTokenKind(
                '%s definition' % {
                    'ctx': 'context',
                    'def': 'constant/reference',
                    'var': 'variable',
                    }[keyword],
                'type name'
                )

        # Merge reference indicator '&' into type.
        if tokens.location.span[0] == typeLocation.span[1]:
            ampLocation = tokens.eat(ExprToken.operator, '&')
            if ampLocation is not None:
                typeLocation = mergeSpan(typeLocation, ampLocation)
                if kind is DeclarationKind.variable:
                    raise ParseError(
                        'references can only be defined using '
                        'the "def" keyword',
                        mergeSpan(startLocation, ampLocation)
                        )
                kind = DeclarationKind.reference

        typeNode = IdentifierNode(typeLocation.text, typeLocation)

        # Name.
        nameLocation = tokens.eat(ExprToken.identifier)
        if nameLocation is None:
            raise badTokenKind(
                f'{kind.name} definition', f'{kind.name} name'
                )
        nameNode = IdentifierNode(nameLocation.text, nameLocation)

        return DeclarationNode(kind, typeNode, nameNode, startLocation)

    def parseIdent() -> IdentifierNode | OperatorNode:
        location = tokens.eat(ExprToken.identifier)
        assert location is not None, tokens.location

        identifier = IdentifierNode(location.text, location)
        if tokens.peek(ExprToken.bracket, '('):
            return parseFunctionCall(identifier)
        else:
            return identifier

    def parseFunctionCall(name: IdentifierNode) -> OperatorNode:
        openLocation = tokens.eat(ExprToken.bracket, '(')
        assert openLocation is not None, tokens.location

        exprs: list[ParseNode] = [name]
        closeLocation = tokens.eat(ExprToken.bracket, ')')
        while closeLocation is None:
            exprs.append(parseExprTop())
            closeLocation = tokens.eat(ExprToken.bracket, ')')
            if closeLocation is None:
                if tokens.eat(ExprToken.separator, ',') is None:
                    raise badTokenKind('function call arguments', '"," or ")"')

        location = mergeSpan(openLocation, closeLocation)
        return OperatorNode(Operator.call, exprs, location)

    def parseNumber() -> NumberNode:
        location = tokens.eat(ExprToken.number)
        assert location is not None, tokens.location
        try:
            value, width = parseInt(location.text)
        except ValueError as ex:
            raise ParseError(f'{ex}', location)
        else:
            return NumberNode(value, width, location)

    parseExprTop = parseOr
    topForMode = {
        _ParseMode.single: parseExprTop,
        _ParseMode.multi: parseList,
        _ParseMode.registers: parseRegs,
        _ParseMode.context: parseContext,
        _ParseMode.statement: parseStatementTop,
        }

    expr = topForMode[mode]()
    if tokens.peek(ExprToken.other):
        raise ParseError(
            f'unexpected character "{tokens.value}" in expression',
            tokens.location
            )
    if not tokens.end:
        raise ParseError(
            f'found {tokens.kind.name} "{tokens.value}" in an unexpected place',
            tokens.location
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

def parseInt(valueStr: str) -> tuple[int, Width]:
    """Parse the given string as a binary, decimal or hexadecimal integer.
    Returns a pair containing the value and the width of the literal in bits.
    Raises ValueError if the given string does not represent an integer.
    """
    if valueStr[0] == '$':
        return parseDigits(valueStr[1:], 16), (len(valueStr) - 1) * 4
    elif valueStr[0] == '%':
        return parseDigits(valueStr[1:], 2), len(valueStr) - 1
    elif valueStr[0] == '0' and len(valueStr) != 1:
        raise ValueError(
            f'leading zeroes not allowed on decimal number: {valueStr}'
            )
    else:
        return parseDigits(valueStr, 10), unlimited

def parseDigits(digits: str, base: int) -> int:
    """Wrapper around the "int" constructor that generated a slightly more
    detailed ValueError message if the given string contains characters that
    are not valid as digits in the given base.
    """
    try:
        return int(digits, base)
    except ValueError:
        baseDesc = {2: 'binary', 10: 'decimal', 16: 'hexadecimal'}
        raise ValueError(
            f'bad {baseDesc[base]} number: {digits}'
            )
