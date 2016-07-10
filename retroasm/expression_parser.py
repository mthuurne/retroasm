from .linereader import BadInput
from .types import unlimited

from enum import Enum
import re

class ParseError(BadInput):
    '''Raised when the input text cannot be parsed into an expression.
    '''

Token = Enum('Token', ( # pylint: disable=invalid-name
    'keyword', 'identifier', 'number', 'operator', 'bracket', 'assignment',
    'definition', 'separator', 'whitespace', 'other', 'end'
    ))

class ExpressionTokenizer:

    _pattern = re.compile('|'.join(
        '(?P<%s>%s)' % (token.name, regex) for token, regex in (
            # pylint: disable=bad-whitespace
            (Token.keyword,     r'var|def'),
            (Token.identifier,  r"[A-Za-z_][A-Za-z0-9_]*'?"),
            (Token.number,      r'[%$0-9]\w*'),
            (Token.operator,    r'[&|\^+\-~!;]|==|!='),
            (Token.bracket,     r'[\[\]()]'),
            (Token.assignment,  r':='),
            (Token.definition,  r'='),
            (Token.separator,   r'[:,]'),
            (Token.whitespace,  r'\s+'),
            (Token.other,       r'.'),
            )
        ))

    def __init__(self, exprStr, location):
        self._tokens = self._pattern.finditer(exprStr)
        self._lineLocation = location
        self._length = len(exprStr)
        self.__next__()

    def __next__(self):
        while True:
            try:
                match = next(self._tokens)
            except StopIteration:
                kind = Token.end
                value = None
                span = (self._length, self._length)
                break
            kind = getattr(Token, match.lastgroup)
            if kind is not Token.whitespace:
                group = kind.name
                value = match.group(group)
                span = match.span(group)
                break
        baseSpan = self._lineLocation.span
        if baseSpan is not None:
            shift = baseSpan[0]
            span = (shift + span[0], shift + span[1])
        self.kind = kind
        self.value = value
        self.location = self._lineLocation.updateSpan(span)

    def peek(self, kind, value=None):
        '''Returns True if the current token matches the given kind and,
        if specified, also the given value, False otherwise.
        '''
        return self.kind is kind and (value is None or self.value == value)

    def eat(self, kind, value=None):
        '''Consumes the current token if it matches the given kind and,
        if specified, also the given value. Returns True if the token is
        consumed, False otherwise.
        '''
        found = self.peek(kind, value)
        if found:
            next(self)
        return found

def _mergeSpan(fromLocation, toLocation):
    mergedSpan = (fromLocation.span[0], toLocation.span[1])
    mergedLocation = fromLocation.updateSpan(mergedSpan)
    assert mergedLocation == toLocation.updateSpan(mergedSpan), \
            (fromLocation, toLocation)
    return mergedLocation

class ParseNode:
    __slots__ = ('location', 'treeLocation')

    def __init__(self, location):
        self.location = location
        self.treeLocation = location
        '''Location information, where the span includes to the entire tree
        under this node.'''

class AssignmentNode(ParseNode):
    __slots__ = ('lhs', 'rhs')

    def __init__(self, lhs, rhs, location):
        ParseNode.__init__(self, location)
        self.lhs = lhs
        self.rhs = rhs
        self.treeLocation = _mergeSpan(lhs.treeLocation, rhs.treeLocation)

Operator = Enum('Operator', ( # pylint: disable=invalid-name
    'bitwise_and', 'bitwise_or', 'bitwise_xor', 'add', 'sub', 'complement',
    'concatenation', 'lookup', 'negation', 'slice', 'call'
    ))

class OperatorNode(ParseNode):
    __slots__ = ('operator', 'operands')

    def __init__(self, operator, operands, location):
        ParseNode.__init__(self, location)
        self.operator = operator
        self.operands = operands
        self.treeLocation = self._treeLocation()

    def _treeLocation(self):
        location = self.location
        baseLocation = location.updateSpan(None)
        treeStart, treeEnd = location.span
        for operand in self.operands:
            if operand is None:
                continue
            location = operand.treeLocation
            assert location.updateSpan(None) == baseLocation
            start, end = location.span
            treeStart = min(treeStart, start)
            treeEnd = max(treeEnd, end)
        return baseLocation.updateSpan((treeStart, treeEnd))

class IdentifierNode(ParseNode):
    __slots__ = ('name',)

    def __init__(self, name, location):
        ParseNode.__init__(self, location)
        self.name = name

DeclarationKind = Enum('DeclarationKind', ( # pylint: disable=invalid-name
    'variable', 'constant', 'reference'
    ))

class DeclarationNode(ParseNode):
    __slots__ = ('kind', 'type', 'name')

    def __init__(self, kind, typ, name, location):
        ParseNode.__init__(self, location)
        self.kind = kind
        self.type = typ
        self.name = name
        self.treeLocation = name.treeLocation if location is None else \
                _mergeSpan(location, name.treeLocation)

class DefinitionNode(ParseNode):
    __slots__ = ('decl', 'value')

    def __init__(self, decl, value, location):
        ParseNode.__init__(self, location)
        self.decl = decl
        self.value = value
        self.treeLocation = _mergeSpan(decl.treeLocation, value.treeLocation)

class NumberNode(ParseNode):
    __slots__ = ('value', 'width')

    def __init__(self, value, width, location):
        ParseNode.__init__(self, location)
        self.value = value
        self.width = width

def _parse(exprStr, location, statement):
    token = ExpressionTokenizer(exprStr, location)

    def badTokenKind(where, expected):
        if token.kind is Token.end:
            gotDesc = 'end of input'
        else:
            gotDesc = '%s "%s"' % (token.kind.name, token.value)
        msg = 'bad %s expression: expected %s, got %s' % (
            where, expected, gotDesc
            )
        return ParseError(msg, token.location)

    def parseAssign():
        expr = parseTop()
        location = token.location
        if token.eat(Token.assignment, ':='):
            return AssignmentNode(expr, parseTop(), location)
        else:
            return expr

    def parseOr():
        expr = parseXor()
        location = token.location
        if token.eat(Token.operator, '|'):
            return OperatorNode(
                Operator.bitwise_or, (expr, parseOr()), location
                )
        else:
            return expr

    def parseXor():
        expr = parseAnd()
        location = token.location
        if token.eat(Token.operator, '^'):
            return OperatorNode(
                Operator.bitwise_xor, (expr, parseXor()), location
                )
        else:
            return expr

    def parseAnd():
        expr = parseAddSub()
        location = token.location
        if token.eat(Token.operator, '&'):
            return OperatorNode(
                Operator.bitwise_and, (expr, parseAnd()), location
                )
        else:
            return expr

    def parseAddSub(expr=None):
        if expr is None:
            expr = parseConcat()
        location = token.location
        if token.eat(Token.operator, '+'):
            return parseAddSub(
                OperatorNode(Operator.add, (expr, parseConcat()), location)
                )
        elif token.eat(Token.operator, '-'):
            return parseAddSub(
                OperatorNode(Operator.sub, (expr, parseConcat()), location)
                )
        else:
            return expr

    def parseConcat():
        expr = parseUnary()
        location = token.location
        if token.eat(Token.operator, ';'):
            return OperatorNode(
                Operator.concatenation, (expr, parseConcat()), location
                )
        else:
            return expr

    def parseUnary():
        location = token.location
        if token.eat(Token.operator, '-'):
            return OperatorNode(Operator.complement, (parseUnary(),), location)
        elif token.eat(Token.operator, '!'):
            return OperatorNode(Operator.negation, (parseUnary(),), location)
        else:
            return parseIndexed()

    def parseIndexed():
        expr = parseGroup()
        openLocation = token.location
        if not token.eat(Token.bracket, '['):
            return expr

        start = None if token.peek(Token.separator, ':') else parseTop()
        if token.eat(Token.separator, ':'):
            end = None if token.peek(Token.bracket, ']') else parseTop()
            location = _mergeSpan(openLocation, token.location)
            if token.eat(Token.bracket, ']'):
                return OperatorNode(
                    Operator.slice, (expr, start, end), location
                    )
            else:
                raise badTokenKind('slice', '"]"')
        else:
            location = _mergeSpan(openLocation, token.location)
            if token.eat(Token.bracket, ']'):
                return OperatorNode(Operator.lookup, (expr, start), location)
            else:
                raise badTokenKind('slice/lookup', '":" or "]"')

    def parseGroup():
        openLocation = token.location
        if token.eat(Token.bracket, '('):
            expr = parseTop()
            closeLocation = token.location
            if not token.eat(Token.bracket, ')'):
                raise badTokenKind('parenthesized', ')')
            expr.treeLocation = _mergeSpan(openLocation, closeLocation)
            return expr
        elif token.kind is Token.keyword:
            return parseDefinition()
        elif token.kind is Token.identifier:
            ident = parseIdent()
            if isinstance(ident, IdentifierNode) and ident.name == 'ret':
                declNode = DeclarationNode(
                    DeclarationKind.reference, None, ident, None
                    )
                defLocation = token.location
                if token.eat(Token.definition):
                    return DefinitionNode(declNode, parseTop(), defLocation)
            return ident
        elif token.kind is Token.number:
            return parseNumber()
        else:
            raise badTokenKind(
                'innermost', 'identifier, number or function call'
                )

    def parseDefinition():
        # Keyword.
        keyword = token.value
        keywordLocation = token.location
        if not token.eat(Token.keyword):
            assert False, token

        # Type.
        typeName = token.value
        typeLocation = token.location
        if not token.eat(Token.identifier):
            raise badTokenKind(
                '%s definition' % {
                    'def': 'constant/reference',
                    'var': 'variable',
                    }[keyword],
                'type name'
                )
        ampLocation = token.location
        if token.eat(Token.operator, '&'):
            if keyword != 'def':
                raise ParseError(
                    'references can only be defined using the "def" keyword',
                    _mergeSpan(keywordLocation, ampLocation)
                    )
            typeName += '&'
            typeLocation = _mergeSpan(typeLocation, ampLocation)
            kind = DeclarationKind.reference
        else:
            kind = {
                'def': DeclarationKind.constant,
                'var': DeclarationKind.variable,
                }[keyword]
        typeNode = IdentifierNode(typeName, typeLocation)

        # Name.
        name = token.value
        nameLocation = token.location
        if not token.eat(Token.identifier):
            raise badTokenKind(
                '%s definition' % kind.name, '%s name' % kind.name
                )
        nameNode = IdentifierNode(name, nameLocation)

        declNode = DeclarationNode(kind, typeNode, nameNode, keywordLocation)

        # Value.
        if kind is DeclarationKind.variable:
            if token.peek(Token.definition):
                raise ParseError(
                    'variables can only get values through assignment '
                    '(use ":=" instead of "=")',
                    token.location
                    )
            return declNode
        else:
            defLocation = token.location
            if not token.eat(Token.definition):
                raise badTokenKind('%s value' % kind.name, '"="')
            return DefinitionNode(declNode, parseTop(), defLocation)

    def parseIdent():
        name = token.value
        location = token.location
        if not token.eat(Token.identifier):
            assert False, token

        identifier = IdentifierNode(name, location)
        if token.peek(Token.bracket, '('):
            return parseFunctionCall(identifier)
        else:
            return identifier

    def parseFunctionCall(name):
        openLocation = token.location
        if not token.eat(Token.bracket, '('):
            assert False, token

        exprs = [name]
        closeLocation = token.location
        if not token.eat(Token.bracket, ')'):
            while True:
                exprs.append(parseTop())
                closeLocation = token.location
                if token.eat(Token.bracket, ')'):
                    break
                if not token.eat(Token.separator, ','):
                    raise badTokenKind('function call arguments', '"," or ")"')
        location = _mergeSpan(openLocation, closeLocation)
        return OperatorNode(Operator.call, exprs, location)

    def parseNumber():
        value = token.value
        location = token.location
        if not token.eat(Token.number):
            assert False, token

        if value[0] == '$':
            value = value[1:]
            base = 16
            width = len(value) * 4
        elif value[0] == '%':
            value = value[1:]
            base = 2
            width = len(value)
        elif value[0] == '0' and len(value) != 1:
            raise ParseError(
                'leading zeroes not allowed on decimal number: %s' % value,
                location
                )
        else:
            base = 10
            width = unlimited

        try:
            return NumberNode(int(value, base), width, location)
        except ValueError:
            baseDesc = {2: 'binary', 10: 'decimal', 16: 'hexadecimal'}
            raise ParseError(
                'bad %s number: %s' % (baseDesc[base], value),
                location
                )

    parseTop = parseOr

    expr = parseAssign() if statement else parseTop()
    if token.kind is Token.other:
        raise ParseError(
            'unexpected character "%s" in expression' % token.value,
            token.location
            )
    elif token.kind is not Token.end:
        raise ParseError(
            'found %s "%s" in an unexpected place'
            % (token.kind.name, token.value),
            token.location
            )
    else:
        return expr

def parseExpr(exprStr, location):
    return _parse(exprStr, location, statement=False)

def parseStatement(exprStr, location):
    return _parse(exprStr, location, statement=True)
