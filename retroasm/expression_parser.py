from .linereader import getSpan, updateSpan
from .types import IntType, Reference, unlimited

from enum import Enum
import re

class ParseError(Exception):
    '''Raised when the input text cannot be parsed into an expression.
    The 'location' attribute contains the location in the instruction set
    definition file that could not be parsed, as a metadata dictionary that can
    be used with LineReader, or None if this information is not available.
    '''

    def __init__(self, msg, location=None):
        Exception.__init__(self, msg)
        self.location = location

def parseType(typeName):
    if not typeName.startswith('u'):
        raise ParseError('type name "%s" does not start with "u"' % typeName)
    widthStr = typeName[1:]
    if not widthStr.isdigit():
        raise ParseError(
            'integer type "%s" is not of the form "u<width>"' % typeName
            )
    return IntType(int(widthStr))

def parseTypeDecl(typeDecl):
    if typeDecl.endswith('&'):
        return Reference(parseType(typeDecl[:-1]))
    else:
        return parseType(typeDecl)

Token = Enum('Token', ( # pylint: disable=invalid-name
    'identifier', 'number', 'operator', 'bracket', 'assignment', 'separator',
    'whitespace', 'other', 'end'
    ))

class ExpressionTokenizer:

    _pattern = re.compile('|'.join(
        '(?P<%s>%s)' % (token.name, regex) for token, regex in (
            # pylint: disable=bad-whitespace
            (Token.identifier,  r"[A-Za-z_][A-Za-z0-9_]*'?"),
            (Token.number,      r'[%$0-9]\w*'),
            (Token.operator,    r'[&|\^+\-~!;]|==|!='),
            (Token.bracket,     r'[\[\]()]'),
            (Token.assignment,  r':='),
            (Token.separator,   r'[:,]'),
            (Token.whitespace,  r'\s+'),
            (Token.other,       r'.'),
            )
        ))

    def __init__(self, exprStr, location):
        self._tokens = self._pattern.finditer(exprStr)
        self._lineLocation = location
        self.__next__()

    def __next__(self):
        while True:
            try:
                match = next(self._tokens)
            except StopIteration:
                kind = Token.end
                value = None
                span = None
                break
            kind = getattr(Token, match.lastgroup)
            if kind is not Token.whitespace:
                group = kind.name
                value = match.group(group)
                span = match.span(group)
                break
        self.kind = kind
        self.value = value
        self.location = updateSpan(self._lineLocation, span)

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

class ParseNode:
    __slots__ = ('location',)

    def __init__(self, location):
        self.location = location

class AssignmentNode(ParseNode):
    __slots__ = ('lhs', 'rhs')

    def __init__(self, lhs, rhs, location):
        ParseNode.__init__(self, location)
        self.lhs = lhs
        self.rhs = rhs

Operator = Enum('Operator', ( # pylint: disable=invalid-name
    'bitwise_and', 'bitwise_or', 'bitwise_xor', 'add', 'sub', 'complement',
    'concatenation', 'lookup', 'slice', 'call'
    ))

class OperatorNode(ParseNode):
    __slots__ = ('operator', 'operands')

    def __init__(self, operator, operands, location):
        ParseNode.__init__(self, location)
        self.operator = operator
        self.operands = operands

class IdentifierNode(ParseNode):
    __slots__ = ('decl', 'name')

    def __init__(self, decl, name, location):
        ParseNode.__init__(self, location)
        self.decl = decl
        self.name = name

class NumberNode(ParseNode):
    __slots__ = ('value', 'width')

    def __init__(self, value, width, location):
        ParseNode.__init__(self, location)
        self.value = value
        self.width = width

def _mergeSpan(fromLocation, toLocation):
    mergedSpan = (getSpan(fromLocation)[0], getSpan(toLocation)[1])
    mergedLocation = updateSpan(fromLocation, mergedSpan)
    assert mergedLocation == updateSpan(toLocation, mergedSpan), \
            (fromLocation, toLocation)
    return mergedLocation

def _parse(exprStr, location, statement):
    token = ExpressionTokenizer(exprStr, location)

    def badTokenKind(where, expected):
        msg = 'bad %s expression: expected %s, got %s "%s"' % (
            where, expected, token.kind.name, token.value
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
            return OperatorNode(Operator.complement, parseUnary(), location)
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
        if token.eat(Token.bracket, '('):
            expr = parseTop()
            if not token.eat(Token.bracket, ')'):
                raise badTokenKind('parenthesized', ')')
            return expr
        elif token.kind is Token.identifier:
            return parseIdent()
        elif token.kind is Token.number:
            return parseNumber()
        else:
            raise badTokenKind(
                'innermost', 'identifier, number or function call'
                )

    def parseIdent():
        name = token.value
        location = token.location
        if not token.eat(Token.identifier):
            assert False, token

        if name == 'var':
            return parseVariableDeclaration()
        identifier = IdentifierNode(None, name, location)
        if token.eat(Token.bracket, '('):
            return parseFunctionCall(identifier)
        else:
            return identifier

    def parseVariableDeclaration():
        # Type.
        decl = token.value
        declLocation = token.location
        if not token.eat(Token.identifier):
            raise badTokenKind('variable declaration', 'type name')

        # Name.
        name = token.value
        location = _mergeSpan(declLocation, token.location)
        if not token.eat(Token.identifier):
            raise badTokenKind('variable declaration', 'variable name')

        return IdentifierNode(decl, name, location)

    def parseFunctionCall(name):
        location = token.location
        exprs = [name]
        if not token.eat(Token.bracket, ')'):
            while True:
                exprs.append(parseTop())
                if token.eat(Token.bracket, ')'):
                    break
                if not token.eat(Token.separator, ','):
                    raise badTokenKind('function call arguments', '"," or ")"')
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
