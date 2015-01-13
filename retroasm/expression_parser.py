from .codeblock import Assignment
from .expression import (
    AddOperator, AndOperator, Complement, Concatenation, IntLiteral,
    OrOperator, Slice, XorOperator
    )
from .function import Function, FunctionCall
from .linereader import getSpan, updateSpan
from .storage import IOChannel, IOReference
from .types import IntType, Reference, unlimited

from enum import Enum
import re

class ParseError(Exception):
    '''Raised when the input text cannot be parsed into an expression.
    The 'span' attribute contains the indices within the input text (line)
    that could not be parsed, or None if this information is not available.
    '''

    def __init__(self, msg, span=None):
        Exception.__init__(self, msg)
        self.span = span

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

    @property
    def span(self):
        return getSpan(self.location)

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

def _parse(exprStr, location, statement):
    token = ExpressionTokenizer(exprStr, location)

    def badTokenKind(where, expected):
        msg = 'bad %s expression: expected %s, got %s "%s"' % (
            where, expected, token.kind.name, token.value
            )
        return ParseError(msg, token.span)

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
        openSpan = token.span
        if not token.eat(Token.bracket, '['):
            return expr

        start = None if token.peek(Token.separator, ':') else parseTop()
        if token.eat(Token.separator, ':'):
            end = None if token.peek(Token.bracket, ']') else parseTop()
            location = updateSpan(token.location, (openSpan[0], token.span[1]))
            if token.eat(Token.bracket, ']'):
                return OperatorNode(
                    Operator.slice, (expr, start, end), location
                    )
            else:
                raise badTokenKind('slice', '"]"')
        else:
            location = updateSpan(token.location, (openSpan[0], token.span[1]))
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
        declSpan = token.span
        if not token.eat(Token.identifier):
            raise badTokenKind('variable declaration', 'type name')

        # Name.
        name = token.value
        location = updateSpan(token.location, (declSpan[0], token.span[1]))
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
                getSpan(location)
                )
        else:
            base = 10
            width = unlimited

        try:
            return NumberNode(int(value, base), width, location)
        except ValueError:
            baseDesc = {2: 'binary', 10: 'decimal', 16: 'hexadecimal'}
            raise ParseError('bad %s number: %s' % (baseDesc[base], value))

    parseTop = parseOr

    expr = parseAssign() if statement else parseTop()
    if token.kind is Token.other:
        raise ParseError(
            'unexpected character "%s" in expression' % token.value, token.span
            )
    elif token.kind is not Token.end:
        raise ParseError(
            'found %s "%s" in an unexpected place'
            % (token.kind.name, token.value),
            token.span
            )
    else:
        return expr

def _convertIdentifier(node, context):
    decl = node.decl
    name = node.name
    if decl is None:
        # Look up identifier in context.
        try:
            return context[name]
        except KeyError:
            raise ParseError(
                'unknown name "%s"' % name,
                getSpan(node.location)
                )
    else:
        # Variable declaration.
        typ = parseType(decl)
        try:
            return context.addVariable(name, typ)
        except AttributeError:
            raise ParseError(
                'attempt to declare variable "%s %s" in a context that does '
                'not support variable declarations' % (decl, name),
                getSpan(node.location)
                )

def _convertFunctionCall(nameNode, *argNodes, context):
    assert isinstance(nameNode, IdentifierNode), nameNode
    assert nameNode.decl is None, nameNode
    name = nameNode.name

    try:
        func = context[nameNode.name]
    except KeyError:
        raise ParseError(
            'no function named "%s"' % name,
            getSpan(nameNode.location)
            )
    if not isinstance(func, Function):
        raise ParseError(
            '"%s" is not a function' % name,
            getSpan(nameNode.location)
            )

    args = tuple(_convert(node, context) for node in argNodes)
    return FunctionCall(func, args)

def _convertLookup(exprNode, indexNode, context):
    expr = _convert(exprNode, context)
    index = _convert(indexNode, context)
    if isinstance(expr, IOChannel):
        return IOReference(expr, index)
    else:
        return Slice(expr, index, 1)

def _convertSlice(location, exprNode, startNode, endNode, context):
    expr = _convert(exprNode, context)

    if startNode is None:
        index = 0
    else:
        start = _convert(startNode, context)
        start = start.simplify()
        try:
            index = start.value
        except AttributeError:
            raise ParseError(
                'start index is not constant: %s' % start,
                getSpan(startNode.location)
                )

    if endNode is None:
        width = expr.width
        if width is unlimited:
            raise ParseError(
                'omitting the end index not allowed when slicing '
                'an unlimited width expression: %s' % expr,
                getSpan(location)
                )
    else:
        end = _convert(endNode, context)
        end = end.simplify()
        try:
            width = end.value - index
        except AttributeError:
            raise ParseError(
                'end index is not constant: %s' % end,
                getSpan(endNode.location)
                )

    return Slice(expr, index, width)

def _convertOperator(node, context):
    operator = node.operator
    if operator is Operator.call:
        return _convertFunctionCall(*node.operands, context=context)
    elif operator is Operator.lookup:
        return _convertLookup(*node.operands, context=context)
    elif operator is Operator.slice:
        return _convertSlice(node.location, *node.operands, context=context)

    exprs = tuple(_convert(node, context) for node in node.operands)
    if operator is Operator.bitwise_and:
        return AndOperator(*exprs)
    elif operator is Operator.bitwise_or:
        return OrOperator(*exprs)
    elif operator is Operator.bitwise_xor:
        return XorOperator(*exprs)
    elif operator is Operator.add:
        return AddOperator(*exprs)
    elif operator is Operator.sub:
        expr1, expr2 = exprs
        return AddOperator(expr1, Complement(expr2))
    elif operator is Operator.complement:
        return Complement(*exprs)
    elif operator is Operator.concatenation:
        return Concatenation(*exprs)
    else:
        assert False, operator

def _convert(node, context):
    if isinstance(node, NumberNode):
        return IntLiteral(node.value, IntType(node.width))
    elif isinstance(node, IdentifierNode):
        return _convertIdentifier(node, context)
    elif isinstance(node, OperatorNode):
        return _convertOperator(node, context)
    else:
        assert False, node

def parseExpr(exprStr, location, context):
    return _convert(_parse(exprStr, location, statement=False), context)

def parseStatement(exprStr, location, context):
    tree = _parse(exprStr, location, statement=True)
    if isinstance(tree, AssignmentNode):
        lhs = _convert(tree.lhs, context)
        rhs = _convert(tree.rhs, context)
        return Assignment(lhs, rhs)
    else:
        return _convert(tree, context)
