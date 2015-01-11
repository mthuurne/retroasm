from .codeblock import Assignment
from .expression import (
    AddOperator, AndOperator, Complement, Concatenation, IntLiteral,
    OrOperator, Slice, XorOperator
    )
from .function import Function, FunctionCall
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

    def __init__(self, exprStr):
        self._tokens = self._pattern.finditer(exprStr)
        self.__next__()

    def __next__(self):
        while True:
            try:
                match = next(self._tokens)
            except StopIteration:
                self.kind = Token.end
                self.value = None
                self.span = None
                break
            kind = getattr(Token, match.lastgroup)
            if kind is not Token.whitespace:
                self.kind = kind
                group = kind.name
                self.value = match.group(group)
                self.span = match.span(group)
                break

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

def _parse(exprStr, context, statement):
    token = ExpressionTokenizer(exprStr)

    def badTokenKind(where, expected):
        msg = 'bad %s expression: expected %s, got %s "%s"' % (
            where, expected, token.kind.name, token.value
            )
        return ParseError(msg, token.span)

    def parseAssign():
        expr = parseTop()
        if token.eat(Token.assignment, ':='):
            return Assignment(expr, parseTop())
        else:
            return expr

    def parseOr():
        expr = parseXor()
        if token.eat(Token.operator, '|'):
            return OrOperator(expr, parseOr())
        else:
            return expr

    def parseXor():
        expr = parseAnd()
        if token.eat(Token.operator, '^'):
            return XorOperator(expr, parseXor())
        else:
            return expr

    def parseAnd():
        expr = parseAddSub()
        if token.eat(Token.operator, '&'):
            return AndOperator(expr, parseAnd())
        else:
            return expr

    def parseAddSub():
        exprs = [parseConcat()]
        while True:
            if token.eat(Token.operator, '+'):
                exprs.append(parseConcat())
            elif token.eat(Token.operator, '-'):
                exprs.append(Complement(parseConcat()))
            else:
                break
        return exprs[0] if len(exprs) == 1 else AddOperator(*exprs)

    def parseConcat():
        exprs = [parseUnary()]
        while token.eat(Token.operator, ';'):
            exprs.append(parseUnary())
        return exprs[0] if len(exprs) == 1 else Concatenation(*exprs)

    def parseUnary():
        if token.eat(Token.operator, '-'):
            return Complement(parseUnary())
        else:
            return parseSlice()

    def parseSlice():
        expr = parseGroup()
        if isinstance(expr, IOChannel):
            return parseIOReference(expr)
        if not token.eat(Token.bracket, '['):
            return expr

        # Bitwise lookup or bit string slicing.
        if token.peek(Token.separator, ':'):
            start = IntLiteral.create(0)
        else:
            startIdx = token.span[0]
            start = parseTop()
            startSpan = (startIdx, token.span[0])
        if token.eat(Token.separator, ':'):
            if token.peek(Token.bracket, ']'):
                if expr.width is unlimited:
                    raise ParseError(
                        'omitting the end index not allowed when slicing '
                        'an unlimited width expression: %s' % expr,
                        token.span
                        )
                end = IntLiteral.create(expr.width)
            else:
                endIdx = token.span[0]
                end = parseTop()
                endSpan = (endIdx, token.span[0])
            if not token.eat(Token.bracket, ']'):
                raise badTokenKind('slice', '"]"')
        elif token.eat(Token.bracket, ']'):
            end = None
        else:
            raise badTokenKind('slice/lookup', '":" or "]"')

        # Convert start/end to start/width.
        # pylint: disable=no-member
        start = start.simplify()
        try:
            index = start.value
        except AttributeError:
            raise ParseError('index is not constant: %s' % start, startSpan)
        if end is None:
            width = 1
        else:
            end = end.simplify()
            try:
                width = end.value - index
            except AttributeError:
                raise ParseError('index is not constant: %s' % end, endSpan)

        return Slice(expr, index, width)

    def parseIOReference(channel):
        # I/O lookup.
        if not token.eat(Token.bracket, '['):
            raise badTokenKind('I/O reference', '"["')
        index = parseTop()
        if not token.eat(Token.bracket, ']'):
            raise badTokenKind('I/O index', '"]"')
        return IOReference(channel, index)

    def parseGroup():
        if token.eat(Token.bracket, '('):
            expr = parseTop()
            if not token.eat(Token.bracket, ')'):
                raise badTokenKind('parenthesized', ')')
            return expr
        else:
            return parseIdent()

    def parseIdent():
        if token.kind is Token.number:
            return parseNumber()
        if token.kind is not Token.identifier:
            raise badTokenKind('innermost', 'identifier')

        name = token.value
        nameSpan = token.span
        next(token)
        if token.eat(Token.bracket, '('):
            # Function call.
            try:
                func = context[name]
            except KeyError:
                raise ParseError('no function named "%s"' % name, nameSpan)
            if not isinstance(func, Function):
                raise ParseError('"%s" is not a function' % name, nameSpan)
            args = parseFuncArgs()
            return FunctionCall(func, args)
        elif token.kind is Token.identifier:
            # Two identifiers in a row means the first is a type declaration.
            try:
                typ = parseType(name)
            except ParseError as ex:
                ex.span = nameSpan
                raise
            name = token.value
            declSpan = (nameSpan[0], token.span[1])
            next(token)
            try:
                return context.addVariable(name, typ)
            except AttributeError:
                raise ParseError(
                    'attempt to declare variable "%s %s" in a context that '
                    'does not support variable declarations' % (typ, name),
                    declSpan
                    )
        else:
            # Look up identifier in context.
            try:
                expr = context[name]
            except KeyError:
                raise ParseError(
                    'unknown name "%s" in expression' % name, nameSpan
                    )
            else:
                return expr

    def parseFuncArgs():
        args = []
        if not token.eat(Token.bracket, ')'):
            while True:
                args.append(parseTop())
                if token.eat(Token.bracket, ')'):
                    break
                if not token.eat(Token.separator, ','):
                    raise badTokenKind('function call arguments', '"," or ")"')
        return args

    def parseNumber():
        if token.value[0] == '$':
            value = token.value[1:]
            next(token)
            return IntLiteral(int(value, 16), IntType(len(value) * 4))
        elif token.value[0] == '%':
            value = token.value[1:]
            next(token)
            return IntLiteral(int(value, 2), IntType(len(value)))
        else:
            value = token.value
            if value[0] == '0' and len(value) != 1:
                raise ParseError(
                    'leading zeroes not allowed on decimal integer literal: %s'
                    % value, token.span
                    )
            next(token)
            return IntLiteral.create(int(value))

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

def parseExpr(exprStr, context):
    return _parse(exprStr, context, statement=False)

def parseStatement(exprStr, context):
    return _parse(exprStr, context, statement=True)
