from .expression import (
    AddOperator, Complement, Concatenation, IOChannel, IOReference, IntLiteral,
    IntType, LocalReference, LocalValue, Slice, Subtraction
    )
import re

def parseType(typeName):
    if not typeName.startswith('u'):
        raise ValueError(
            'type name "%s" does not start with "u"' % typeName)
    widthStr = typeName[1:]
    if not widthStr.isdigit():
        raise ValueError(
            'integer type "%s" is not of the form "u<width>"' % typeName)
    return IntType(int(widthStr))

def parseLocalDecl(typeDecl, name):
    if typeDecl.endswith('&'):
        return LocalReference(name, parseType(typeDecl[:-1]))
    else:
        return LocalValue(name, parseType(typeDecl))

class ExpressionTokenizer:

    _pattern = re.compile('|'.join('(?P<%s>%s)' % pair for pair in (
        # pylint: disable=bad-whitespace
        ('identifier',  r"[A-Za-z_][A-Za-z0-9_]*'?"),
        ('number',      r'[%$0-9]\w*'),
        ('operator',    r'[&|\^+\-~!;]|==|!='),
        ('bracket',     r'[\[\]():]'),
        ('assignment',  r':='),
        ('whitespace',  r'\s+'),
        ('other',       r'.'),
        )))

    def __init__(self, exprStr):
        self._tokens = self._pattern.finditer(exprStr)
        self.__next__()

    def __next__(self):
        while True:
            try:
                match = next(self._tokens)
            except StopIteration:
                self.kind = 'end'
                self.value = None
                break
            kind = match.lastgroup
            if kind != 'whitespace':
                self.kind = kind
                self.value = match.group(kind)
                break

    def eat(self, kind, value=None):
        '''Consumes the current token if it matches the given kind and,
        if specified, also the given value. Returns True if the token is
        consumed, False otherwise.
        '''
        if self.kind == kind and (value is None or self.value == value):
            next(self)
            return True
        else:
            return False

def parseExpr(exprStr, context):
    token = ExpressionTokenizer(exprStr)

    class BadToken(ValueError):
        def __init__(self, where, expected):
            msg = 'bad %s expression: expected %s, got %s "%s"' % (
                where, expected, token.kind, token.value
                )
            ValueError.__init__(self, msg)

    def parseAddSub():
        expr = parseConcat()
        if token.eat('operator', '+'):
            return AddOperator(expr, parseTop())
        elif token.eat('operator', '-'):
            return Subtraction(expr, parseTop())
        else:
            return expr

    def parseConcat():
        exprs = [parseUnary()]
        while token.eat('operator', ';'):
            exprs.append(parseUnary())
        return exprs[0] if len(exprs) == 1 else Concatenation(*exprs)

    def parseUnary():
        if token.eat('operator', '-'):
            return Complement(parseUnary())
        else:
            return parseSlice()

    def parseSlice():
        expr = parseGroup()
        if isinstance(expr, IOChannel):
            return parseIOReference(expr)
        if not token.eat('bracket', '['):
            return expr

        # Bitwise lookup or bit string slicing.
        start = parseTop()
        if token.eat('bracket', ':'):
            end = parseTop()
            if not token.eat('bracket', ']'):
                raise BadToken('slice', '"]"')
        elif token.eat('bracket', ']'):
            end = None
        else:
            raise BadToken('slice/lookup', '":" or "]"')

        # Convert start/end to start/width.
        # pylint: disable=no-member
        start = start.simplify()
        try:
            index = start.value
        except AttributeError:
            raise ValueError('index is not constant: %s' % start)
        if end is None:
            width = 1
        else:
            end = end.simplify()
            try:
                width = end.value - index
            except AttributeError:
                raise ValueError('index is not constant: %s' % end)

        return Slice(expr, index, width)

    def parseIOReference(channel):
        # I/O lookup.
        if not token.eat('bracket', '['):
            raise BadToken('I/O reference', '"["')
        index = parseTop()
        if not token.eat('bracket', ']'):
            raise BadToken('I/O index', '"]"')
        return IOReference(channel, index)

    def parseGroup():
        if token.eat('bracket', '('):
            expr = parseTop()
            if not token.eat('bracket', ')'):
                raise BadToken('parenthesized', ')')
            return expr
        else:
            return parseIdent()

    def parseIdent():
        if token.kind == 'number':
            return parseNumber()
        if token.kind != 'identifier':
            raise BadToken('innermost', 'identifier')

        # Look up identifier in context.
        name = token.value
        try:
            expr = context[name]
        except KeyError:
            raise ValueError('unknown name "%s" in expression' % name)
        else:
            next(token)
            return expr

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
            next(token)
            if value[0] == '0' and len(value) != 1:
                raise ValueError(
                    'leading zeroes not allowed on decimal integer '
                    'literals: %s' % value
                    )
            return IntLiteral.create(int(value))

    parseTop = parseAddSub

    expr = parseTop()
    if token.kind == 'other':
        raise ValueError(
            'unexpected character "%s" in expression' % token.value
            )
    elif token.kind != 'end':
        raise ValueError(
            'found %s "%s" in an unexpected place' % (token.kind, token.value)
            )
    else:
        return expr
