from .linereader import BadInput, mergeSpan
from .types import unlimited

from enum import Enum
import re

class ParseError(BadInput):
    '''Raised when the input text cannot be parsed into an expression.
    '''

Token = Enum('Token', ( # pylint: disable=invalid-name
    'keyword', 'identifier', 'label', 'flagtest', 'number', 'operator',
    'bracket', 'assignment', 'definition', 'separator', 'whitespace', 'other',
    'end'
    ))

class ExpressionTokenizer:

    _pattern = re.compile('|'.join(
        '(?P<%s>%s)' % (token.name, regex) for token, regex in (
            # pylint: disable=bad-whitespace
            (Token.keyword,     r'var|def|branch|nop'),
            (Token.identifier,  r"[A-Za-z_][A-Za-z0-9_]*'?"),
            (Token.label,       r"@[A-Za-z_][A-Za-z0-9_]*'?"),
            (Token.flagtest,    r"\?[A-Za-z_][A-Za-z0-9_]*'?"),
            (Token.number,      r'[%$0-9]\w*'),
            (Token.operator,    r'<<|>>|==|!=|<=|>=|[<>&|\^+\-~!;]'),
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

class ParseNode:
    __slots__ = ('location', 'treeLocation')

    def __init__(self, location):
        self.location = location
        self.treeLocation = location
        '''Location information, where the span includes to the entire tree
        under this node.'''

    def __repr__(self):
        return '%s(%s)' % (
            self.__class__.__name__,
            ', '.join(
                '%s=%s' % (slot, getattr(self, slot))
                for cls in self.__class__.__mro__[:-2] # drop ParseNode, object
                for slot in cls.__slots__
                )
            )

    def __iter__(self):
        yield self

class EmptyNode(ParseNode):
    __slots__ = ()

class LabelNode(ParseNode):
    __slots__ = ('name', )

    def __init__(self, name, location):
        ParseNode.__init__(self, location)
        self.name = name

class FlagTestNode(ParseNode):
    __slots__ = ('name', )

    def __init__(self, name, location):
        ParseNode.__init__(self, location)
        self.name = name

class BranchNode(ParseNode):
    __slots__ = ('cond', 'target')

    def __init__(self, cond, target, location):
        ParseNode.__init__(self, location)
        self.cond = cond
        self.target = target
        self.treeLocation = mergeSpan(location, target.treeLocation)

class AssignmentNode(ParseNode):
    __slots__ = ('lhs', 'rhs')

    def __init__(self, lhs, rhs, location):
        ParseNode.__init__(self, location)
        self.lhs = lhs
        self.rhs = rhs
        self.treeLocation = mergeSpan(lhs.treeLocation, rhs.treeLocation)

    def __iter__(self):
        yield self
        yield from self.lhs
        yield from self.rhs

Operator = Enum('Operator', ( # pylint: disable=invalid-name
    'bitwise_and', 'bitwise_or', 'bitwise_xor', 'add', 'sub', 'complement',
    'bitwise_complement', 'concatenation', 'lookup', 'negation', 'slice',
    'shift_left', 'shift_right', 'equal', 'unequal', 'lesser', 'lesser_equal',
    'greater', 'greater_equal', 'call'
    ))

class OperatorNode(ParseNode):
    __slots__ = ('operator', 'operands')

    def __init__(self, operator, operands, location):
        ParseNode.__init__(self, location)
        self.operator = operator
        self.operands = tuple(operands)
        self.treeLocation = self._treeLocation()

    def __iter__(self):
        yield self
        for operand in self.operands:
            if operand is not None:
                yield from operand

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
                mergeSpan(location, name.treeLocation)

class DefinitionNode(ParseNode):
    __slots__ = ('decl', 'value')

    def __init__(self, decl, value, location):
        ParseNode.__init__(self, location)
        self.decl = decl
        self.value = value
        self.treeLocation = mergeSpan(decl.treeLocation, value.treeLocation)

    def __iter__(self):
        yield self
        yield from self.decl
        yield from self.value

class NumberNode(ParseNode):
    __slots__ = ('value', 'width')

    def __init__(self, value, width, location):
        ParseNode.__init__(self, location)
        self.value = value
        self.width = width

_ParseMode = Enum('_ParseMode', ( # pylint: disable=invalid-name
    'single', 'multi', 'statement', 'context'
    ))

def _parse(exprStr, location, mode):
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

    def parseStatementTop():
        location = token.location
        if token.peek(Token.label):
            return parseLabel()
        elif token.eat(Token.keyword, 'branch'):
            if token.peek(Token.label):
                cond = NumberNode(1, 1, location)
            else:
                cond = parseExprTop()
            target = parseLabel()
            return BranchNode(cond, target, location)
        elif token.eat(Token.keyword, 'nop'):
            return EmptyNode(location)
        else:
            return parseAssign()

    def parseLabel():
        value = token.value
        location = token.location
        if token.eat(Token.label):
            return LabelNode(value[1:], location)
        else:
            raise badTokenKind('label', '"@<name>"')

    def parseAssign():
        expr = parseExprTop()
        location = token.location
        if token.eat(Token.assignment, ':='):
            return AssignmentNode(expr, parseExprTop(), location)
        else:
            return expr

    def parseList():
        exprs = []
        while True:
            exprs.append(parseExprTop())
            if not token.eat(Token.separator, ','):
                return exprs

    def parseContext():
        elems = []
        while True:
            if token.peek(Token.identifier):
                node = parseDecl('ctx', token.location)
                defLocation = token.location
                if token.eat(Token.definition):
                    node = DefinitionNode(node, parseExprTop(), defLocation)
            elif token.peek(Token.flagtest):
                node = FlagTestNode(token.value[1:], token.location)
                token.eat(Token.flagtest)
            else:
                raise badTokenKind(
                    'context element', 'placeholder declaration or flag test'
                    )
            elems.append(node)
            if not token.eat(Token.separator, ','):
                return elems

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
        expr = parseEqual()
        location = token.location
        if token.eat(Token.operator, '&'):
            return OperatorNode(
                Operator.bitwise_and, (expr, parseAnd()), location
                )
        else:
            return expr

    def parseEqual():
        expr = parseCompare()
        location = token.location
        if token.eat(Token.operator, '=='):
            return OperatorNode(
                Operator.equal, (expr, parseEqual()), location
                )
        elif token.eat(Token.operator, '!='):
            return OperatorNode(
                Operator.unequal, (expr, parseEqual()), location
                )
        else:
            return expr

    def parseCompare():
        expr = parseShift()
        location = token.location
        if token.eat(Token.operator, '<'):
            return OperatorNode(
                Operator.lesser, (expr, parseCompare()), location
                )
        elif token.eat(Token.operator, '<='):
            return OperatorNode(
                Operator.lesser_equal, (expr, parseCompare()), location
                )
        elif token.eat(Token.operator, '>='):
            return OperatorNode(
                Operator.greater_equal, (expr, parseCompare()), location
                )
        elif token.eat(Token.operator, '>'):
            return OperatorNode(
                Operator.greater, (expr, parseCompare()), location
                )
        else:
            return expr

    def parseShift():
        expr = parseAddSub()
        location = token.location
        if token.eat(Token.operator, '<<'):
            return OperatorNode(
                Operator.shift_left, (expr, parseShift()), location
                )
        elif token.eat(Token.operator, '>>'):
            return OperatorNode(
                Operator.shift_right, (expr, parseShift()), location
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
        elif token.eat(Token.operator, '~'):
            return OperatorNode(
                Operator.bitwise_complement, (parseUnary(),), location
                )
        else:
            return parseIndexed()

    def parseIndexed():
        expr = parseGroup()
        while True:
            openLocation = token.location
            if not token.eat(Token.bracket, '['):
                return expr

            start = None if token.peek(Token.separator, ':') else parseExprTop()
            if token.eat(Token.separator, ':'):
                end = None if token.peek(Token.bracket, ']') else parseExprTop()
                location = mergeSpan(openLocation, token.location)
                if token.eat(Token.bracket, ']'):
                    expr = OperatorNode(
                        Operator.slice, (expr, start, end), location
                        )
                else:
                    raise badTokenKind('slice', '"]"')
            else:
                location = mergeSpan(openLocation, token.location)
                if token.eat(Token.bracket, ']'):
                    expr = OperatorNode(
                        Operator.lookup, (expr, start), location
                        )
                else:
                    raise badTokenKind('slice/lookup', '":" or "]"')

    def parseGroup():
        openLocation = token.location
        if token.eat(Token.bracket, '('):
            expr = parseExprTop()
            closeLocation = token.location
            if not token.eat(Token.bracket, ')'):
                raise badTokenKind('parenthesized', ')')
            expr.treeLocation = mergeSpan(openLocation, closeLocation)
            return expr
        elif token.kind is Token.keyword and token.value in ('var', 'def'):
            return parseDefinition()
        elif token.kind is Token.identifier:
            ident = parseIdent()
            if isinstance(ident, IdentifierNode) and ident.name == 'ret':
                declNode = DeclarationNode(
                    DeclarationKind.reference, None, ident, None
                    )
                defLocation = token.location
                if token.eat(Token.definition):
                    return DefinitionNode(declNode, parseExprTop(), defLocation)
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

        # Declaration.
        declNode = parseDecl(keyword, keywordLocation)

        # Value.
        if declNode.kind is DeclarationKind.variable:
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
                raise badTokenKind('%s value' % declNode.kind.name, '"="')
            return DefinitionNode(declNode, parseExprTop(), defLocation)

    def parseDecl(keyword, startLocation):
        # Type.
        typeName = token.value
        typeLocation = token.location
        if not token.eat(Token.identifier):
            raise badTokenKind(
                '%s definition' % {
                    'ctx': 'context',
                    'def': 'constant/reference',
                    'var': 'variable',
                    }[keyword],
                'type name'
                )
        ampLocation = token.location
        if ampLocation.span[0] == typeLocation.span[1] \
                and token.eat(Token.operator, '&'):
            if keyword == 'var':
                raise ParseError(
                    'references can only be defined using the "def" keyword',
                    mergeSpan(startLocation, ampLocation)
                    )
            typeName += '&'
            typeLocation = mergeSpan(typeLocation, ampLocation)
            kind = DeclarationKind.reference
        else:
            kind = {
                'ctx': DeclarationKind.constant,
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

        return DeclarationNode(kind, typeNode, nameNode, startLocation)

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
                exprs.append(parseExprTop())
                closeLocation = token.location
                if token.eat(Token.bracket, ')'):
                    break
                if not token.eat(Token.separator, ','):
                    raise badTokenKind('function call arguments', '"," or ")"')
        location = mergeSpan(openLocation, closeLocation)
        return OperatorNode(Operator.call, exprs, location)

    def parseNumber():
        valueStr = token.value
        location = token.location
        if not token.eat(Token.number):
            assert False, token
        try:
            value, width = parseInt(valueStr)
        except ValueError as ex:
            raise ParseError('%s' % ex, location)
        else:
            return NumberNode(value, width, location)

    parseExprTop = parseOr
    topForMode = {
        _ParseMode.single: parseExprTop,
        _ParseMode.multi: parseList,
        _ParseMode.context: parseContext,
        _ParseMode.statement: parseStatementTop,
        }

    expr = topForMode[mode]()
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
    return _parse(exprStr, location, _ParseMode.single)

def parseExprList(exprStr, location):
    return _parse(exprStr, location, _ParseMode.multi)

def parseContext(exprStr, location):
    return _parse(exprStr, location, _ParseMode.context)

def parseStatement(exprStr, location):
    return _parse(exprStr, location, _ParseMode.statement)

def parseInt(valueStr):
    '''Parse the given string as a binary, decimal or hexadecimal integer.
    Returns a pair containing the value and the width of the literal in bits.
    Raises ValueError if the given string does not represent an integer.
    '''
    if valueStr[0] == '$':
        return parseDigits(valueStr[1:], 16), (len(valueStr) - 1) * 4
    elif valueStr[0] == '%':
        return parseDigits(valueStr[1:], 2), len(valueStr) - 1
    elif valueStr[0] == '0' and len(valueStr) != 1:
        raise ValueError(
            'leading zeroes not allowed on decimal number: %s' % valueStr
            )
    else:
        return parseDigits(valueStr, 10), unlimited

def parseDigits(digits, base):
    '''Wrapper around the "int" constructor that generated a slightly more
    detailed ValueError message if the given string contains characters that
    are not valid as digits in the given base.
    '''
    try:
        return int(digits, base)
    except ValueError:
        baseDesc = {2: 'binary', 10: 'decimal', 16: 'hexadecimal'}
        raise ValueError(
            'bad %s number: %s' % (baseDesc[base], digits)
            )
