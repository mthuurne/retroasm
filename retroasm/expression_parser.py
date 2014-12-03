from .expression import (
    AddOperator, Concatenation, Expression, IOChannel, IOReference, IntLiteral,
    IntType, LocalReference, LocalValue, Slice, SubOperator
    )

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

_binaryOperators = {
    '+': AddOperator,
    '-': SubOperator,
}

def parseBinaryOperator(operator, exprStr, context):
    lStr, rStr = exprStr.split(operator, 1)
    lhs = parseExpr(lStr, context)
    rhs = parseExpr(rStr, context)
    return _binaryOperators[operator](lhs, rhs)

def parseTerminal(exprStr, context):
    exprStr = exprStr.strip()
    if exprStr.isdigit():
        if exprStr[0] == '0' and len(exprStr) != 1:
            raise ValueError(
                'leading zeroes not allowed on decimal integer literals: %s'
                % exprStr
                )
        return IntLiteral.create(int(exprStr))
    elif exprStr.startswith('%'):
        return IntLiteral(int(exprStr[1:], 2), IntType(len(exprStr)-1))
    elif exprStr.startswith('$'):
        return IntLiteral(int(exprStr[1:], 16), IntType((len(exprStr)-1) * 4))
    elif exprStr.endswith(']'):
        try:
            name, indexStr = exprStr[:-1].split('[', 1)
        except ValueError:
            raise ValueError('invalid lookup or slice expression: %s' % exprStr)
        try:
            value = context[name]
        except KeyError:
            raise ValueError('the name "%s" does not exist' % name)
        if ':' in indexStr:
            try:
                indexStrLo, indexStrHi = indexStr.split(':')
            except ValueError:
                raise ValueError(
                    'multiple ":" in slice expression: %s' % indexStr
                    )
            try:
                indexLo = parseExpr(indexStrLo, context)
            except ValueError as ex:
                raise ValueError('error in slice lower bound: %s' % ex)
            try:
                indexHi = parseExpr(indexStrHi, context)
            except ValueError as ex:
                raise ValueError('error in slice upper bound: %s' % ex)
        else:
            try:
                indexLo = parseExpr(indexStr, context)
            except ValueError as ex:
                raise ValueError('bad index: %s' % ex)
            indexHi = None
        if isinstance(value, IOChannel):
            if indexHi is None:
                return IOReference(value, indexLo)
            else:
                raise ValueError(
                    '"%s" is an I/O channel and therefore does not support '
                    'slicing' % name
                    )
        else:
            try:
                index = indexLo.value
            except AttributeError:
                raise ValueError('index is not constant: %s' % indexLo)
            if indexHi is None:
                width = 1
            else:
                try:
                    width = indexHi.value - index
                except AttributeError:
                    raise ValueError('index is not constant: %s' % indexHi)
            return Slice(value, index, width)
    elif '+' in exprStr:
        return parseBinaryOperator('+', exprStr, context)
    elif '-' in exprStr:
        return parseBinaryOperator('-', exprStr, context)
    else:
        try:
            expr = context[exprStr]
        except KeyError:
            raise ValueError('unknown name "%s" in expression' % exprStr)
        try:
            Expression.checkInstance(expr)
        except TypeError:
            raise ValueError('name "%s" is not an expression' % exprStr)
        return expr

def parseConcat(exprStr, context):
    return Concatenation(*(
        parseTerminal(sub, context)
        for sub in exprStr.split(';')
        ))

def parseExpr(exprStr, context):
    return parseTerminal(exprStr, context)
