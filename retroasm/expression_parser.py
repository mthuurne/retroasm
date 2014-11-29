from .expression import (
    Concatenation, Expression, IOChannel, IOReference, IntLiteral, IntType,
    LocalReference, LocalValue
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

def parseTerminal(exprStr, context):
    exprStr = exprStr.strip()
    if exprStr.isdigit():
        if exprStr[0] == '0' and len(exprStr) != 1:
            raise ValueError(
                'leading zeroes not allowed on decimal integer literals: %s'
                % exprStr
                )
        return IntLiteral(int(exprStr), IntType(None))
    elif exprStr.startswith('%'):
        return IntLiteral(int(exprStr[1:], 2), IntType(len(exprStr)-1))
    elif exprStr.startswith('$'):
        return IntLiteral(int(exprStr[1:], 16), IntType((len(exprStr)-1) * 4))
    elif exprStr.endswith(']'):
        try:
            name, indexStr = exprStr[:-1].split('[', 1)
        except ValueError:
            raise ValueError('invalid I/O expression: %s' % exprStr)
        try:
            channel = context[name]
        except KeyError:
            raise ValueError('I/O channel "%s" does not exist' % name)
        if not isinstance(channel, IOChannel):
            raise ValueError('"%s" is not an I/O channel' % name)
        try:
            index = parseExpr(indexStr, context)
        except ValueError as ex:
            raise ValueError('bad I/O index: %s' % ex)
        return IOReference(channel, index)
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
    return Concatenation(
        parseTerminal(sub, context)
        for sub in exprStr.split(';')
        )

def parseExpr(exprStr, context):
    return parseTerminal(exprStr, context)
