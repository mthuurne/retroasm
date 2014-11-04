from .expression import Concatenation, IntLiteral, IntType

def parseType(typeName):
    if not typeName.startswith('i'):
        raise ValueError(
            'type name "%s" does not start with "i"' % typeName)
    widthStr = typeName[1:]
    if not widthStr.isdigit():
        raise ValueError(
            'integer type "%s" is not of the form "i<width>"' % typeName)
    return IntType(int(widthStr))

def parseTerminal(exprStr, context):
    exprStr = exprStr.strip()
    if exprStr.isdigit():
        if exprStr[0] == '0' and len(exprStr) != 1:
            raise ValueError(
                'leading zeroes not allowed on decimal integer literals: %s'
                % exprStr
                )
        value = int(exprStr)
        return IntLiteral(value, IntType(value.bit_length()))
    elif exprStr.startswith('%'):
        return IntLiteral(int(exprStr[1:], 2), IntType(len(exprStr)-1))
    elif exprStr.startswith('$'):
        return IntLiteral(int(exprStr[1:], 16), IntType((len(exprStr)-1) * 4))
    else:
        try:
            return context[exprStr]
        except KeyError:
            raise ValueError('unknown global name "%s" in expression' % exprStr)

def parseConcat(exprStr, context):
    return Concatenation(
        parseTerminal(sub, context)
        for sub in exprStr.split(';')
        )
