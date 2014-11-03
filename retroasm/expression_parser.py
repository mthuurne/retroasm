from .expression import Concatenation, IntType

def parseType(typeName):
    if not typeName.startswith('i'):
        raise ValueError(
            'type name "%s" does not start with "i"' % typeName)
    widthStr = typeName[1:]
    if not widthStr.isdigit():
        raise ValueError(
            'integer type "%s" is not of the form "i<width>"' % typeName)
    return IntType(int(widthStr))

def parseConcat(exprStr, context):
    def parseSub(sub):
        sub = sub.strip()
        try:
            return context[sub]
        except KeyError:
            raise ValueError('unknown global name "%s" in expression' % sub)
    return Concatenation(parseSub(sub) for sub in exprStr.split(';'))
