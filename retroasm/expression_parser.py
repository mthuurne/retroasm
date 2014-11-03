from .expression import IntType

def parseType(typeName):
    if not typeName.startswith('i'):
        raise ValueError(
            'type name "%s" does not start with "i"' % typeName)
    widthStr = typeName[1:]
    if not widthStr.isdigit():
        raise ValueError(
            'integer type "%s" is not of the form "i<width>"' % typeName)
    return IntType(int(widthStr))
