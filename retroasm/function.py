from .types import ReferenceType

class Function:

    def __init__(self, name, retType, args, code):
        if code is not None:
            _checkReturn(retType, code.retRef)

        self.name = name
        self.retType = retType
        self.args = args
        self.code = code

    def __repr__(self):
        return 'Function(%r, %r, %r, %r)' % (
            self.name, self.retType, self.args, self.code
            )

    def __str__(self):
        return 'func %s%s(%s)' % (
            '' if self.retType is None else '%s ' % self.retType,
            self.name,
            ', '.join(
                '%s %s' % (decl, name)
                for name, decl in self.args.items()
                )
            )

    def dump(self):
        print(str(self))
        if self.code is None:
            print('    no code')
        else:
            self.code.dump()

def _checkReturn(retType, retRef):
    '''Check consistency between declared return type and code block.
    Raises ValueError if an inconsistency is found.
    '''
    if retType is None:
        if retRef is not None:
            raise ValueError(
                'function "%s" has no return type, but its code block defines '
                '"ret"' % name
                )
    elif isinstance(retType, ReferenceType):
        if retRef is None:
            raise ValueError(
                'function "%s" should return a reference but does not define '
                '"ret"' % name
                )
    else: # returns a value
        if retRef is None:
            raise ValueError(
                'missing return value assignment in function "%s"' % name
                )
