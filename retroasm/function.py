from .codeblock import ArgumentValue
from .storage import RefArgStorage
from .types import ReferenceType

class Function:

    def __init__(self, retType, args, code):
        if code is not None:
            _checkArgs(args, code.arguments)
            _checkReturn(retType, code.returned)

        self.retType = retType
        self.args = args
        self.code = code

    def __repr__(self):
        return 'Function(%r, %r, %r)' % (self.retType, self.args, self.code)

    def __str__(self):
        return '(%s) -> %s' % (
            ', '.join(
                '%s %s' % (decl, name)
                for name, decl in self.args.items()
                ),
            'unit' if self.retType is None else self.retType,
            )

    def dump(self):
        print(str(self))
        if self.code is None:
            print('    no code')
        else:
            self.code.dump()

def _checkArgs(declArgs, codeArgs):
    '''Check consistency between declared argument types and code block.
    Raises ValueError if an inconsistency is found.
    '''
    for name, arg in codeArgs.items():
        try:
            typ = declArgs[name]
        except KeyError:
            raise ValueError('code block uses undeclared argument "%s"' % name)

        if isinstance(typ, ReferenceType):
            if not isinstance(arg, RefArgStorage):
                raise ValueError(
                    'reference argument "%s" is not a reference in code block'
                    % name
                    )
            if typ.type.width != arg.width:
                raise ValueError(
                    'reference argument "%s" is declared with width %s but '
                    'has width %s in code block'
                    % (name, typ.type.width, arg.width)
                    )
        else:
            if not isinstance(arg, ArgumentValue):
                raise ValueError(
                    'value argument "%s" is not a value in code block'
                    % name
                    )

def _checkReturn(retType, returned):
    '''Check consistency between declared return type and code block.
    Raises ValueError if an inconsistency is found.
    '''
    if len(returned) > 1:
        raise ValueError('code block returns multiple values')

    if retType is None:
        if len(returned) != 0:
            raise ValueError(
                'function has no return type, but its code block defines "ret"'
                )
    elif isinstance(retType, ReferenceType):
        if len(returned) == 0:
            raise ValueError(
                'function has return type %s, but its code block does not '
                'define "ret"' % retType
                )
        retBits, = returned
        if retType.type.width != retBits.width:
            raise ValueError(
                'function has return type %s, but its code block defines "ret" '
                'with width %s'
                % (retType, retBits.width)
                )
    else: # returns a value
        if len(returned) == 0:
            raise ValueError(
                'function has return type %s, but its code block does not '
                'assign to "ret"' % retType
                )
        retBits, = returned
        if retType.width != retBits.width:
            raise ValueError(
                'function has return type %s, but its code block defines "ret" '
                'with width %s'
                % (retType, retBits.width)
                )
