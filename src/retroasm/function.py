from typing import Mapping, Optional, Sequence, Union

from .codeblock import CodeBlock
from .reference import BitString
from .storage import RefArgStorage, Storage, ValArgStorage
from .types import IntType, ReferenceType


class Function:

    def __init__(self,
                 retType: Union[None, IntType, ReferenceType],
                 args: Mapping[str, Union[IntType, ReferenceType]],
                 code: Optional[CodeBlock]
                 ):
        if code is not None:
            _checkArgs(args, code.arguments)
            _checkReturn(retType, code.returned)

        self.retType = retType
        self.args = args
        self.code = code

    def __repr__(self) -> str:
        return f'Function({self.retType!r}, {self.args!r}, {self.code!r})'

    def __str__(self) -> str:
        return '(%s) -> %s' % (
            ', '.join(
                f'{decl} {name}'
                for name, decl in self.args.items()
                ),
            'unit' if self.retType is None else self.retType,
            )

    def dump(self) -> None:
        print(str(self))
        if self.code is None:
            print('    no code')
        else:
            self.code.dump()

def _checkArgs(declArgs: Mapping[str, Union[IntType, ReferenceType]],
               codeArgs: Mapping[str, Storage]
               ) -> None:
    '''Check consistency between declared argument types and code block.
    Raises ValueError if an inconsistency is found.
    '''
    for name, arg in codeArgs.items():
        try:
            typ = declArgs[name]
        except KeyError:
            raise ValueError(f'code block uses undeclared argument "{name}"')

        if isinstance(typ, ReferenceType):
            if not isinstance(arg, RefArgStorage):
                raise ValueError(
                    'reference argument "%s" is not a reference in code block'
                    % name
                    )
            typ = typ.type
        else:
            if not isinstance(arg, ValArgStorage):
                raise ValueError(
                    f'value argument "{name}" is not a value in code block'
                    )
        if typ.width != arg.width:
            raise ValueError(
                'argument "%s" is declared with width %s but has width %s '
                'in code block'
                % (name, typ.width, arg.width)
                )

def _checkReturn(retType: Union[None, IntType, ReferenceType],
                 returned: Sequence[BitString]
                 ) -> None:
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
