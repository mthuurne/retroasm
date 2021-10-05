from __future__ import annotations

from typing import Mapping, Sequence

from .codeblock import CodeBlock
from .reference import BitString
from .storage import Storage
from .types import IntType, ReferenceType


class Function:
    def __init__(
        self,
        retType: None | IntType | ReferenceType,
        args: Mapping[str, IntType | ReferenceType],
        code: CodeBlock | None,
    ):
        if code is not None:
            _checkArgs(args, code.arguments)
            _checkReturn(retType, code.returned)

        self.retType = retType
        self.args = args
        self.code = code

    def __repr__(self) -> str:
        return f"Function({self.retType!r}, {self.args!r}, {self.code!r})"

    def __str__(self) -> str:
        return "(%s) -> %s" % (
            ", ".join(f"{decl} {name}" for name, decl in self.args.items()),
            "unit" if self.retType is None else self.retType,
        )

    def dump(self) -> None:
        print(str(self))
        if self.code is None:
            print("    no code")
        else:
            self.code.dump()


def _checkArgs(
    declArgs: Mapping[str, IntType | ReferenceType], codeArgs: Mapping[str, Storage]
) -> None:
    """Check consistency between declared argument types and code block.
    Raises ValueError if an inconsistency is found.
    """
    for name, arg in codeArgs.items():
        try:
            typ = declArgs[name]
        except KeyError:
            raise ValueError(f'code block uses undeclared argument "{name}"')

        if isinstance(typ, ReferenceType):
            typ = typ.type
        if typ.width != arg.width:
            raise ValueError(
                f'argument "{name}" is declared with width {typ.width} '
                f"but has width {arg.width} in code block"
            )


def _checkReturn(
    retType: None | IntType | ReferenceType, returned: Sequence[BitString]
) -> None:
    """Check consistency between declared return type and code block.
    Raises ValueError if an inconsistency is found.
    """
    if len(returned) > 1:
        raise ValueError("code block returns multiple values")

    if retType is None:
        if len(returned) != 0:
            raise ValueError(
                'function has no return type, but its code block defines "ret"'
            )
    elif isinstance(retType, ReferenceType):
        if len(returned) == 0:
            raise ValueError(
                f"function has return type {retType}, but its code block "
                f'does not define "ret"'
            )
        (retBits,) = returned
        if retType.type.width != retBits.width:
            raise ValueError(
                f"function has return type {retType}, but its code block "
                f'defines "ret" with width {retBits.width}'
            )
    else:  # returns a value
        if len(returned) == 0:
            raise ValueError(
                f"function has return type {retType}, but its code block "
                f'does not assign to "ret"'
            )
        (retBits,) = returned
        if retType.width != retBits.width:
            raise ValueError(
                f"function has return type {retType}, but its code block "
                f'defines "ret" with width {retBits.width}'
            )
