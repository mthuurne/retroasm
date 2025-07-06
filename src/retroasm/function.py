from __future__ import annotations

from collections.abc import Mapping, Sequence
from typing import override

from .codeblock import FunctionBody
from .reference import BitString
from .storage import Storage
from .types import IntType, ReferenceType


class Function:
    def __init__(
        self,
        ret_type: None | IntType | ReferenceType,
        args: Mapping[str, IntType | ReferenceType],
        code: FunctionBody | None,
    ):
        if code is not None:
            _check_args(args, code.arguments)
            _check_return(ret_type, code.returned)

        self.ret_type = ret_type
        self.args = args
        self.code = code

    @override
    def __repr__(self) -> str:
        return f"Function({self.ret_type!r}, {self.args!r}, {self.code!r})"

    @override
    def __str__(self) -> str:
        args = ", ".join(f"{decl} {name}" for name, decl in self.args.items())
        ret = "unit" if self.ret_type is None else self.ret_type
        return f"({args}) -> {ret}"

    def dump(self) -> None:
        print(str(self))
        if self.code is None:
            print("    no code")
        else:
            self.code.dump()


def _check_args(
    decl_args: Mapping[str, IntType | ReferenceType], code_args: Mapping[str, Storage]
) -> None:
    """
    Check consistency between declared argument types and code block.
    Raises ValueError if an inconsistency is found.
    """
    for name, arg in code_args.items():
        try:
            typ = decl_args[name]
        except KeyError:
            raise ValueError(f'code block uses undeclared argument "{name}"') from None

        if isinstance(typ, ReferenceType):
            typ = typ.type
        if typ.width != arg.width:
            raise ValueError(
                f'argument "{name}" is declared with width {typ.width} '
                f"but has width {arg.width} in code block"
            )


def _check_return(
    ret_type: None | IntType | ReferenceType, returned: Sequence[BitString]
) -> None:
    """
    Check consistency between declared return type and code block.
    Raises ValueError if an inconsistency is found.
    """
    if len(returned) > 1:
        raise ValueError("code block returns multiple values")

    if ret_type is None:
        if len(returned) != 0:
            raise ValueError('function has no return type, but its code block defines "ret"')
    elif isinstance(ret_type, ReferenceType):
        if len(returned) == 0:
            raise ValueError(
                f'function has return type {ret_type}, but its code block does not define "ret"'
            )
        (ret_bits,) = returned
        if ret_type.type.width != ret_bits.width:
            raise ValueError(
                f"function has return type {ret_type}, but its code block "
                f'defines "ret" with width {ret_bits.width}'
            )
    else:  # returns a value
        if len(returned) == 0:
            raise ValueError(
                f"function has return type {ret_type}, but its code block "
                f'does not assign to "ret"'
            )
        (ret_bits,) = returned
        if ret_type.width != ret_bits.width:
            raise ValueError(
                f"function has return type {ret_type}, but its code block "
                f'defines "ret" with width {ret_bits.width}'
            )
