from __future__ import annotations

from .expression_parser import DeclarationNode, ParseNode
from .mode import Mode
from .types import IntType, ReferenceType, Width


class PlaceholderSpec:

    @property
    def decl(self) -> DeclarationNode:
        return self._decl

    @property
    def name(self) -> str:
        return self._decl.name.name

    @property
    def encodingWidth(self) -> Width | None:
        raise NotImplementedError

    @property
    def semanticsType(self) -> None | IntType | ReferenceType:
        raise NotImplementedError

    @property
    def value(self) -> ParseNode | None:
        raise NotImplementedError

    def __init__(self, decl: DeclarationNode):
        self._decl = decl

class ValuePlaceholderSpec(PlaceholderSpec):

    @property
    def encodingWidth(self) -> Width:
        return self._type.width

    @property
    def semanticsType(self) -> IntType:
        return self._type

    @property
    def value(self) -> ParseNode | None:
        return self._value

    def __init__(self,
                 decl: DeclarationNode,
                 typ: IntType,
                 value: ParseNode | None
                 ):
        PlaceholderSpec.__init__(self, decl)
        self._type = typ
        self._value = value

    def __repr__(self) -> str:
        return f'ValuePlaceholderSpec({self._decl!r}, {self._type!r}, ' \
                                    f'{self._value!r})'

    def __str__(self) -> str:
        return f'{{{self._type} {self.name}}}'

class MatchPlaceholderSpec(PlaceholderSpec):

    @property
    def encodingWidth(self) -> int | None:
        return self._mode.encodingWidth

    @property
    def semanticsType(self) -> None | IntType | ReferenceType:
        return self._mode.semanticsType

    @property
    def value(self) -> None:
        return None

    @property
    def mode(self) -> Mode:
        return self._mode

    def __init__(self, decl: DeclarationNode, mode: Mode):
        PlaceholderSpec.__init__(self, decl)
        self._mode = mode

    def __repr__(self) -> str:
        return f'MatchPlaceholderSpec({self._decl!r}, {self._mode!r})'

    def __str__(self) -> str:
        return f'{{{self._mode.name} {self.name}}}'
