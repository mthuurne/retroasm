from typing import Optional, Union

from .expression_parser import DeclarationNode, ParseNode
from .mode import Mode
from .types import IntType, ReferenceType, Width
from .utils import checkType


class PlaceholderSpec:

    @property
    def decl(self) -> DeclarationNode:
        return self._decl

    @property
    def name(self) -> str:
        return self._decl.name.name

    @property
    def encodingWidth(self) -> Optional[Width]:
        raise NotImplementedError

    @property
    def semanticsType(self) -> Union[None, IntType, ReferenceType]:
        raise NotImplementedError

    @property
    def value(self) -> Optional[ParseNode]:
        raise NotImplementedError

    def __init__(self, decl: DeclarationNode):
        self._decl = checkType(decl, DeclarationNode, 'placeholder declaration')

class ValuePlaceholderSpec(PlaceholderSpec):

    @property
    def encodingWidth(self) -> Width:
        return self._type.width

    @property
    def semanticsType(self) -> IntType:
        return self._type

    @property
    def value(self) -> Optional[ParseNode]:
        return self._value

    def __init__(self,
                 decl: DeclarationNode,
                 typ: IntType,
                 value: Optional[ParseNode]
                 ):
        PlaceholderSpec.__init__(self, decl)
        self._type = typ
        self._value = checkType(
            value, (type(None), ParseNode), 'placeholder value'
            )

    def __repr__(self) -> str:
        return 'ValuePlaceholderSpec(%r, %r, %r)' % (
            self._decl, self._type, self._value
            )

    def __str__(self) -> str:
        return '{%s %s}' % (self._type, self.name)

class MatchPlaceholderSpec(PlaceholderSpec):

    @property
    def encodingWidth(self) -> Optional[int]:
        return self._mode.encodingWidth

    @property
    def semanticsType(self) -> Union[None, IntType, ReferenceType]:
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
        return 'MatchPlaceholderSpec(%r, %r)' % (self._decl, self._mode)

    def __str__(self) -> str:
        return '{%s %s}' % (self._mode.name, self.name)
