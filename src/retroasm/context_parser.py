from .expression_parser import DeclarationNode, ParseNode
from .utils import checkType


class PlaceholderSpec:

    decl = property(lambda self: self._decl)
    name = property(lambda self: self._decl.name.name)

    encodingWidth = property()
    semanticsType = property()
    value = property()

    def __init__(self, decl):
        self._decl = checkType(decl, DeclarationNode, 'placeholder declaration')

class ValuePlaceholderSpec(PlaceholderSpec):

    encodingWidth = property(lambda self: self._type.width)
    semanticsType = property(lambda self: self._type)
    value = property(lambda self: self._value)

    def __init__(self, decl, typ, value):
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

    encodingWidth = property(lambda self: self._mode.encodingWidth)
    semanticsType = property(lambda self: self._mode.semanticsType)
    value = property(lambda self: None)

    mode = property(lambda self: self._mode)

    def __init__(self, decl, mode):
        PlaceholderSpec.__init__(self, decl)
        self._mode = mode

    def __repr__(self) -> str:
        return 'MatchPlaceholderSpec(%r, %r)' % (self._decl, self._mode)

    def __str__(self) -> str:
        return '{%s %s}' % (self._mode.name, self.name)
