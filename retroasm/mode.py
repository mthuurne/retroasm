from .expression import Expression
from .expression_parser import DeclarationNode, ParseNode
from .utils import checkType

class Mode:
    '''A pattern for operands, such as an addressing mode or a table defining
    register encoding.
    '''

    name = property(lambda self: self._name)
    encodingType = property(lambda self: self._encType)
    semanticsType = property(lambda self: self._semType)
    location = property(lambda self: self._location)

    def __init__(self, name, semType, location):
        self._name = name
        self._semType = semType
        self._location = location
        self._encType = None
        self._entries = []

    def __str__(self):
        return 'mode %s %s' % (self._semType, self._name)

    def __iter__(self):
        return iter(self._entries)

    @encodingType.setter
    def encodingType(self, encType):
        assert self._encType is None, self._encType
        self._encType = encType

    def addEntry(self, entry):
        assert isinstance(entry, ModeEntry), entry
        self._entries.append(entry)

class ModeEntry:

    def __init__(self, encoding, mnemonic, semantics, context, flagsRequired):
        self.encoding = encoding
        self.mnemonic = mnemonic
        self.semantics = semantics
        self.context = context
        self.flagsRequired = frozenset(flagsRequired)

class Placeholder:
    '''Abstract base class for a mode context element.
    '''

    decl = property(lambda self: self._decl)
    name = property(lambda self: self._decl.name.name)

    encodingType = property()
    semanticsType = property()
    value = property()

    def __init__(self, decl):
        self._decl = checkType(decl, DeclarationNode, 'placeholder declaration')

class ValuePlaceholder(Placeholder):
    '''An element from a mode context that represents a numeric value.
    The value will be a ParseNode, or None if no value was given in the context.
    '''

    encodingType = property(lambda self: self._type)
    semanticsType = property(lambda self: self._type)
    value = property(lambda self: self._value)

    def __init__(self, decl, typ, value):
        Placeholder.__init__(self, decl)
        self._type = typ
        self._value = checkType(
            value, (type(None), ParseNode), 'placeholder value'
            )

    def __repr__(self):
        return 'ValuePlaceholder(%r, %r, %r)' % (
            self._decl, self._type, self._value
            )

    def __str__(self):
        return '{%s %s}' % (self._type, self.name)

class MatchPlaceholder(Placeholder):
    '''An element from a mode context that will be filled in by a match made
    in a different mode table.
    '''

    encodingType = property(lambda self: self._mode.encodingType)
    semanticsType = property(lambda self: self._mode.semanticsType)
    value = property(lambda self: None)

    mode = property(lambda self: self._mode)

    def __init__(self, decl, mode):
        Placeholder.__init__(self, decl)
        self._mode = mode

    def __repr__(self):
        return 'MatchPlaceholder(%r, %r)' % (self._decl, self._mode)

    def __str__(self):
        return '{%s %s}' % (self._mode.name, self.name)

class Immediate(Expression):
    '''A constant value defined as part of an instruction.
    Note that the name of an immediate is unique only within the mode entry
    that declares it, not in mode entries that include it.
    '''
    __slots__ = ('_name', '_type', '_location')

    name = property(lambda self: self._name)
    type = property(lambda self: self._type)
    width = property(lambda self: self._type.width)
    mask = property(lambda self: self._type.mask)
    location = property(lambda self: self._location)

    def __init__(self, name, typ, location):
        Expression.__init__(self)
        self._name = name
        self._type = typ
        self._location = location

    def _ctorargs(self):
        return self._name, self._type, self._location

    def __str__(self):
        return self._name

    def _equals(self, other):
        # pylint: disable=protected-access
        return self._name == other._name
