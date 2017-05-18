from .expression import Expression

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

    def addEntry(self, encoding, mnemonic, semantics, context, flagsRequired):
        self._entries.append(
            (encoding, mnemonic, semantics, context, frozenset(flagsRequired))
            )

class Immediate(Expression):
    '''A constant value defined as part of an instruction.
    Note that the name of an immediate is unique only within the mode entry
    that declares, not in mode entries that include it.
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
