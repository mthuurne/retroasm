from .expression import Expression
from .expression_parser import DeclarationNode, ParseNode
from .utils import checkType

class ModeEntry:
    '''One row in a mode table.
    '''

    def __init__(self, encoding, mnemonic, semantics, context, flagsRequired):
        self.encoding = encoding
        self.mnemonic = mnemonic
        self.semantics = semantics
        self.context = context
        self.flagsRequired = frozenset(flagsRequired)

    @property
    def encodingType(self):
        '''The type of the first encoding element in this mode entry.
        '''
        return self.encoding[0][0].type

class ModeTable:
    '''Abstract base class for mode tables.
    '''

    def __init__(self):
        self._entries = []
        self._mnemTree = ({}, [])

    def dumpMnemonicTree(self):
        def matchKey(match):
            if isinstance(match, str):
                return 0, match
            elif isinstance(match, Mode):
                return 1, match.name
            elif match is int:
                return 2, None
            else:
                assert False, match
        def dumpNode(node, indent):
            for entry in node[1]:
                tokens = ' '.join(str(token) for token in entry.mnemonic)
                if entry.flagsRequired:
                    flags = ' -- prefix ' + ' '.join(
                        sorted(entry.flagsRequired)
                        )
                else:
                    flags = ''
                print('%s= %s%s' % (indent, tokens, flags))
            for match in sorted(node[0].keys(), key=matchKey):
                print('%s+ %s' % (indent, match))
                dumpNode(node[0][match], ' ' * len(indent) + '`---')
        dumpNode(self._mnemTree, '')

    def addEntry(self, entry):
        assert isinstance(entry, ModeEntry), entry
        self._entries.append(entry)
        self._updateMnemTree(entry)

    def _updateMnemTree(self, entry):
        # Update match tree for mnemonics.
        mnemonic = entry.mnemonic
        def addMnemonic(node, idx):
            if idx == len(mnemonic):
                node[1].append(entry)
            else:
                token = mnemonic[idx]
                if isinstance(token, str):
                    match = token
                elif isinstance(token, (int, ValuePlaceholder)):
                    match = int
                elif isinstance(token, MatchPlaceholder):
                    match = token.mode
                else:
                    assert False, token
                child = node[0].setdefault(match, ({}, []))
                addMnemonic(child, idx + 1)
        addMnemonic(self._mnemTree, 0)

class Mode(ModeTable):
    '''A pattern for operands, such as an addressing mode or a table defining
    register encoding.
    '''

    name = property(lambda self: self._name)
    encodingType = property(lambda self: self._encType)
    semanticsType = property(lambda self: self._semType)
    location = property(lambda self: self._location)

    def __init__(self, name, encType, semType, location, entries):
        ModeTable.__init__(self)
        self._name = name
        self._encType = encType
        self._semType = semType
        self._location = location
        self._entries = entries = tuple(entries)

        for entry in entries:
            if entry.encodingType is not encType:
                raise ValueError(
                    'Mode with encoding type %s contains entry with encoding '
                    'type %s' % (encType, entry.encodingType)
                    )
            self._updateMnemTree(entry)

    def __str__(self):
        return 'mode %s %s' % (self._semType, self._name)

    def __iter__(self):
        return iter(self._entries)

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
