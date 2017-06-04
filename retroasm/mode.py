from .expression import Expression
from .expression_parser import DeclarationNode, ParseNode
from .linereader import mergeSpan
from .types import unlimited
from .utils import checkType

class EncodingExpr:
    '''A single element in an encoding sequence that is specified using an
    expression.
    '''

    ref = property(lambda self: self._ref)
    value = property(lambda self: self._value)
    location = property(lambda self: self._location)

    width = property(lambda self: self._ref.width)

    def __init__(self, ref, value, location):
        self._ref = ref
        self._value = value
        self._location = location

class EncodingMultiMatch:
    '''A segment in an encoding sequence of zero or more elements, that will
    be filled in by a matched entry from an included mode.
    '''

    mode = property(lambda self: self._mode)
    start = property(lambda self: self._start)
    location = property(lambda self: self._location)

    width = property(lambda self: self._mode.auxEncodingWidth)

    def __init__(self, mode, start, location):
        self._mode = mode
        self._start = start
        self._location = location

class ModeEntry:
    '''One row in a mode table.
    '''

    def __init__(
            self, encoding, decoding, mnemonic, semantics, context,
            flagsRequired, location
            ):
        self.encoding = encoding = tuple(encoding)
        self.decoding = decoding
        self.mnemonic = mnemonic
        self.semantics = semantics
        self.context = context
        self.flagsRequired = frozenset(flagsRequired)
        self.location = location

        for encElem in encoding:
            checkType(encElem, EncodingExpr, 'encoding element')
        auxWidth = self.auxEncodingWidth
        if auxWidth is not None:
            if any(encElem.width != auxWidth for encElem in encoding[1:]):
                raise ValueError(
                    'Inconsistent widths among auxiliary encoding elements'
                    )

    @property
    def encodingWidth(self):
        '''The width in bits of the first encoding element in this mode entry,
        or None if this entry contains an empty encoding sequence.
        '''
        encoding = self.encoding
        return None if len(encoding) == 0 else encoding[0].width

    @property
    def encodingLocation(self):
        '''The InputLocation of the first encoding element in this mode entry.
        '''
        encoding = self.encoding
        if len(encoding) == 0:
            return self.location.updateSpan((0, 1))
        else:
            return encoding[0].location

    @property
    def auxEncodingWidth(self):
        '''The width in bits of the non-first encoding elements in this mode
        entry, or None if there is no or one encoding element.
        '''
        encoding = self.encoding
        return None if len(encoding) < 2 else encoding[1].width

    @property
    def auxEncodingLocation(self):
        '''The InputLocation of the auxiliary encoding elements in this mode
        entry.
        '''
        encoding = self.encoding
        if len(encoding) == 0:
            return self.location.updateSpan((0, 1))
        elif len(encoding) == 1:
            firstEnd = encoding[0].location.span[1]
            return self.location.updateSpan((firstEnd, firstEnd + 1))
        else:
            return mergeSpan(encoding[1].location, encoding[-1].location)

def _formatEncodingWidth(width):
    return 'empty encoding' if width is None else 'encoding width %d' % width

def _formatAuxEncodingWidth(width):
    return (
        'no auxiliary encoding items'
        if width is None
        else 'auxiliary encoding width %d' % width
        )

class ModeTable:
    '''Abstract base class for mode tables.
    '''

    encodingWidth = property(lambda self: self._encWidth)
    auxEncodingWidth = property(lambda self: self._auxEncWidth)

    def __init__(self, encWidth, auxEncWidth, entries):
        if encWidth is unlimited or auxEncWidth is unlimited:
            raise ValueError('Unlimited width is not allowed for encoding')
        self._encWidth = encWidth
        self._auxEncWidth = auxEncWidth

        self._entries = entries = tuple(entries)
        for entry in entries:
            assert isinstance(entry, ModeEntry), entry
            if entry.encodingWidth != encWidth:
                raise ValueError(
                    'Mode with %s contains entry with %s' % (
                        _formatEncodingWidth(encWidth),
                        _formatEncodingWidth(entry.encodingWidth)
                        )
                    )
            if entry.auxEncodingWidth not in (None, auxEncWidth):
                raise ValueError(
                    'Mode with %s contains entry with %s' % (
                        _formatAuxEncodingWidth(auxEncWidth),
                        _formatAuxEncodingWidth(entry.auxEncodingWidth)
                        )
                    )

        self._mnemTree = ({}, [])
        for entry in entries:
            self._updateMnemTree(entry)

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
    semanticsType = property(lambda self: self._semType)
    location = property(lambda self: self._location)

    def __init__(self, name, encWidth, auxEncWidth, semType, location, entries):
        ModeTable.__init__(self, encWidth, auxEncWidth, entries)
        self._name = name
        self._semType = semType
        self._location = location

    def __str__(self):
        return 'mode %s %s' % (self._semType, self._name)

    def __iter__(self):
        return iter(self._entries)

class Placeholder:
    '''Abstract base class for a mode context element.
    '''

    decl = property(lambda self: self._decl)
    name = property(lambda self: self._decl.name.name)

    encodingWidth = property()
    semanticsType = property()
    value = property()

    def __init__(self, decl):
        self._decl = checkType(decl, DeclarationNode, 'placeholder declaration')

class ValuePlaceholder(Placeholder):
    '''An element from a mode context that represents a numeric value.
    The value will be a ParseNode, or None if no value was given in the context.
    '''

    encodingWidth = property(lambda self: self._type.width)
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

    encodingWidth = property(lambda self: self._mode.encodingWidth)
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
