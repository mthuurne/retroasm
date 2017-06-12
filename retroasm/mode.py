from .expression import Expression, IntLiteral
from .expression_parser import DeclarationNode, ParseNode
from .expression_simplifier import simplifyExpression
from .linereader import mergeSpan
from .types import maskForWidth, unlimited
from .utils import checkType

class EncodingExpr:
    '''A single element in an encoding sequence that is specified using an
    expression.
    '''

    ref = property(lambda self: self._ref)
    value = property(lambda self: self._value)
    location = property(lambda self: self._location)

    width = property(lambda self: self._ref.width)
    encodedLength = property(lambda self: 1)

    def __init__(self, ref, value, location):
        self._ref = ref
        self._value = value
        self._location = location

class EncodingMultiMatch:
    '''A segment in an encoding sequence of zero or more elements, that will
    be filled in by a matched entry from an included mode.
    '''

    name = property(lambda self: self._name)
    mode = property(lambda self: self._mode)
    start = property(lambda self: self._start)
    location = property(lambda self: self._location)

    width = property(lambda self: self._mode.auxEncodingWidth)

    def __init__(self, name, mode, start, location):
        self._name = name
        self._mode = mode
        self._start = start
        self._location = location

    @property
    def encodedLength(self):
        length = self._mode.encodedLength
        return None if length is None else length - self._start

class ModeEntry:
    '''One row in a mode table.
    '''

    def __init__(
            self, encoding, decoding, mnemonic, semantics, placeholders,
            flagsRequired, location
            ):
        self.encoding = encoding = tuple(encoding)
        self.decoding = decoding
        self.mnemonic = mnemonic
        self.semantics = semantics
        self.placeholders = placeholders
        self.flagsRequired = frozenset(flagsRequired)
        self.location = location

        for encElem in encoding:
            checkType(
                encElem, (EncodingExpr, EncodingMultiMatch), 'encoding element'
                )
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

    @property
    def encodedLength(self):
        '''The number of encoded data units (bytes, words etc.) that this mode
        entry matches, or None if that number may vary depending on which match
        is made in an included mode.
        '''
        total = 0
        for encItem in self.encoding:
            length = encItem.encodedLength
            if length is None:
                return None
            total += length
        return total

    def tryDecode(self, encoded):
        '''Attempts to decode the given encoded value as an instance of this
        mode entry.
        Returns an EncodeMatch on success, or None on failure.
        '''
        assert len(self.encoding) == 1, 'not implemented yet'

        # Check literal bits.
        fixedMatcher = self.decoding[None]
        if len(fixedMatcher) != 0:
            encIdx, fixedMask, fixedValue = fixedMatcher[0]
            assert encIdx == 0, fixedMatcher
            if encoded & fixedMask != fixedValue:
                return None

        # Compose encoded immediates.
        encodedImmediates = {}
        for name, slices in self.decoding.items():
            if name is None:
                continue
            value = 0
            for encIdx, refIdx, width in slices:
                assert encIdx == 0, slices
                value <<= width
                value |= (encoded >> refIdx) & maskForWidth(width)
            encodedImmediates[name] = value

        # Find values for placeholders.
        placeholders = self.placeholders
        mapping = {}
        for name, immEnc in encodedImmediates.items():
            placeholder = placeholders[name]
            if isinstance(placeholder, MatchPlaceholder):
                decoded = placeholder.mode.tryDecode(immEnc)
                if decoded is None:
                    return None
            elif isinstance(placeholder, ValuePlaceholder):
                decoded = immEnc
            else:
                assert False, placeholder
            mapping[name] = decoded
        return EncodeMatch(self, mapping)

class EncodeMatch:
    '''A match on the encoding field of a mode entry.
    '''

    def __init__(self, entry, mapping):
        self._entry = entry
        self._mapping = mapping

    def _substMapping(self, expr):
        if isinstance(expr, Immediate):
            return IntLiteral(self._mapping[expr.name])
        else:
            return None

    def iterMnemonic(self):
        '''Yields a mnemonic representation of this match.
        '''
        entry = self._entry
        mapping = self._mapping
        placeholders = entry.placeholders
        for mnemElem in entry.mnemonic:
            if isinstance(mnemElem, str):
                yield mnemElem
            elif isinstance(mnemElem, int):
                yield '$%x' % mnemElem
            elif isinstance(mnemElem, MatchPlaceholder):
                match = mapping[mnemElem.name]
                yield from match.iterMnemonic()
            elif isinstance(mnemElem, ValuePlaceholder):
                name = mnemElem.name
                typ = mnemElem.type
                value = mapping.get(name)
                if value is None:
                    expr = simplifyExpression(
                        mnemElem.value.substitute(self._substMapping)
                        )
                    if isinstance(expr, IntLiteral):
                        yield expr.value, typ
                    else:
                        # TODO: Is this a bug? A definition error?
                        #       Or can it happen normally?
                        yield name
                else:
                    if typ.signed:
                        width = typ.width
                        if value >= 1 << (width - 1):
                            value -= 1 << width
                    yield value, typ
            else:
                assert False, mnemElem

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

    @property
    def encodedLength(self):
        '''The number of encoded data units (bytes, words etc.) that all
        entries in this mode use, or None if that number may vary depending
        on which match is made.
        '''
        if self._encWidth is None:
            return 0
        if self._auxEncWidth is None:
            return 1
        commonLen = None
        for entry in self._entries:
            entryLen = entry.encodedLength
            if entryLen is None:
                return None
            if entryLen != commonLen:
                if commonLen is None:
                    commonLen = entryLen
                else:
                    return None
        assert commonLen is not None, self
        return commonLen

    def tryDecode(self, encoded):
        '''Attempts to decode the given value as an entry in this table.
        Returns an EncodeMatch, or None if no match could be made.
        '''
        for entry in reversed(self._entries):
            match = entry.tryDecode(encoded)
            if match is not None:
                return match
        return None

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

    name = property(lambda self: self._name)

    def __init__(self, name):
        self._name = name

class ValuePlaceholder(Placeholder):
    '''An element from a mode context that represents a numeric value.
    '''

    type = property(lambda self: self._type)
    value = property(lambda self: self._value)

    def __init__(self, name, typ, value):
        Placeholder.__init__(self, name)
        self._type = typ
        self._value = None if value is None else simplifyExpression(value)

    def __repr__(self):
        return 'ValuePlaceholder(%r, %r, %r)' % (
            self._name, self._type, self._value
            )

    def __str__(self):
        value = self._value
        if value is None:
            return '{%s %s}' % (self._type, self._name)
        else:
            return '{%s %s = %s}' % (self._type, self._name, self._value)

class MatchPlaceholder(Placeholder):
    '''An element from a mode context that will be filled in by a match made
    in a different mode table.
    '''

    mode = property(lambda self: self._mode)

    def __init__(self, name, mode):
        Placeholder.__init__(self, name)
        self._mode = mode

    def __repr__(self):
        return 'MatchPlaceholder(%r, %r)' % (self._name, self._mode)

    def __str__(self):
        return '{%s %s}' % (self._mode.name, self._name)

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
