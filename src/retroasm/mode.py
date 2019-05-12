from .analysis import CodeTemplate
from .codeblock import CodeBlock
from .codeblock_builder import SemanticsCodeBlockBuilder
from .codeblock_simplifier import CodeBlockSimplifier
from .expression import IntLiteral
from .expression_simplifier import simplifyExpression
from .linereader import mergeSpan
from .reference import FixedValue, Reference, SingleStorage, decodeInt
from .storage import ValArgStorage
from .types import IntType, unlimited
from .utils import checkType, const_property

from collections import OrderedDict
from enum import Enum

class EncodingExpr:
    '''A single element in an encoding sequence that is specified using an
    expression.
    '''

    bits = property(lambda self: self._bits)
    location = property(lambda self: self._location)

    encodingWidth = property(lambda self: self._bits.width)
    encodedLength = property(lambda self: 1)

    def __init__(self, bits, location):
        self._bits = bits
        self._location = location

    def __str__(self):
        return str(self._bits)

    def __repr__(self):
        return 'EncodingExpr(%r, %r)' % (self._bits, self._location)

    def fillPlaceholder(self, name, entry):
        '''Returns a new EncodingExpr, in which the match placeholder with the
        given name replaced by the first encoding element of the given mode
        entry.
        If no placeholder with the given name exists, this EncodingExpr is
        returned.
        '''
        def substPlaceholder(storage):
            if isinstance(storage, ValArgStorage) and storage.name == name:
                return entry.encoding[0]
        bits = self._bits
        newBits = bits.substitute(storageFunc=substPlaceholder)
        if bits is newBits:
            return self
        else:
            return EncodingExpr(newBits, self._location)

    def rename(self, nameMap):
        '''Returns a new EncodingExpr, with placeholder names substituted by
        their value in the given mapping.
        '''
        def renameValArg(storage):
            if isinstance(storage, ValArgStorage):
                return SingleStorage(
                    ValArgStorage(nameMap[storage.name], storage.width)
                    )
        return EncodingExpr(
            self._bits.substitute(storageFunc=renameValArg),
            self._location
            )

class EncodingMultiMatch:
    '''A segment in an encoding sequence of zero or more elements, that will
    be filled in by a matched entry from an included mode.
    '''

    name = property(lambda self: self._name)
    mode = property(lambda self: self._mode)
    start = property(lambda self: self._start)
    location = property(lambda self: self._location)

    encodingWidth = property(lambda self:
        self._mode.encodingWidth
        if self._start == 0 else
        self._mode.auxEncodingWidth
        )
    auxEncodingWidth = property(lambda self: self._mode.auxEncodingWidth)

    def __init__(self, name, mode, start, location):
        self._name = name
        self._mode = mode
        self._start = start
        self._location = location

    def __str__(self):
        return '%s@' % self._name

    def __repr__(self):
        return 'EncodingMultiMatch(%r, %r, %r, %r)' % (
            self._name, self._mode, self._start, self._location
            )

    def rename(self, nameMap):
        '''Returns a new EncodingMultiMatch, with the placeholder name
        substituted by its value in the given mapping.
        '''
        return EncodingMultiMatch(
            nameMap[self._name], self._mode, self._start, self._location
            )

    @const_property
    def encodedLength(self):
        length = self._mode.encodedLength
        return None if length is None else length - self._start

def _findFirstAuxIndex(encoding):
    '''Returns the index of the first encoding item that can match auxiliary
    encoding units, or None if no auxiliary encoding units can be matched.
    The given encoding sequence must not contain matchers that never match
    any encoding units.
    '''
    if len(encoding) == 0:
        # No units matched because there are no matchers.
        return None
    firstLen = encoding[0].encodedLength
    if firstLen >= 2:
        # First element can match multiple encoding units.
        return 0
    assert firstLen != 0, encoding
    if len(encoding) == 1:
        # First element matches 1 encoding unit, no second element.
        return None
    else:
        # The second element will match the second unit.
        assert encoding[1].encodedLength != 0, encoding
        return 1

class Encoding:
    '''Defines how (part of) an instruction is encoded.
    We call the elements of a definition 'items', these are represented by
    EncodingExpr and EncodingMultiMatch objects. We call the elements of an
    encoded instruction 'units', depending on the instruction set these are
    bytes or words or some other fixed-size bit strings.
    The items within an encoding definition are exposed as a sequence.
    '''

    def __init__(self, items, location):
        # Filter out zero-length encoding items.
        # There is no good reason to ban them, but keeping them around would
        # unnecessarily complicate the code.
        self._items = items = tuple(
            item
            for item in items
            if checkType(
                item, (EncodingExpr, EncodingMultiMatch), 'encoding element'
                ).encodedLength != 0
            )
        self._firstAuxIndex = firstAuxIndex = _findFirstAuxIndex(items)

        # Verify that all auxiliary units have the same width.
        auxWidth = self.auxEncodingWidth
        if auxWidth is not None:
            consistent = True
            for idx, item in enumerate(items):
                if idx != 0:
                    consistent &= item.encodingWidth == auxWidth
                if isinstance(item, EncodingMultiMatch):
                    consistent &= item.auxEncodingWidth in (None, auxWidth)
            if not consistent:
                raise ValueError(
                    'inconsistent widths among auxiliary encoding units'
                    )

        self._location = location

    def __iter__(self):
        return iter(self._items)

    def __len__(self):
        return len(self._items)

    def __getitem__(self, index):
        return self._items[index]

    def fillPlaceholder(self, name, entry):
        '''Returns a new Encoding, in which the match placeholder with the
        given name is replaced by the given mode entry.
        If no placeholder with the given name exists, this Encoding is returned.
        '''
        items = []
        changed = False
        for item in self._items:
            if isinstance(item, EncodingExpr):
                filledItem = item.fillPlaceholder(name, entry)
                items.append(filledItem)
                changed |= filledItem is not item
            elif isinstance(item, EncodingMultiMatch):
                if item.name == name:
                    items += entry.encoding[item.start:]
                    changed = True
                else:
                    items.append(item)
            else:
                assert False, item
        return Encoding(items, self._location) if changed else self

    def rename(self, nameMap):
        '''Returns a new Encoding, in which all placeholder names are
        substituted by their value in the given mapping.
        '''
        return Encoding(
            (item.rename(nameMap) for item in self._items),
            self._location
            )

    @property
    def encodingWidth(self):
        '''The width in bits a first encoding unit matched by this encoding
        definition would have, or None if this encoding definition always
        matches zero encoding units.
        '''
        items = self._items
        return None if len(items) == 0 else items[0].encodingWidth

    @property
    def encodingLocation(self):
        '''The InputLocation of the first item in this encoding definition.
        '''
        items = self._items
        return self._location if len(items) == 0 else items[0].location

    @property
    def auxEncodingWidth(self):
        '''The width in bits that all non-first encoding units matched by this
        encoding definition would have, or None if a match cannot contain more
        than one encoding unit.
        '''
        firstAuxIndex = self._firstAuxIndex
        if firstAuxIndex is None:
            return None
        elif firstAuxIndex == 0:
            return self._items[0].auxEncodingWidth
        else:
            assert firstAuxIndex == 1, firstAuxIndex
            return self._items[1].encodingWidth

    @property
    def auxEncodingLocation(self):
        '''The InputLocation of the auxiliary encoding items in this mode
        entry. If there are no auxiliary encoding items, the end of the
        encoding field is returned.
        '''
        items = self._items
        firstAuxIndex = self._firstAuxIndex
        if firstAuxIndex is None:
            location = self._location if len(items) == 0 else items[0].location
            return location.endLocation
        else:
            return mergeSpan(items[firstAuxIndex].location, items[-1].location)

    @const_property
    def encodedLength(self):
        '''The number of encoded units (bytes, words etc.) that this encoding
        definitions matches, or None if that number may vary depending on which
        match is made in an included mode.
        '''
        total = 0
        for item in self._items:
            length = item.encodedLength
            if length is None:
                return None
            total += length
        return total

class Mnemonic:
    '''Defines how (part of) an instruction is presented in assembly source
    code.
    The items within a mnemonic definition are exposed as a sequence.
    '''

    def __init__(self, items):
        self._items = tuple(
            checkType(item, (str, int, Placeholder), 'mnemonic item')
            for item in items
            )

    def __iter__(self):
        return iter(self._items)

    def __len__(self):
        return len(self._items)

    def __getitem__(self, index):
        return self._items[index]

    def fillPlaceholder(self, name, entry):
        '''Returns a new Mnemonic, in which the match placeholder of the given
        name is replaced by the given mode entry.
        If no placeholder with the given name exists, this Mnemonic is returned.
        '''
        items = []
        changed = False
        for item in self._items:
            if isinstance(item, MatchPlaceholder) and item.name == name:
                items += entry.mnemonic
                changed = True
            else:
                items.append(item)
        return Mnemonic(items) if changed else self

    def rename(self, nameMap):
        '''Returns a new Mnemonic, in which all placeholder names are
        substituted by their value in the given mapping.
        '''
        return Mnemonic(
            item.rename(nameMap[item.name])
                if isinstance(item, Placeholder) else item
            for item in self._items
            )

class ModeEntry:
    '''One row in a mode table.
    '''

    def __init__(self, encoding, mnemonic, semantics, placeholders):
        self.encoding = checkType(encoding, Encoding, 'encoding definition')
        self.mnemonic = checkType(mnemonic, Mnemonic, 'mnemonic definition')
        self.semantics = checkType(semantics, CodeTemplate, 'semantics')
        self.placeholders = checkType(
            placeholders, OrderedDict, 'placeholders definition'
            )

    def __repr__(self):
        return 'ModeEntry(%r, %r, %r, %r)' % (
            self.encoding, self.mnemonic, self.semantics, self.placeholders
            )

    def fillPlaceholder(self, name, entry):
        '''Returns a new entry, in which the match placeholder with the given
        name is replaced by the given mode entry.
        '''
        placeholders = self.placeholders.copy()
        placeholders.pop(name)
        # TODO: Implement merge.
        assert len(entry.placeholders) == 0, entry.placeholders

        return ModeEntry(
            self.encoding.fillPlaceholder(name, entry),
            self.mnemonic.fillPlaceholder(name, entry),
            self.semantics.fillPlaceholder(name, entry),
            placeholders
            )

    def rename(self, nameMap):
        '''Returns a new ModeEntry, in which all placeholder names are
        substituted by their value in the given mapping.
        '''
        def renamePlaceholders():
            for name, placeholder in self.placeholders.items():
                newName = nameMap[name]
                yield newName, placeholder.rename(newName)
        return ModeEntry(
            self.encoding.rename(nameMap),
            self.mnemonic.rename(nameMap),
            self.semantics.rename(nameMap),
            OrderedDict(renamePlaceholders())
            )

class ModeMatch:
    '''A flattened match of a mode entry at a particular address.
    Flattened means that all submode matches have been resolved and substituted
    into this match.
    '''
    __slots__ = (
        '_entry', '_values', '_subs', '_encoding', '_mnemonic', '_semantics'
        )

    @classmethod
    def fromEncodeMatch(cls, match, pcVal):
        '''Construct a ModeMatch using the data captured in an EncodeMatch.
        '''
        entry = match.entry
        placeholders = entry.placeholders
        pcBits = entry.semantics.pcBits

        builder = SemanticsCodeBlockBuilder()
        pcBits.emitStore(builder, pcVal, None)

        values = {}
        subs = {}
        for name, placeholder in placeholders.items():
            if isinstance(placeholder, MatchPlaceholder):
                subs[name] = cls.fromEncodeMatch(match[name], pcVal)
            elif isinstance(placeholder, ValuePlaceholder):
                typ = placeholder.type
                code = placeholder.code
                if code is None:
                    # Value was decoded.
                    value = IntLiteral(match[name])
                else:
                    # Value is computed.
                    returned = builder.inlineBlock(code, values.__getitem__)
                    matchCode = CodeBlockSimplifier(builder.nodes, returned)
                    matchCode.simplify()
                    valBits, = matchCode.returned
                    assert isinstance(valBits, FixedValue), valBits
                    valExpr = decodeInt(valBits.expr, typ)
                    value = simplifyExpression(valExpr)
                values[name] = FixedValue(value, typ.width)
            else:
                assert False, placeholder

        return cls(entry, values, subs)

    def __init__(self, entry, values, subs):
        self._entry = checkType(entry, ModeEntry, 'mode entry')
        self._values = values
        self._subs = subs

    def __repr__(self):
        return 'ModeMatch(%r, %r, %r)' % (self._entry, self._values, self._subs)

    @const_property
    def mnemonic(self):
        entry = self._entry
        subs = self._subs
        values = self._values

        for mnemElem in entry.mnemonic:
            if isinstance(mnemElem, str):
                yield mnemElem
            elif isinstance(mnemElem, int):
                yield Reference(
                    FixedValue(IntLiteral(mnemElem), unlimited),
                    IntType.int
                    )
            elif isinstance(mnemElem, MatchPlaceholder):
                yield from subs[mnemElem.name].mnemonic
            elif isinstance(mnemElem, ValuePlaceholder):
                yield Reference(values[mnemElem.name], mnemElem.type)
            else:
                assert False, mnemElem

def _formatEncodingWidth(width):
    return 'empty encoding' if width is None else 'encoding width %s' % width

def _formatAuxEncodingWidth(width):
    return (
        'no auxiliary encoding items'
        if width is None
        else 'auxiliary encoding width %s' % width
        )

class ModeTable:
    '''Abstract base class for mode tables.
    '''

    encodingWidth = property(lambda self: self._encWidth)
    auxEncodingWidth = property(lambda self: self._auxEncWidth)

    def __init__(self, encWidth, auxEncWidth, entries):
        if encWidth is unlimited or auxEncWidth is unlimited:
            raise ValueError('unlimited width is not allowed for encoding')
        self._encWidth = encWidth
        self._auxEncWidth = auxEncWidth
        self._entries = entries = tuple(entries)

        for entry in entries:
            assert isinstance(entry, ModeEntry), entry
            encDef = entry.encoding
            if encDef.encodingWidth != encWidth:
                raise ValueError(
                    'mode with %s contains entry with %s' % (
                        _formatEncodingWidth(encWidth),
                        _formatEncodingWidth(encDef.encodingWidth)
                        )
                    )
            if encDef.auxEncodingWidth not in (None, auxEncWidth):
                raise ValueError(
                    'mode with %s contains entry with %s' % (
                        _formatAuxEncodingWidth(auxEncWidth),
                        _formatAuxEncodingWidth(encDef.auxEncodingWidth)
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
                print('%s= %s' % (indent, tokens))
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

    @const_property
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
            entryLen = entry.encoding.encodedLength
            if entryLen is None:
                return None
            if entryLen != commonLen:
                if commonLen is None:
                    commonLen = entryLen
                else:
                    return None
        assert commonLen is not None, self
        return commonLen

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

PlaceholderRole = Enum('PlaceholderRole', ( # pylint: disable=invalid-name
    'code_addr', 'data_addr'
    ))

class Placeholder:
    '''Abstract base class for a mode context element.
    '''

    name = property(lambda self: self._name)

    def __init__(self, name):
        self._name = name

    def rename(self, name):
        '''Returns a new placeholder that is the same as this one, except
        the name is changed to the given name.
        '''
        raise NotImplementedError

class ValuePlaceholder(Placeholder):
    '''An element from a mode context that represents a numeric value.
    '''

    type = property(lambda self: self._type)
    code = property(lambda self: self._code)

    def __init__(self, name, typ, code):
        Placeholder.__init__(self, name)
        self._type = typ
        self._code = checkType(code, (CodeBlock, type(None)), 'code block')

    def __repr__(self):
        return 'ValuePlaceholder(%r, %r, %r)' % (
            self._name, self._type, self._code
            )

    def __str__(self):
        if self._code is None:
            return '{%s %s}' % (self._type, self._name)
        else:
            return '{%s %s = ...}' % (self._type, self._name)

    def rename(self, name):
        return ValuePlaceholder(name, self._type, self._code)

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

    def rename(self, name):
        return MatchPlaceholder(name, self._mode)
