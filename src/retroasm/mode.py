from __future__ import annotations

from collections import OrderedDict
from enum import Enum
from typing import (
    Any, Callable, Dict, Iterable, Iterator, List, Mapping, Optional, Sequence,
    Tuple, Type, Union, cast, overload
)

from .codeblock import CodeBlock
from .codeblock_builder import SemanticsCodeBlockBuilder
from .codeblock_simplifier import CodeBlockSimplifier
from .expression import Expression, IntLiteral
from .expression_simplifier import simplifyExpression
from .linereader import InputLocation, mergeSpan
from .reference import (
    BitString, FixedValue, Reference, SingleStorage, decodeInt
)
from .storage import ArgStorage, Storage
from .types import IntType, ReferenceType, unlimited
from .utils import const_property


class EncodingExpr:
    '''A single element in an encoding sequence that is specified using an
    expression.
    '''

    @property
    def bits(self) -> BitString:
        return self._bits

    @property
    def location(self) -> InputLocation:
        return self._location

    @property
    def encodingWidth(self) -> int:
        width = self._bits.width
        # TODO: Is it indeed impossible to create an unlimited-width
        #       EncodingExpr?
        assert isinstance(width, int)
        return width

    @property
    def encodedLength(self) -> int:
        return 1

    def __init__(self, bits: BitString, location: InputLocation):
        self._bits = bits
        self._location = location

    def __str__(self) -> str:
        return str(self._bits)

    def __repr__(self) -> str:
        return f'EncodingExpr({self._bits!r}, {self._location!r})'

    def substitute(self,
                   func: Callable[[str], Optional[BitString]],
                   ) -> EncodingExpr:
        '''Apply the given substitution function to each placeholder.
        The function is passed a placeholder name and should either return
        a bit string containing the value for that placeholder, or None
        to preserve the placeholder.
        '''

        def substPlaceholder(storage: Storage) -> Optional[BitString]:
            if isinstance(storage, ArgStorage):
                return func(storage.name)
            else:
                return None

        bits = self._bits
        newBits = bits.substitute(storageFunc=substPlaceholder)
        if bits is newBits:
            return self
        else:
            return EncodingExpr(newBits, self._location)

    def rename(self, nameMap: Mapping[str, str]) -> EncodingExpr:
        '''Returns a new EncodingExpr, with placeholder names substituted by
        their value in the given mapping.
        '''
        def renameValArg(storage: Storage) -> Optional[SingleStorage]:
            if isinstance(storage, ArgStorage):
                return SingleStorage(
                    ArgStorage(nameMap[storage.name], storage.width)
                    )
            else:
                return None
        return EncodingExpr(
            self._bits.substitute(storageFunc=renameValArg),
            self._location
            )

class EncodingMultiMatch:
    '''A segment in an encoding sequence of zero or more elements, that will
    be filled in by a matched entry from an included mode.
    '''

    @property
    def name(self) -> str:
        return self._name

    @property
    def mode(self) -> Mode:
        return self._mode

    @property
    def start(self) -> int:
        return self._start

    @property
    def location(self) -> InputLocation:
        return self._location

    @property
    def encodingWidth(self) -> Optional[int]:
        if self._start == 0:
            return self._mode.encodingWidth
        else:
            return self._mode.auxEncodingWidth

    @property
    def auxEncodingWidth(self) -> Optional[int]:
        return self._mode.auxEncodingWidth

    def __init__(self,
                 name: str,
                 mode: Mode,
                 start: int,
                 location: InputLocation
                 ):
        self._name = name
        self._mode = mode
        self._start = start
        self._location = location

    def __str__(self) -> str:
        return f'{self._name}@'

    def __repr__(self) -> str:
        return f'EncodingMultiMatch({self._name!r}, {self._mode!r}, ' \
                                 f' {self._start!r}, {self._location!r})'

    def rename(self, nameMap: Mapping[str, str]) -> EncodingMultiMatch:
        '''Returns a new EncodingMultiMatch, with the placeholder name
        substituted by its value in the given mapping.
        '''
        return EncodingMultiMatch(
            nameMap[self._name], self._mode, self._start, self._location
            )

    @const_property
    def encodedLength(self) -> Optional[int]:
        length = self._mode.encodedLength
        return None if length is None else length - self._start

def _findFirstAuxIndex(encoding: Sequence[EncodingItem]) -> Optional[int]:
    '''Returns the index of the first encoding item that can match auxiliary
    encoding units, or None if no auxiliary encoding units can be matched.
    The given encoding sequence must not contain matchers that never match
    any encoding units.
    '''
    if len(encoding) == 0:
        # No units matched because there are no matchers.
        return None
    firstLen = encoding[0].encodedLength
    assert firstLen is not None, encoding
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

EncodingItem = Union[EncodingExpr, EncodingMultiMatch]

class Encoding:
    '''Defines how (part of) an instruction is encoded.
    We call the elements of a definition 'items', these are represented by
    EncodingExpr and EncodingMultiMatch objects. We call the elements of an
    encoded instruction 'units', depending on the instruction set these are
    bytes or words or some other fixed-size bit strings.
    The items within an encoding definition are exposed as a sequence.
    '''

    def __init__(self,
                 items: Iterable[EncodingItem],
                 location: InputLocation
                 ):
        # Filter out zero-length encoding items.
        # There is no good reason to ban them, but keeping them around would
        # unnecessarily complicate the code.
        nonEmptyItems: Sequence[EncodingItem] = tuple(
            item for item in items if item.encodedLength != 0
            )
        self._items = nonEmptyItems
        self._firstAuxIndex = firstAuxIndex = _findFirstAuxIndex(nonEmptyItems)

        # Verify that all auxiliary units have the same width.
        auxWidth = self.auxEncodingWidth
        if auxWidth is not None:
            consistent = True
            for idx, item in enumerate(nonEmptyItems):
                if idx != 0:
                    consistent &= item.encodingWidth == auxWidth
                if isinstance(item, EncodingMultiMatch):
                    consistent &= item.auxEncodingWidth in (None, auxWidth)
            if not consistent:
                raise ValueError(
                    'inconsistent widths among auxiliary encoding units'
                    )

        self._location = location

    def __iter__(self) -> Iterator[EncodingItem]:
        return iter(self._items)

    def __len__(self) -> int:
        return len(self._items)

    @overload
    def __getitem__(self, index: int) -> EncodingItem: ...

    @overload
    def __getitem__(self, index: slice) -> Sequence[EncodingItem]: ...

    def __getitem__(self, index: Union[int, slice]) -> Any:
        return self._items[index]

    def fillPlaceholders(self, match: EncodeMatch) -> Encoding:
        '''Return a new encoding, in which placeholders are replaced by
        match results, if available.
        '''

        # In the case of multi-matches, we might need a filled submode encoding
        # multiple times, so cache them.
        subEncodings: Dict[str, Encoding] = {}
        def getSubEncoding(name: str, subMatch: EncodeMatch) -> Encoding:
            try:
                return subEncodings[name]
            except KeyError:
                subEnc = subMatch.entry.encoding.fillPlaceholders(subMatch)
                subEncodings[name] = subEnc
                return subEnc

        def substPlaceholder(name: str) -> Optional[BitString]:
            try:
                value = match[name]
            except KeyError:
                return None
            if isinstance(value, EncodeMatch):
                # We're called to substitute into an EncodingExpr and those
                # always match the first encoding item of the submode.
                firstItem = getSubEncoding(name, value)[0]
                if isinstance(firstItem, EncodingExpr):
                    return firstItem.bits
                elif isinstance(firstItem, EncodingMultiMatch):
                    # TODO: Add support.
                    #       I think this will happen in practice, for example
                    #       when the entire encoding field is one multi-match
                    #       (full delegation to submode, possibly selected by
                    #       decode flag).
                    #       Note that this can only happen when the submode
                    #       still has unresolved match placeholders, so it
                    #       will only break in the case of partial fills,
                    #       which we don't use yet.
                    assert False, firstItem
                else:
                    assert False, firstItem
            elif isinstance(value, int):
                assert False, value
            else:
                assert False, value

        items: List[EncodingItem] = []
        for item in self._items:
            if isinstance(item, EncodingExpr):
                items.append(item.substitute(substPlaceholder))
            elif isinstance(item, EncodingMultiMatch):
                name = item.name
                try:
                    subMatch = cast(EncodeMatch, match[name])
                except KeyError:
                    items.append(item)
                else:
                    items += getSubEncoding(name, subMatch)[item.start:]
            else:
                assert False, item
        return Encoding(items, self._location)

    def rename(self, nameMap: Mapping[str, str]) -> Encoding:
        '''Returns a new Encoding, in which all placeholder names are
        substituted by their value in the given mapping.
        '''
        return Encoding(
            (item.rename(nameMap) for item in self._items),
            self._location
            )

    @property
    def encodingWidth(self) -> Optional[int]:
        '''The width in bits a first encoding unit matched by this encoding
        definition would have, or None if this encoding definition always
        matches zero encoding units.
        '''
        items = self._items
        return None if len(items) == 0 else items[0].encodingWidth

    @property
    def encodingLocation(self) -> InputLocation:
        '''The InputLocation of the first item in this encoding definition.
        '''
        items = self._items
        return self._location if len(items) == 0 else items[0].location

    @property
    def auxEncodingWidth(self) -> Optional[int]:
        '''The width in bits that all non-first encoding units matched by this
        encoding definition would have, or None if a match cannot contain more
        than one encoding unit.
        '''
        firstAuxIndex = self._firstAuxIndex
        if firstAuxIndex is None:
            return None
        elif firstAuxIndex == 0:
            item = self._items[0]
            assert isinstance(item, EncodingMultiMatch), item
            return item.auxEncodingWidth
        else:
            assert firstAuxIndex == 1, firstAuxIndex
            return self._items[1].encodingWidth

    @property
    def auxEncodingLocation(self) -> InputLocation:
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
    def encodedLength(self) -> Optional[int]:
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

MnemItem = Union[str, int, 'Placeholder']

class Mnemonic:
    '''Defines how (part of) an instruction is presented in assembly source
    code.
    The items within a mnemonic definition are exposed as a sequence.
    '''

    def __init__(self, items: Iterable[MnemItem]):
        self._items: Sequence[MnemItem] = tuple(items)

    def __iter__(self) -> Iterator[MnemItem]:
        return iter(self._items)

    def __len__(self) -> int:
        return len(self._items)

    def __getitem__(self, index: int) -> MnemItem:
        return self._items[index]

    def fillPlaceholders(self, match: EncodeMatch) -> Mnemonic:
        '''Return a new mnemonic, in which placeholders are replaced by
        match results, if available.
        '''
        items: List[MnemItem] = []
        for item in self._items:
            if isinstance(item, MatchPlaceholder):
                # Submode match.
                try:
                    subMatch = cast(EncodeMatch, match[item.name])
                except KeyError:
                    items.append(item)
                else:
                    items += subMatch.entry.mnemonic.fillPlaceholders(subMatch)
            elif isinstance(item, ValuePlaceholder):
                # Immediate value.
                try:
                    value = cast(int, match[item.name])
                except KeyError:
                    # TODO: Apply substitutions inside computed values.
                    #       See ModeMatch.fromEncodeMatch() for a blueprint.
                    items.append(item)
                else:
                    # TODO: This loses type information; ModeMatch.mnemonic
                    #       use Reference instead of plain integers.
                    items.append(value)
            else:
                # Fixed item.
                items.append(item)
        return Mnemonic(items)

    def rename(self, nameMap: Mapping[str, str]) -> Mnemonic:
        '''Returns a new Mnemonic, in which all placeholder names are
        substituted by their value in the given mapping.
        '''
        return Mnemonic(
            item.rename(nameMap[item.name])
                if isinstance(item, Placeholder) else item
            for item in self._items
            )

class CodeTemplate:
    '''A container for a code block which contains placeholders that will be
    filled in later.
    '''

    def __init__(self,
                 code: CodeBlock,
                 placeholders: OrderedDict[str, Placeholder]
                 ):
        self.code = code
        self.placeholders = placeholders

    def fillPlaceholders(self, match: EncodeMatch) -> CodeTemplate:
        '''Return a new code template, in which placeholders are replaced by
        match results, if available.
        '''

        placeholders: OrderedDict[str, Placeholder] = OrderedDict()
        values = {}
        for name, placeholder in self.placeholders.items():
            try:
                value = match[name]
            except KeyError:
                placeholders[name] = placeholder
            else:
                if isinstance(value, EncodeMatch):
                    subSem = value.entry.semantics.fillPlaceholders(value)
                    fillCode = subSem.code
                    # TODO: Support submode semantics with side effects.
                    assert len(fillCode.nodes) == 0, name
                    values[name] = fillCode.returned[0]
                elif isinstance(value, int):
                    values[name] = FixedValue(
                        IntLiteral(value),
                        cast(ValuePlaceholder, placeholder).type.width
                        )
                else:
                    assert False, value

        builder = SemanticsCodeBlockBuilder()
        returned = builder.inlineBlock(self.code, values.get)
        newCode = builder.createCodeBlock(returned)

        return CodeTemplate(newCode, placeholders)

    def rename(self, nameMap: Mapping[str, str]) -> CodeTemplate:
        '''Returns a new CodeTemplate, in which all placeholder names are
        substituted by their value in the given mapping.
        '''
        code = self.code
        argMap = {
            storage.name: SingleStorage(
                storage.__class__(nameMap[storage.name], storage.width)
                )
            for storage in code.storages
            if isinstance(storage, ArgStorage)
            }
        builder = SemanticsCodeBlockBuilder()
        builder.inlineBlock(code, argMap.__getitem__)
        newCode = builder.createCodeBlock(())

        return CodeTemplate(
            newCode,
            OrderedDict(
                (nameMap[name], value)
                for name, value in self.placeholders.items()
                )
            )

class ModeEntry:
    '''One row in a mode table.
    '''

    def __init__(self,
                 encoding: Encoding,
                 mnemonic: Mnemonic,
                 semantics: Optional[CodeTemplate],
                 placeholders: OrderedDict[str, Placeholder]
                 ):
        self.encoding = encoding
        self.mnemonic = mnemonic
        self._semantics = semantics
        self.placeholders = placeholders

    def __repr__(self) -> str:
        return f'ModeEntry({self.encoding!r}, {self.mnemonic!r}, ' \
                         f'{self.semantics!r}, {self.placeholders!r})'

    @property
    def semantics(self) -> CodeTemplate:
        '''The semantics of this mode entry.
        It is an error to access this property for instruction sets that were
        loaded with the `wantSemantics=False` option.
        '''
        semantics = self._semantics
        if semantics is None:
            # In theory this can also occur if semantics are accessed after
            # there were errors reading the instruction set definition,
            # but that would be an internal error: the user shouldn't be
            # able to trigger it since no InstructionSet object is created
            # if there were errors.
            raise RuntimeError('Missing semantics')
        return semantics

    def rename(self, nameMap: Mapping[str, str]) -> ModeEntry:
        '''Returns a new ModeEntry, in which all placeholder names are
        substituted by their value in the given mapping.
        '''
        def renamePlaceholders() -> Iterator[Tuple[str, Placeholder]]:
            for name, placeholder in self.placeholders.items():
                newName = nameMap[name]
                yield newName, placeholder.rename(newName)
        semantics = self._semantics
        return ModeEntry(
            self.encoding.rename(nameMap),
            self.mnemonic.rename(nameMap),
            None if semantics is None else semantics.rename(nameMap),
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
    def fromEncodeMatch(cls, match: EncodeMatch) -> ModeMatch:
        '''Construct a ModeMatch using the data captured in an EncodeMatch.
        '''
        entry = match.entry
        placeholders = entry.placeholders

        builder = SemanticsCodeBlockBuilder()

        values: Dict[str, BitString] = {}
        subs = {}
        for name, placeholder in placeholders.items():
            if isinstance(placeholder, MatchPlaceholder):
                subMatch = cast(EncodeMatch, match[name])
                subs[name] = cls.fromEncodeMatch(subMatch)
            elif isinstance(placeholder, ComputedPlaceholder):
                values[name] = placeholder.computeValue(builder,
                                                        values.__getitem__)
            elif isinstance(placeholder, ValuePlaceholder):
                values[name] = FixedValue(
                    IntLiteral(cast(int, match[name])),
                    placeholder.type.width
                    )
            else:
                assert False, placeholder

        return cls(entry, values, subs)

    def __init__(self,
                 entry: ModeEntry,
                 values: Mapping[str, BitString],
                 subs: Mapping[str, ModeMatch]
                 ):
        self._entry = entry
        self._values = values
        self._subs = subs

    def __repr__(self) -> str:
        return f'ModeMatch({self._entry!r}, {self._values!r}, {self._subs!r})'

    def substPC(self, pc: Reference, pcVal: Expression) -> ModeMatch:
        '''Return a new mode match with the value `pcVal` substituted for
        the program counter `pc`.
        '''

        entry = self._entry
        placeholders = entry.placeholders

        values: Dict[str, BitString] = {}
        for name, value in self._values.items():
            placeholder = placeholders[name]
            if isinstance(placeholder, ComputedPlaceholder):
                builder = SemanticsCodeBlockBuilder()
                pc.emitStore(builder, pcVal, None)
                value = placeholder.computeValue(builder, values.__getitem__)
            values[name] = value

        subs = {
            subName: subMatch.substPC(pc, pcVal)
            for subName, subMatch in self._subs.items()
            }

        return ModeMatch(entry, values, subs)

    @const_property
    def mnemonic(self) -> Iterator[Union[str, Reference]]:
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

def _formatEncodingWidth(width: Optional[int]) -> str:
    return 'empty encoding' if width is None else f'encoding width {width}'

def _formatAuxEncodingWidth(width: Optional[int]) -> str:
    return (
        'no auxiliary encoding items'
        if width is None
        else f'auxiliary encoding width {width}'
        )

MnemMatch = Union[str, Type[int], 'Mode']
MnemTreeNode = Tuple[Dict[MnemMatch, Any], List[ModeEntry]]

class ModeTable:
    '''Abstract base class for mode tables.
    '''

    @property
    def encodingWidth(self) -> Optional[int]:
        return self._encWidth

    @property
    def auxEncodingWidth(self) -> Optional[int]:
        return self._auxEncWidth

    def __init__(self,
                 encWidth: Optional[int],
                 auxEncWidth: Optional[int],
                 entries: Iterable[ModeEntry]
                 ):
        if encWidth is unlimited or auxEncWidth is unlimited:
            raise ValueError('unlimited width is not allowed for encoding')
        self._encWidth = encWidth
        self._auxEncWidth = auxEncWidth
        self._entries = entries = tuple(entries)

        for entry in entries:
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

        # TODO: Annotate in more detail.
        self._mnemTree: MnemTreeNode = ({}, [])
        for entry in entries:
            self._updateMnemTree(entry)

    def dumpMnemonicTree(self) -> None:
        def matchKey(match: MnemMatch) -> Tuple[int, Optional[str]]:
            if isinstance(match, str):
                return 0, match
            elif isinstance(match, Mode):
                return 1, match.name
            elif match is int:
                return 2, None
            else:
                assert False, match
        def dumpNode(node: MnemTreeNode, indent: str) -> None:
            for entry in node[1]:
                tokens = ' '.join(str(token) for token in entry.mnemonic)
                print(f'{indent}= {tokens}')
            for match in sorted(node[0].keys(), key=matchKey):
                print(f'{indent}+ {match}')
                dumpNode(node[0][match], ' ' * len(indent) + '`---')
        dumpNode(self._mnemTree, '')

    def _updateMnemTree(self, entry: ModeEntry) -> None:
        # Update match tree for mnemonics.
        mnemonic = entry.mnemonic
        def addMnemonic(node: MnemTreeNode, idx: int) -> None:
            if idx == len(mnemonic):
                node[1].append(entry)
            else:
                token = mnemonic[idx]
                match: MnemMatch
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
    def encodedLength(self) -> Optional[int]:
        '''The number of encoded data units (bytes, words etc.) that all
        entries in this mode use, or None if that number may vary depending
        on which match is made.
        '''
        if self._encWidth is None:
            return 0
        if self._auxEncWidth is None:
            return 1
        commonLen: Optional[int] = None
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

    @property
    def name(self) -> str:
        return self._name

    @property
    def semanticsType(self) -> Union[None, IntType, ReferenceType]:
        return self._semType

    @property
    def location(self) -> InputLocation:
        return self._location

    def __init__(self,
                 name: str,
                 encWidth: Optional[int],
                 auxEncWidth: Optional[int],
                 semType: Union[None, IntType, ReferenceType],
                 location: InputLocation,
                 entries: Iterable[ModeEntry]
                 ):
        ModeTable.__init__(self, encWidth, auxEncWidth, entries)
        self._name = name
        self._semType = semType
        self._location = location

    def __str__(self) -> str:
        return f'mode {self._semType} {self._name}'

PlaceholderRole = Enum('PlaceholderRole', ( # pylint: disable=invalid-name
    'code_addr', 'data_addr'
    ))

class Placeholder:
    '''Abstract base class for a mode context element.
    '''

    @property
    def name(self) -> str:
        return self._name

    def __init__(self, name: str):
        self._name = name

    def rename(self, name: str) -> Placeholder:
        '''Returns a new placeholder that is the same as this one, except
        the name is changed to the given name.
        '''
        raise NotImplementedError

class ValuePlaceholder(Placeholder):
    '''An element from a mode context that represents a numeric value.
    '''

    @property
    def type(self) -> IntType:
        return self._type

    def __init__(self, name: str, typ: IntType):
        Placeholder.__init__(self, name)
        self._type = typ

    def __repr__(self) -> str:
        return f'ValuePlaceholder({self._name!r}, {self._type!r})'

    def __str__(self) -> str:
        return f'{{{self._type} {self._name}}}'

    def rename(self, name: str) -> ValuePlaceholder:
        return ValuePlaceholder(name, self._type)

class ComputedPlaceholder(ValuePlaceholder):
    '''An element from a mode context that represents a computed numeric value.
    '''

    @property
    def code(self) -> CodeBlock:
        return self._code

    def __init__(self, name: str, typ: IntType, code: CodeBlock):
        ValuePlaceholder.__init__(self, name, typ)
        self._code = code

    def __repr__(self) -> str:
        return f'ComputedPlaceholder({self._name!r}, {self._type!r}, ' \
                                   f'{self._code!r})'

    def __str__(self) -> str:
        return f'{{{self._type} {self._name} = ...}}'

    def rename(self, name: str) -> ComputedPlaceholder:
        return ComputedPlaceholder(name, self._type, self._code)

    def computeValue(self,
                     builder: SemanticsCodeBlockBuilder,
                     argFetcher: Callable[[str], Optional[BitString]]
                     ) -> FixedValue:
        '''Computes the value of this placeholder.
        The builder can already contain nodes, for example to initialize
        registers like the program counter. This placeholder's code will
        be inlined on the builder.
        See `SemanticsCodeBlockBuilder.inlineBlock` to learn how argument
        fetching works.
        '''
        returned = builder.inlineBlock(self._code, argFetcher)
        computeCode = CodeBlockSimplifier(builder.nodes, returned)
        computeCode.simplify()
        valBits, = computeCode.returned
        assert isinstance(valBits, FixedValue), valBits
        valType = self._type
        valExpr = decodeInt(valBits.expr, valType)
        return FixedValue(simplifyExpression(valExpr), valType.width)

class MatchPlaceholder(Placeholder):
    '''An element from a mode context that will be filled in by a match made
    in a different mode table.
    '''

    @property
    def mode(self) -> Mode:
        return self._mode

    def __init__(self, name: str, mode: Mode):
        Placeholder.__init__(self, name)
        self._mode = mode

    def __repr__(self) -> str:
        return f'MatchPlaceholder({self._name!r}, {self._mode!r})'

    def __str__(self) -> str:
        return f'{{{self._mode.name} {self._name}}}'

    def rename(self, name: str) -> MatchPlaceholder:
        return MatchPlaceholder(name, self._mode)

class EncodeMatch:
    '''A match on the encoding field of a mode entry.
    '''

    @property
    def entry(self) -> ModeEntry:
        return self._entry

    def __init__(self, entry: ModeEntry):
        self._entry = entry
        self._mapping: Dict[str, Union[EncodeMatch, int]] = {}

    def __repr__(self) -> str:
        return f'EncodeMatch({self._entry!r}, {self._mapping!r})'

    def __getitem__(self, key: str) -> Union[EncodeMatch, int]:
        return self._mapping[key]

    def __setitem__(self, key: str, value: Union[EncodeMatch, int]) -> None:
        assert key not in self._mapping, key
        self._mapping[key] = value

    def fillPlaceholders(self) -> ModeEntry:
        '''Return a new entry, in which those placeholders that are present
        in this match are replaced by the mode/value they are mapped to.
        It is not necessary for the match to provide modes/values for every
        placeholder: whatever is not matched is left untouched.
        '''

        entry = self._entry
        mapping = self._mapping
        if not mapping:
            # Skip no-op substitution for efficiency's sake.
            return entry

        encoding = entry.encoding.fillPlaceholders(self)
        mnemonic = entry.mnemonic.fillPlaceholders(self)
        semantics = entry.semantics.fillPlaceholders(self)
        placeholders = OrderedDict(
            (name, placeholder)
            for name, placeholder in entry.placeholders.items()
            if name not in mapping
            )
        return ModeEntry(encoding, mnemonic, semantics, placeholders)

    @const_property
    def encodedLength(self) -> int:
        encDef = self._entry.encoding
        length = encDef.encodedLength
        if length is not None:
            # Mode entry has fixed encoded length.
            return length

        # Mode entry has variable encoded length.
        mapping = self._mapping
        length = 0
        for encItem in encDef:
            if isinstance(encItem, EncodingExpr):
                length += 1
            elif isinstance(encItem, EncodingMultiMatch):
                match = cast(EncodeMatch, mapping[encItem.name])
                length += match.encodedLength - encItem.start
            else:
                assert False, encItem
        return length
