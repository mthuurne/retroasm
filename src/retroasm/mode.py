from __future__ import annotations

from collections import defaultdict
from collections.abc import Callable, Iterable, Iterator, Mapping, Sequence
from dataclasses import dataclass
from enum import Enum, auto
from typing import AbstractSet, Any, TypeAlias, Union, cast, overload

from .codeblock import CodeBlock
from .codeblock_builder import SemanticsCodeBlockBuilder
from .codeblock_simplifier import CodeBlockSimplifier
from .expression import Expression
from .expression_simplifier import simplifyExpression
from .linereader import InputLocation, mergeSpan
from .reference import (
    BitString,
    FixedValue,
    FixedValueReference,
    Reference,
    SingleStorage,
    decode_int,
)
from .storage import ArgStorage, Storage
from .types import IntType, ReferenceType, Width
from .utils import bad_type, const_property


class EncodingExpr:
    """
    A single element in an encoding sequence that is specified using an
    expression.
    """

    @property
    def bits(self) -> BitString:
        return self._bits

    @property
    def location(self) -> InputLocation:
        return self._location

    @property
    def encodingWidth(self) -> Width:
        return self._bits.width

    @property
    def encodingType(self) -> IntType:
        return IntType.u(self.encodingWidth)

    @property
    def reference(self) -> Reference:
        return Reference(self.bits, self.encodingType)

    @property
    def encodedLength(self) -> int:
        return 1

    def __init__(self, bits: BitString, location: InputLocation):
        self._bits = bits
        self._location = location

    def __str__(self) -> str:
        return str(self._bits)

    def __repr__(self) -> str:
        return f"EncodingExpr({self._bits!r}, {self._location!r})"

    def substitute(
        self,
        func: Callable[[str], BitString | None],
    ) -> EncodingExpr:
        """
        Apply the given substitution function to each placeholder.
        The function is passed a placeholder name and should either return
        a bit string containing the value for that placeholder, or None
        to preserve the placeholder.
        """

        def substPlaceholder(storage: Storage) -> BitString | None:
            match storage:
                case ArgStorage(name=name):
                    return func(name)
                case _:
                    return None

        bits = self._bits
        newBits = bits.substitute(storage_func=substPlaceholder)
        if bits is newBits:
            return self
        else:
            return EncodingExpr(newBits, self._location)

    def rename(self, nameMap: Mapping[str, str]) -> EncodingExpr:
        """
        Returns a new EncodingExpr, with placeholder names substituted by
        their value in the given mapping.
        """

        def renameValArg(storage: Storage) -> SingleStorage | None:
            match storage:
                case ArgStorage(name=name, width=width):
                    return SingleStorage(ArgStorage(nameMap[name], width))
                case _:
                    return None

        return EncodingExpr(
            self._bits.substitute(storage_func=renameValArg), self._location
        )


class EncodingMultiMatch:
    """
    A segment in an encoding sequence of zero or more elements, that will
    be filled in by a matched entry from an included mode.
    """

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
    def encodingWidth(self) -> Width | None:
        if self._start == 0:
            return self._mode.encodingWidth
        else:
            return self._mode.auxEncodingWidth

    @property
    def auxEncodingWidth(self) -> Width | None:
        return self._mode.auxEncodingWidth

    def __init__(self, name: str, mode: Mode, start: int, location: InputLocation):
        self._name = name
        self._mode = mode
        self._start = start
        self._location = location

    def __str__(self) -> str:
        return f"{self._name}@"

    def __repr__(self) -> str:
        return (
            f"EncodingMultiMatch({self._name!r}, {self._mode!r}, "
            f" {self._start!r}, {self._location!r})"
        )

    def rename(self, nameMap: Mapping[str, str]) -> EncodingMultiMatch:
        """
        Returns a new EncodingMultiMatch, with the placeholder name
        substituted by its value in the given mapping.
        """
        return EncodingMultiMatch(
            nameMap[self._name], self._mode, self._start, self._location
        )

    @const_property
    def encodedLength(self) -> int | None:
        length = self._mode.encodedLength
        return None if length is None else length - self._start


def _findFirstAuxIndex(encoding: Sequence[EncodingItem]) -> int | None:
    """
    Returns the index of the first encoding item that can match auxiliary
    encoding units, or None if no auxiliary encoding units can be matched.
    The given encoding sequence must not contain matchers that never match
    any encoding units.
    """
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


EncodingItem: TypeAlias = Union[EncodingExpr, EncodingMultiMatch]


class Encoding:
    """
    Defines how (part of) an instruction is encoded.
    We call the elements of a definition 'items', these are represented by
    EncodingExpr and EncodingMultiMatch objects. We call the elements of an
    encoded instruction 'units', depending on the instruction set these are
    bytes or words or some other fixed-size bit strings.
    The items within an encoding definition are exposed as a sequence.
    """

    def __init__(self, items: Iterable[EncodingItem], location: InputLocation):
        # Filter out zero-length encoding items.
        # There is no good reason to ban them, but keeping them around would
        # unnecessarily complicate the code.
        nonEmptyItems: Sequence[EncodingItem] = tuple(
            item for item in items if item.encodedLength != 0
        )
        self._items = nonEmptyItems
        self._firstAuxIndex = _findFirstAuxIndex(nonEmptyItems)

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
                raise ValueError("inconsistent widths among auxiliary encoding units")

        self._location = location

    def __repr__(self) -> str:
        return f"Encoding({self._items!r}, {self._location!r})"

    def __iter__(self) -> Iterator[EncodingItem]:
        return iter(self._items)

    def __len__(self) -> int:
        return len(self._items)

    @overload
    def __getitem__(self, index: int) -> EncodingItem:
        ...

    @overload
    def __getitem__(self, index: slice) -> Sequence[EncodingItem]:
        ...

    def __getitem__(self, index: int | slice) -> Any:
        return self._items[index]

    def fillPlaceholders(self, match: EncodeMatch) -> Encoding:
        """
        Return a new encoding, in which placeholders are replaced by
        match results, if available.
        """

        # In the case of multi-matches, we might need a filled submode encoding
        # multiple times, so cache them.
        subEncodings: dict[str, Encoding] = {}

        def getSubEncoding(name: str, subMatch: EncodeMatch) -> Encoding:
            try:
                return subEncodings[name]
            except KeyError:
                subEnc = subMatch.entry.encoding.fillPlaceholders(subMatch)
                subEncodings[name] = subEnc
                return subEnc

        def substPlaceholder(name: str) -> BitString | None:
            try:
                value = match[name]
            except KeyError:
                return None
            match value:
                case EncodeMatch() as sub_match:
                    # We're called to substitute into an EncodingExpr and those
                    # always match the first encoding item of the submode.
                    firstItem = getSubEncoding(name, sub_match)[0]
                    match firstItem:
                        case EncodingExpr(bits=bits):
                            return bits
                        case EncodingMultiMatch():
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
                        case item:
                            bad_type(item)
                case FixedValueReference():
                    assert False, value
                case value:
                    bad_type(value)

        items: list[EncodingItem] = []
        for item in self._items:
            match item:
                case EncodingExpr():
                    items.append(item.substitute(substPlaceholder))
                case EncodingMultiMatch(name=name):
                    try:
                        subMatch = cast(EncodeMatch, match[name])
                    except KeyError:
                        items.append(item)
                    else:
                        items += getSubEncoding(name, subMatch)[item.start :]
                case item:
                    bad_type(item)
        return Encoding(items, self._location)

    def rename(self, nameMap: Mapping[str, str]) -> Encoding:
        """
        Returns a new Encoding, in which all placeholder names are
        substituted by their value in the given mapping.
        """
        return Encoding((item.rename(nameMap) for item in self._items), self._location)

    @property
    def encodingWidth(self) -> Width | None:
        """
        The width in bits a first encoding unit matched by this encoding
        definition would have, or None if this encoding definition always
        matches zero encoding units.
        """
        items = self._items
        return None if len(items) == 0 else items[0].encodingWidth

    @property
    def encodingLocation(self) -> InputLocation:
        """The InputLocation of the first item in this encoding definition."""
        items = self._items
        return self._location if len(items) == 0 else items[0].location

    @property
    def auxEncodingWidth(self) -> Width | None:
        """
        The width in bits that all non-first encoding units matched by this
        encoding definition would have, or None if a match cannot contain more
        than one encoding unit.
        """
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
        """
        The InputLocation of the auxiliary encoding items in this mode
        entry. If there are no auxiliary encoding items, the end of the
        encoding field is returned.
        """
        items = self._items
        firstAuxIndex = self._firstAuxIndex
        if firstAuxIndex is None:
            location = self._location if len(items) == 0 else items[0].location
            return location.endLocation
        else:
            return mergeSpan(items[firstAuxIndex].location, items[-1].location)

    @const_property
    def encodedLength(self) -> int | None:
        """
        The number of encoded units (bytes, words etc.) that this encoding
        definitions matches, or None if that number may vary depending on which
        match is made in an included mode.
        """
        total = 0
        for item in self._items:
            length = item.encodedLength
            if length is None:
                return None
            total += length
        return total


MnemItem: TypeAlias = Union[
    str, FixedValueReference, "MatchPlaceholder", "ValuePlaceholder"
]


class Mnemonic:
    """
    Defines how (part of) an instruction is presented in assembly source code.
    The items within a mnemonic definition are exposed as a sequence.
    """

    def __init__(self, items: Iterable[MnemItem]):
        self._items = tuple(items)

    def __repr__(self) -> str:
        return f"Mnemonic({self._items!r})"

    def __iter__(self) -> Iterator[MnemItem]:
        return iter(self._items)

    def __len__(self) -> int:
        return len(self._items)

    def __getitem__(self, index: int) -> MnemItem:
        return self._items[index]

    def fillPlaceholders(self, match: EncodeMatch) -> Mnemonic:
        """
        Return a new mnemonic, in which placeholders are replaced by
        match results, if available.
        """
        items: list[MnemItem] = []
        for item in self._items:
            match item:
                case MatchPlaceholder(name=name) as item:
                    # Submode match.
                    try:
                        subMatch = cast(EncodeMatch, match[name])
                    except KeyError:
                        items.append(item)
                    else:
                        items += subMatch.entry.mnemonic.fillPlaceholders(subMatch)
                case ValuePlaceholder(name=name) as item:
                    # Immediate value.
                    try:
                        value = cast(FixedValueReference, match[name])
                    except KeyError:
                        # TODO: Apply substitutions inside computed values.
                        #       See ModeMatch.fromEncodeMatch() for a blueprint.
                        items.append(item)
                    else:
                        items.append(value)
                case item:
                    # Fixed item.
                    items.append(item)
        return Mnemonic(items)

    def rename(self, nameMap: Mapping[str, str]) -> Mnemonic:
        """
        Returns a new Mnemonic, in which all placeholder names are
        substituted by their value in the given mapping.
        """
        return Mnemonic(
            item.rename(nameMap[item.name])
            if isinstance(item, (MatchPlaceholder, ValuePlaceholder))
            else item
            for item in self._items
        )


class CodeTemplate:
    """
    A container for a code block which contains placeholders that will be
    filled in later.
    """

    def __init__(
        self,
        code: CodeBlock,
        placeholders: Iterable[MatchPlaceholder | ValuePlaceholder],
    ):
        self.code = code
        self.placeholders = tuple(placeholders)

    def fillPlaceholders(self, match: EncodeMatch) -> CodeTemplate:
        """
        Return a new code template, in which placeholders are replaced by
        match results, if available.
        """

        unfilled = []
        values = {}
        for placeholder in self.placeholders:
            name = placeholder.name
            try:
                value = match[name]
            except KeyError:
                unfilled.append(placeholder)
            else:
                match value:
                    case EncodeMatch() as value:
                        subSem = value.entry.semantics.fillPlaceholders(value)
                        fillCode = subSem.code
                        # TODO: Support submode semantics with side effects.
                        assert len(fillCode.nodes) == 0, name
                        values[name] = fillCode.returned[0]
                    case FixedValueReference(bits=bits):
                        values[name] = bits
                    case value:
                        bad_type(value)

        builder = SemanticsCodeBlockBuilder()
        returned = builder.inlineBlock(self.code, values.get)
        newCode = builder.createCodeBlock(returned)

        return CodeTemplate(newCode, unfilled)

    def rename(self, nameMap: Mapping[str, str]) -> CodeTemplate:
        """
        Returns a new CodeTemplate, in which all placeholder names are
        substituted by their value in the given mapping.
        """
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

        newPlaceholders = (
            placeholder.rename(nameMap[placeholder.name])
            for placeholder in self.placeholders
        )

        return CodeTemplate(newCode, newPlaceholders)


class ModeEntry:
    """One row in a mode table."""

    def __init__(
        self,
        encoding: Encoding,
        mnemonic: Mnemonic,
        semantics: CodeTemplate | None,
        placeholders: Iterable[MatchPlaceholder | ValuePlaceholder],
        flagsRequired: AbstractSet[str],
    ):
        self.encoding = encoding
        self.mnemonic = mnemonic
        self._semantics = semantics
        self.placeholders = tuple(placeholders)
        self.flagsRequired = frozenset(flagsRequired)

    def __repr__(self) -> str:
        return (
            f"ModeEntry({self.encoding!r}, {self.mnemonic!r}, "
            f"{self._semantics!r}, {self.placeholders!r}, {self.flagsRequired!r})"
        )

    @property
    def semantics(self) -> CodeTemplate:
        """
        The semantics of this mode entry.
        It is an error to access this property for instruction sets that were
        loaded with the `wantSemantics=False` option.
        """
        semantics = self._semantics
        if semantics is None:
            # In theory this can also occur if semantics are accessed after
            # there were errors reading the instruction set definition,
            # but that would be an internal error: the user shouldn't be
            # able to trigger it since no InstructionSet object is created
            # if there were errors.
            raise RuntimeError("Missing semantics")
        return semantics

    def rename(self, nameMap: Mapping[str, str]) -> ModeEntry:
        """
        Returns a new ModeEntry, in which all placeholder names are
        substituted by their value in the given mapping.
        """
        semantics = self._semantics
        return ModeEntry(
            self.encoding.rename(nameMap),
            self.mnemonic.rename(nameMap),
            None if semantics is None else semantics.rename(nameMap),
            (p.rename(nameMap[p.name]) for p in self.placeholders),
            self.flagsRequired,
        )


class ModeMatch:
    """
    A flattened match of a mode entry at a particular address.
    Flattened means that all submode matches have been resolved and substituted
    into this match.
    """

    __slots__ = ("_entry", "_values", "_subs", "_mnemonic")

    @classmethod
    def fromEncodeMatch(cls, match: EncodeMatch) -> ModeMatch:
        """Construct a ModeMatch using the data captured in an EncodeMatch."""
        entry = match.entry
        placeholders = entry.placeholders

        builder = SemanticsCodeBlockBuilder()

        values: dict[str, BitString] = {}
        subs = {}
        for placeholder in placeholders:
            match placeholder:
                case MatchPlaceholder(name=name):
                    subMatch = cast(EncodeMatch, match[name])
                    subs[name] = cls.fromEncodeMatch(subMatch)
                case ComputedPlaceholder(name=name) as placeholder:
                    values[name] = placeholder.computeValue(builder, values.__getitem__)
                case ValuePlaceholder(name=name):
                    values[name] = cast(FixedValueReference, match[name]).bits
                case placeholder:
                    bad_type(placeholder)

        return cls(entry, values, subs)

    def __init__(
        self,
        entry: ModeEntry,
        values: Mapping[str, BitString],
        subs: Mapping[str, ModeMatch],
    ):
        self._entry = entry
        self._values = values
        self._subs = subs

    def __repr__(self) -> str:
        return f"ModeMatch({self._entry!r}, {self._values!r}, {self._subs!r})"

    @property
    def flagsRequired(self) -> AbstractSet[str]:
        """The prefix flags that must be set to match this mode entry."""
        flags = self._entry.flagsRequired
        for subMatch in self._subs.values():
            flags |= subMatch.flagsRequired
        return flags

    def iterBits(self) -> Iterator[BitString]:
        """
        Yield the encoding of this match as bit strings.
        Each yielded item has the instruction set's encoding width.
        """

        def subst(name: str) -> BitString:
            value = self._values.get(name)
            if value is not None:
                return value
            return next(self._subs[name].iterBits())

        for encItem in self._entry.encoding:
            match encItem:
                case EncodingExpr() as expr:
                    yield expr.substitute(subst).bits
                case EncodingMultiMatch(name=name, start=start):
                    subBits = self._subs[name].iterBits()
                    if start != 0:
                        for _ in subBits:
                            break
                    yield from subBits
                case item:
                    bad_type(item)

    def substPC(self, pc: Reference, pcVal: Expression) -> ModeMatch:
        """
        Return a new mode match with the value `pcVal` substituted for
        the program counter `pc`.
        """

        entry = self._entry
        placeholders = {p.name: p for p in entry.placeholders}

        values: dict[str, BitString] = {}
        for name, value in self._values.items():
            placeholder = placeholders[name]
            if isinstance(placeholder, ComputedPlaceholder):
                builder = SemanticsCodeBlockBuilder()
                pc.emit_store(builder, pcVal, None)
                value = placeholder.computeValue(builder, values.__getitem__)
            values[name] = value

        subs = {
            subName: subMatch.substPC(pc, pcVal)
            for subName, subMatch in self._subs.items()
        }

        return ModeMatch(entry, values, subs)

    @const_property
    def mnemonic(self) -> Iterator[str | FixedValueReference]:
        entry = self._entry
        subs = self._subs
        values = self._values

        for mnemElem in entry.mnemonic:
            match mnemElem:
                case str() | FixedValueReference() as elem:
                    yield elem
                case MatchPlaceholder(name=name):
                    yield from subs[name].mnemonic
                case ValuePlaceholder(name=name, type=typ):
                    value = values[name]
                    assert isinstance(value, FixedValue), value
                    yield FixedValueReference(value.expr, typ)
                case elem:
                    bad_type(elem)


def _formatEncodingWidth(width: Width | None) -> str:
    return "empty encoding" if width is None else f"encoding width {width}"


def _formatAuxEncodingWidth(width: Width | None) -> str:
    return (
        "no auxiliary encoding items"
        if width is None
        else f"auxiliary encoding width {width}"
    )


MnemMatch: TypeAlias = Union[str, type[int], "Mode"]


class _MnemTreeNode:
    """
    A node in a mnemonic match tree.

    A mnemonic match tree efficiently finds the matching mode entry for a given
    mnemonic sequence.
    """

    def __init__(self) -> None:
        self._children: dict[MnemMatch, _MnemTreeNode] = defaultdict(_MnemTreeNode)
        self._leaves: list[ModeEntry] = []

    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self._children!r}, {self._leaves!r})"

    @staticmethod
    def _matchKey(match: MnemMatch) -> tuple[int, str | None]:
        match match:
            case str() as text:
                return 0, text
            case Mode(name=name):
                return 1, name
            case type() as typ:
                assert typ is int
                return 2, None
            case match:
                bad_type(match)

    def dump(self, indent: str) -> None:
        for entry in self._leaves:
            tokens = " ".join(str(token) for token in entry.mnemonic)
            print(f"{indent}= {tokens}")
        children = self._children
        for match in sorted(children.keys(), key=self._matchKey):
            print(f"{indent}+ {match}")
            children[match].dump(" " * len(indent) + "`---")

    def addModeEntry(self, entry: ModeEntry) -> None:
        """Add the given mode entry to this tree."""
        node = self
        for token in entry.mnemonic:
            match: MnemMatch
            match token:
                case str() as text:
                    match = text
                case FixedValueReference() | ValuePlaceholder():
                    match = int
                case MatchPlaceholder(mode=mode):
                    match = mode
                case token:
                    bad_type(token)
            node = node._children[match]
        node._leaves.append(entry)


class ModeTable:
    """Abstract base class for mode tables."""

    @property
    def encodingWidth(self) -> Width | None:
        return self._encWidth

    @property
    def auxEncodingWidth(self) -> Width | None:
        return self._auxEncWidth

    def __init__(
        self,
        encWidth: Width | None,
        auxEncWidth: Width | None,
        entries: Iterable[ModeEntry],
    ):
        self._encWidth = encWidth
        self._auxEncWidth = auxEncWidth
        self._entries = entries = tuple(entries)

        for entry in entries:
            encDef = entry.encoding
            if encDef.encodingWidth != encWidth:
                raise ValueError(
                    f"mode with {_formatEncodingWidth(encWidth)} contains "
                    f"entry with {_formatEncodingWidth(encDef.encodingWidth)}"
                )
            if encDef.auxEncodingWidth not in (None, auxEncWidth):
                raise ValueError(
                    f"mode with {_formatAuxEncodingWidth(auxEncWidth)} contains "
                    f"entry with {_formatAuxEncodingWidth(encDef.auxEncodingWidth)}"
                )

        self._mnemTree = mnemTree = _MnemTreeNode()
        for entry in entries:
            mnemTree.addModeEntry(entry)

    def dumpMnemonicTree(self) -> None:
        self._mnemTree.dump("")

    @const_property
    def encodedLength(self) -> int | None:
        """
        The number of encoded data units (bytes, words etc.) that all
        entries in this mode use, or None if that number may vary depending
        on which match is made.
        """
        if self._encWidth is None:
            return 0
        if self._auxEncWidth is None:
            return 1
        commonLen: int | None = None
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
    """
    A pattern for operands, such as an addressing mode or a table defining
    register encoding.
    """

    @property
    def name(self) -> str:
        return self._name

    @property
    def semanticsType(self) -> None | IntType | ReferenceType:
        return self._semType

    @property
    def location(self) -> InputLocation:
        return self._location

    def __init__(
        self,
        name: str,
        encWidth: int | None,
        auxEncWidth: int | None,
        semType: None | IntType | ReferenceType,
        location: InputLocation,
        entries: Iterable[ModeEntry],
    ):
        ModeTable.__init__(self, encWidth, auxEncWidth, entries)
        self._name = name
        self._semType = semType
        self._location = location

    def __str__(self) -> str:
        return f"mode {self._semType} {self._name}"


class PlaceholderRole(Enum):
    code_addr = auto()
    data_addr = auto()


@dataclass(frozen=True)
class ValuePlaceholder:
    """An element from a mode context that represents a numeric value."""

    name: str
    type: IntType

    def __str__(self) -> str:
        return f"{{{self.type} {self.name}}}"

    def rename(self, name: str) -> ValuePlaceholder:
        return ValuePlaceholder(name, self.type)


@dataclass(frozen=True)
class ComputedPlaceholder(ValuePlaceholder):
    """An element from a mode context that represents a computed numeric value."""

    code: CodeBlock

    def __str__(self) -> str:
        return f"{{{self.type} {self.name} = ...}}"

    def rename(self, name: str) -> ComputedPlaceholder:
        return ComputedPlaceholder(name, self.type, self.code)

    def computeValue(
        self,
        builder: SemanticsCodeBlockBuilder,
        argFetcher: Callable[[str], BitString | None],
    ) -> FixedValue:
        """
        Computes the value of this placeholder.
        The builder can already contain nodes, for example to initialize
        registers like the program counter. This placeholder's code will
        be inlined on the builder.
        See `SemanticsCodeBlockBuilder.inlineBlock` to learn how argument
        fetching works.
        """
        returned = builder.inlineBlock(self.code, argFetcher)
        computeCode = CodeBlockSimplifier(builder.nodes, returned)
        computeCode.simplify()
        (valBits,) = computeCode.returned
        assert isinstance(valBits, FixedValue), valBits
        valType = self.type
        valExpr = decode_int(valBits.expr, valType)
        return FixedValue(simplifyExpression(valExpr), valType.width)


@dataclass(frozen=True)
class MatchPlaceholder:
    """
    An element from a mode context that will be filled in by a match made
    in a different mode table.
    """

    name: str
    mode: Mode

    def __str__(self) -> str:
        return f"{{{self.mode.name} {self.name}}}"

    def rename(self, name: str) -> MatchPlaceholder:
        return MatchPlaceholder(name, self.mode)


class EncodeMatch:
    """A match on the encoding field of a mode entry."""

    @property
    def entry(self) -> ModeEntry:
        return self._entry

    def __init__(self, entry: ModeEntry):
        self._entry = entry
        self._mapping: dict[str, EncodeMatch | FixedValueReference] = {}

    def __repr__(self) -> str:
        return f"EncodeMatch({self._entry!r}, {self._mapping!r})"

    def __getitem__(self, key: str) -> EncodeMatch | FixedValueReference:
        return self._mapping[key]

    def __setitem__(self, key: str, value: EncodeMatch | FixedValueReference) -> None:
        assert key not in self._mapping, key
        self._mapping[key] = value

    def fillPlaceholders(self) -> ModeEntry:
        """
        Return a new entry, in which those placeholders that are present
        in this match are replaced by the mode/value they are mapped to.
        It is not necessary for the match to provide modes/values for every
        placeholder: whatever is not matched is left untouched.
        """

        entry = self._entry
        mapping = self._mapping
        if not mapping:
            # Skip no-op substitution for efficiency's sake.
            return entry

        encoding = entry.encoding.fillPlaceholders(self)
        mnemonic = entry.mnemonic.fillPlaceholders(self)
        semantics = entry.semantics.fillPlaceholders(self)
        unfilled = (p for p in entry.placeholders if p.name not in mapping)

        flagsRequired = entry.flagsRequired.union(
            *(
                match.entry.flagsRequired
                for match in self._mapping.values()
                if isinstance(match, EncodeMatch)
            )
        )

        return ModeEntry(encoding, mnemonic, semantics, unfilled, flagsRequired)

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
            match encItem:
                case EncodingExpr():
                    length += 1
                case EncodingMultiMatch(name=name, start=start):
                    match = cast(EncodeMatch, mapping[name])
                    length += match.encodedLength - start
                case _ as obj:
                    bad_type(obj)
        return length
