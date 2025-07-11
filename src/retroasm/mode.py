from __future__ import annotations

from collections.abc import Callable, Iterable, Iterator, Mapping, Sequence, Set
from dataclasses import dataclass
from functools import partial
from typing import Any, Final, overload, override

from .codeblock import FunctionBody
from .expression import Expression
from .input import InputLocation
from .reference import BitString, FixedValue, FixedValueReference, Reference
from .symbol import CurrentAddress, ImmediateValue
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
    def location(self) -> InputLocation | None:
        return self._location

    @property
    def encoding_width(self) -> Width:
        return self._bits.width

    @property
    def encoding_type(self) -> IntType:
        return IntType.u(self.encoding_width)

    @property
    def reference(self) -> Reference:
        return Reference(self.bits, self.encoding_type)

    @property
    def encoded_length(self) -> int:
        return 1

    def __init__(self, bits: BitString, location: InputLocation | None):
        self._bits = bits
        self._location = location

    @override
    def __str__(self) -> str:
        return str(self._bits)

    @override
    def __repr__(self) -> str:
        return f"EncodingExpr({self._bits!r}, {self._location!r})"

    def substitute(self, func: Callable[[str], Expression | None]) -> EncodingExpr:
        """
        Apply the given substitution function to each placeholder.
        The function is passed a placeholder name and should either return
        a bit string containing the value for that placeholder, or None
        to preserve the placeholder.
        """

        def subst_placeholder(expr: Expression) -> Expression | None:
            match expr:
                case ImmediateValue(name=name):
                    return func(name)
                case _:
                    return None

        bits = self._bits
        new_bits = bits.substitute(expression_func=subst_placeholder)
        if bits is new_bits:
            return self
        else:
            return EncodingExpr(new_bits, self._location)

    def rename(self, name_map: Mapping[str, str]) -> EncodingExpr:
        """
        Returns a new EncodingExpr, with placeholder names substituted by
        their value in the given mapping.
        """

        bits = self._bits
        new_bits = bits.substitute(
            expression_func=partial(_rename_immediate, name_map=name_map)
        )
        if new_bits is bits:
            return self
        else:
            return EncodingExpr(new_bits, self._location)


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
    def location(self) -> InputLocation | None:
        return self._location

    @property
    def encoding_width(self) -> Width | None:
        if self._start == 0:
            return self._mode.encoding_width
        else:
            return self._mode.aux_encoding_width

    @property
    def aux_encoding_width(self) -> Width | None:
        return self._mode.aux_encoding_width

    def __init__(self, name: str, mode: Mode, start: int, location: InputLocation | None):
        self._name = name
        self._mode = mode
        self._start = start
        self._location = location

    @override
    def __str__(self) -> str:
        return f"{self._name}@"

    @override
    def __repr__(self) -> str:
        return (
            f"EncodingMultiMatch({self._name!r}, {self._mode!r}, "
            f" {self._start!r}, {self._location!r})"
        )

    def rename(self, name_map: Mapping[str, str]) -> EncodingMultiMatch:
        """
        Returns a new EncodingMultiMatch, with the placeholder name
        substituted by its value in the given mapping.
        """
        return EncodingMultiMatch(name_map[self._name], self._mode, self._start, self._location)

    @const_property
    def encoded_length(self) -> int | None:
        length = self._mode.encoded_length
        return None if length is None else length - self._start


def _find_first_aux_index(encoding: Sequence[EncodingItem]) -> int | None:
    """
    Returns the index of the first encoding item that can match auxiliary
    encoding units, or None if no auxiliary encoding units can be matched.
    The given encoding sequence must not contain matchers that never match
    any encoding units.
    """
    if len(encoding) == 0:
        # No units matched because there are no matchers.
        return None
    first_len = encoding[0].encoded_length
    assert first_len is not None, encoding
    if first_len >= 2:
        # First element can match multiple encoding units.
        return 0
    assert first_len != 0, encoding
    if len(encoding) == 1:
        # First element matches 1 encoding unit, no second element.
        return None
    else:
        # The second element will match the second unit.
        assert encoding[1].encoded_length != 0, encoding
        return 1


type EncodingItem = EncodingExpr | EncodingMultiMatch


class Encoding:
    """
    Defines how (part of) an instruction is encoded.
    We call the elements of a definition 'items', these are represented by
    EncodingExpr and EncodingMultiMatch objects. We call the elements of an
    encoded instruction 'units', depending on the instruction set these are
    bytes or words or some other fixed-size bit strings.
    The items within an encoding definition are exposed as a sequence.
    """

    def __init__(
        self,
        items: Iterable[EncodingItem],
        flags_required: Iterable[str],
        location: InputLocation,
    ):
        # Filter out zero-length encoding items.
        # There is no good reason to ban them, but keeping them around would
        # unnecessarily complicate the code.
        non_empty_items: Sequence[EncodingItem] = tuple(
            item for item in items if item.encoded_length != 0
        )
        self._items = non_empty_items
        self._first_aux_index = _find_first_aux_index(non_empty_items)

        # Verify that all auxiliary units have the same width.
        aux_width = self.aux_encoding_width
        if aux_width is not None:
            consistent = True
            for idx, item in enumerate(non_empty_items):
                if idx != 0:
                    consistent &= item.encoding_width == aux_width
                if isinstance(item, EncodingMultiMatch):
                    consistent &= item.aux_encoding_width in (None, aux_width)
            if not consistent:
                raise ValueError("inconsistent widths among auxiliary encoding units")

        self.flags_required: Final[Set[str]] = frozenset(flags_required)

        self._location = location

    @override
    def __repr__(self) -> str:
        return f"Encoding({self._items!r}, {self._location!r})"

    def __iter__(self) -> Iterator[EncodingItem]:
        return iter(self._items)

    def __len__(self) -> int:
        return len(self._items)

    @overload
    def __getitem__(self, index: int) -> EncodingItem: ...

    @overload
    def __getitem__(self, index: slice) -> Sequence[EncodingItem]: ...

    def __getitem__(self, index: int | slice) -> Any:
        return self._items[index]

    def fill_placeholders(self, match: EncodeMatch) -> Encoding:
        """
        Return a new encoding, in which placeholders are replaced by
        match results, if available.
        """

        # In the case of multi-matches, we might need a filled submode encoding
        # multiple times, so cache them.
        sub_encodings: dict[str, Encoding] = {}

        def get_sub_encoding(name: str, submatch: EncodeMatch) -> Encoding:
            try:
                return sub_encodings[name]
            except KeyError:
                sub_enc = submatch.entry.encoding.fill_placeholders(submatch)
                sub_encodings[name] = sub_enc
                return sub_enc

        def subst_placeholder(name: str) -> Expression | None:
            try:
                submatch = match.get_submatch(name)
            except KeyError:
                return None
            # We're called to substitute into an EncodingExpr and those
            # always match the first encoding item of the submode.
            first_item = get_sub_encoding(name, submatch)[0]
            match first_item:
                case EncodingExpr(bits=bits):
                    # TODO: Currently the unit tests don't reach this.
                    #       Is the code unreachable or are the tests incomplete?
                    assert False, bits
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
                    assert False, first_item
                case item:
                    bad_type(item)

        items: list[EncodingItem] = []
        for item in self._items:
            match item:
                case EncodingExpr():
                    items.append(item.substitute(subst_placeholder))
                case EncodingMultiMatch(name=name):
                    try:
                        submatch = match.get_submatch(name)
                    except KeyError:
                        items.append(item)
                    else:
                        items += get_sub_encoding(name, submatch)[item.start :]
                case item:
                    bad_type(item)
        return Encoding(items, match.flags_required, self._location)

    def rename(self, name_map: Mapping[str, str]) -> Encoding:
        """
        Returns a new Encoding, in which all placeholder names are
        substituted by their value in the given mapping.
        """
        return Encoding(
            (item.rename(name_map) for item in self._items), self.flags_required, self._location
        )

    @property
    def encoding_width(self) -> Width | None:
        """
        The width in bits a first encoding unit matched by this encoding
        definition would have, or None if this encoding definition always
        matches zero encoding units.
        """
        items = self._items
        return None if len(items) == 0 else items[0].encoding_width

    @property
    def location(self) -> InputLocation:
        """The location spanning the entire encoding definition."""
        return self._location

    @property
    def encoding_location(self) -> InputLocation | None:
        """The InputLocation of the first item in this encoding definition."""
        items = self._items
        return self._location if len(items) == 0 else items[0].location

    @property
    def aux_encoding_width(self) -> Width | None:
        """
        The width in bits that all non-first encoding units matched by this
        encoding definition would have, or None if a match cannot contain more
        than one encoding unit.
        """
        first_aux_index = self._first_aux_index
        if first_aux_index is None:
            return None
        elif first_aux_index == 0:
            item = self._items[0]
            assert isinstance(item, EncodingMultiMatch), item
            return item.aux_encoding_width
        else:
            assert first_aux_index == 1, first_aux_index
            return self._items[1].encoding_width

    @property
    def aux_encoding_location(self) -> InputLocation | None:
        """
        The InputLocation of the auxiliary encoding items in this mode
        entry. If there are no auxiliary encoding items, the end of the
        encoding field is returned.
        """
        items = self._items
        first_aux_index = self._first_aux_index
        if first_aux_index is None:
            location = self._location if len(items) == 0 else items[0].location
            return None if location is None else location.end_location
        else:
            return InputLocation.merge_span(items[first_aux_index].location, items[-1].location)

    @const_property
    def encoded_length(self) -> int | None:
        """
        The number of encoded units (bytes, words etc.) that this encoding
        definitions matches, or None if that number may vary depending on which
        match is made in an included mode.
        """
        total = 0
        for item in self._items:
            length = item.encoded_length
            if length is None:
                return None
            total += length
        return total


type MnemItem = str | FixedValueReference | MatchPlaceholder


class Mnemonic:
    """
    Defines how (part of) an instruction is presented in assembly source code.
    The items within a mnemonic definition are exposed as a sequence.
    """

    def __init__(self, items: Iterable[MnemItem]):
        self._items = tuple(items)

    @override
    def __repr__(self) -> str:
        return f"Mnemonic({self._items!r})"

    def __iter__(self) -> Iterator[MnemItem]:
        return iter(self._items)

    def __len__(self) -> int:
        return len(self._items)

    def __getitem__(self, index: int) -> MnemItem:
        return self._items[index]

    def fill_placeholders(self, match: EncodeMatch) -> Mnemonic:
        """
        Return a new mnemonic, in which placeholders are replaced by
        match results, if available.
        """

        def fill_immediate(expr: Expression) -> Expression | None:
            if isinstance(expr, ImmediateValue):
                try:
                    # TODO: Are the matched values already truncated?
                    return match.get_value(expr.name).expr
                except KeyError:
                    pass
            return None

        items: list[MnemItem] = []
        for item in self._items:
            match item:
                case MatchPlaceholder(name=name) as item:
                    # Submode match.
                    try:
                        submatch = match.get_submatch(name)
                    except KeyError:
                        items.append(item)
                    else:
                        items += submatch.entry.mnemonic.fill_placeholders(submatch)
                case FixedValueReference():
                    # Expression that may or may not include immediate values.
                    items.append(item.substitute(fill_immediate))
                case str():
                    items.append(item)
                case _:
                    bad_type(item)
        return Mnemonic(items)

    def rename(self, name_map: Mapping[str, str]) -> Mnemonic:
        """
        Returns a new Mnemonic, in which all placeholder names are
        substituted by their value in the given mapping.
        """

        def rename_item(item: MnemItem) -> MnemItem:
            match item:
                case MatchPlaceholder():
                    return item.rename(name_map)
                case FixedValueReference():
                    return item.substitute(partial(_rename_immediate, name_map=name_map))
                case str():
                    return item
                case _:
                    bad_type(item)

        return Mnemonic(rename_item(item) for item in self._items)


class ModeEntry:
    """One row in a mode table."""

    def __init__(
        self,
        encoding: Encoding,
        mnemonic: Mnemonic,
        semantics: FunctionBody | None,
        match_placeholders: Iterable[MatchPlaceholder],
        value_placeholders: Mapping[str, FixedValueReference],
    ):
        self.encoding = encoding
        self.mnemonic = mnemonic
        self._semantics = semantics
        self._match_placeholders = tuple(match_placeholders)
        self._value_placeholders = dict(value_placeholders)

    @override
    def __repr__(self) -> str:
        return (
            f"ModeEntry({self.encoding!r}, {self.mnemonic!r}, "
            f"{self._semantics!r}, {self.match_placeholders!r}, "
            f"{self._value_placeholders!r})"
        )

    @property
    def match_placeholders(self) -> Iterator[MatchPlaceholder]:
        return iter(self._match_placeholders)

    @property
    def value_placeholders(self) -> Mapping[str, FixedValueReference]:
        return self._value_placeholders

    @property
    def semantics(self) -> FunctionBody:
        """
        The semantics of this mode entry.
        It is an error to access this property for instruction sets that were
        loaded with the `want_semantics=False` option.
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

    def rename(self, name_map: Mapping[str, str]) -> ModeEntry:
        """
        Returns a new ModeEntry, in which all placeholder names are
        substituted by their value in the given mapping.
        """
        return ModeEntry(
            self.encoding.rename(name_map),
            self.mnemonic.rename(name_map),
            # TODO: Rename immediates in semantics as well.
            self._semantics,
            (p.rename(name_map) for p in self._match_placeholders),
            {
                name_map[name]: ref.substitute(partial(_rename_immediate, name_map=name_map))
                for name, ref in self._value_placeholders.items()
            },
        )


class ModeMatch:
    """
    A flattened match of a mode entry at a particular address.
    Flattened means that all submode matches have been resolved and substituted
    into this match.
    """

    __slots__ = ("_entry", "_values", "_subs", "_mnemonic")

    def __init__(
        self, entry: ModeEntry, values: Mapping[str, BitString], subs: Mapping[str, ModeMatch]
    ):
        self._entry = entry
        self._values = values
        self._subs = subs

    @override
    def __repr__(self) -> str:
        return f"ModeMatch({self._entry!r}, {self._values!r}, {self._subs!r})"

    @property
    def flags_required(self) -> Set[str]:
        """The prefix flags that must be set to match this mode entry."""
        flags = self._entry.encoding.flags_required
        for submatch in self._subs.values():
            flags |= submatch.flags_required
        return flags

    def iter_bits(self) -> Iterator[BitString]:
        """
        Yield the encoding of this match as bit strings.
        Each yielded item has the instruction set's encoding width.
        """

        def subst(name: str) -> Expression:
            value = self._values.get(name)
            if value is not None:
                # TODO: This is sufficient to pass the current unit tests, but can it really
                #       only be FixedValue? If so, narrow annotations.
                assert isinstance(value, FixedValue), type(value)
                return value.expr
            bits = next(self._subs[name].iter_bits())
            # TODO: This is sufficient to pass the current unit tests, but can it really
            #       only be FixedValue? If so, narrow annotations.
            assert isinstance(bits, FixedValue), type(bits)
            return bits.expr

        for enc_item in self._entry.encoding:
            match enc_item:
                case EncodingExpr() as expr:
                    yield expr.substitute(subst).bits
                case EncodingMultiMatch(name=name, start=start):
                    sub_bits = self._subs[name].iter_bits()
                    if start != 0:
                        for _ in sub_bits:
                            break
                    yield from sub_bits
                case item:
                    bad_type(item)

    def subst_pc(self, pc_val: Expression) -> ModeMatch:
        """
        Return a new mode match with the value `pc_val` substituted for
        the program counter `pc`.
        """

        entry = self._entry
        values = dict(self._values)

        def resolve_immediate(expr: Expression) -> Expression | None:
            if isinstance(expr, CurrentAddress):
                return pc_val
            # TODO: We don't just resolve PC.
            #       This method has overlap with EncodeMatch.complete().
            if isinstance(expr, ImmediateValue):
                value = values[expr.name]
                # TODO: Is it really only FixedValues?
                #       If so, update annotations. Otherwise, code is incorrect.
                assert isinstance(value, FixedValue), value
                return value.expr
            return None

        for name, ref in entry.value_placeholders.items():
            values[name] = ref.substitute(resolve_immediate).bits

        subs = {name: submatch.subst_pc(pc_val) for name, submatch in self._subs.items()}

        return ModeMatch(entry, values, subs)

    # TODO: This property has a lot of overlap with Mnemonic.fill_placeholders().
    @const_property
    def mnemonic(self) -> Iterator[str | FixedValueReference]:
        entry = self._entry
        subs = self._subs
        values = self._values

        def fill_immediate(expr: Expression) -> Expression | None:
            if isinstance(expr, ImmediateValue):
                value = values[expr.name]
                assert isinstance(value, FixedValue), value
                return value.expr
            return None

        for mnem_elem in entry.mnemonic:
            match mnem_elem:
                case MatchPlaceholder(name=name):
                    yield from subs[name].mnemonic
                case FixedValueReference() as elem:
                    yield elem.substitute(fill_immediate)
                case str() as elem:
                    yield elem
                case elem:
                    bad_type(elem)


def _format_encoding_width(width: Width | None) -> str:
    return "empty encoding" if width is None else f"encoding width {width}"


def _format_aux_encoding_width(width: Width | None) -> str:
    return (
        "no auxiliary encoding items" if width is None else f"auxiliary encoding width {width}"
    )


class ModeTable:
    """Abstract base class for mode tables."""

    @property
    def encoding_width(self) -> Width | None:
        return self._enc_width

    @property
    def aux_encoding_width(self) -> Width | None:
        return self._aux_enc_width

    @property
    def entries(self) -> Sequence[ModeEntry]:
        return self._entries

    def __init__(
        self, enc_width: Width | None, aux_enc_width: Width | None, entries: Iterable[ModeEntry]
    ):
        self._enc_width = enc_width
        self._aux_enc_width = aux_enc_width
        self._entries = entries = tuple(entries)

        for entry in entries:
            enc_def = entry.encoding
            if enc_def.encoding_width != enc_width:
                raise ValueError(
                    f"mode with {_format_encoding_width(enc_width)} contains "
                    f"entry with {_format_encoding_width(enc_def.encoding_width)}"
                )
            if enc_def.aux_encoding_width not in (None, aux_enc_width):
                raise ValueError(
                    f"mode with {_format_aux_encoding_width(aux_enc_width)}"
                    " contains entry with "
                    f"{_format_aux_encoding_width(enc_def.aux_encoding_width)}"
                )

    @const_property
    def encoded_length(self) -> int | None:
        """
        The number of encoded data units (bytes, words etc.) that all
        entries in this mode use, or None if that number may vary depending
        on which match is made.
        """
        if self._enc_width is None:
            return 0
        if self._aux_enc_width is None:
            return 1
        common_len: int | None = None
        for entry in self._entries:
            entry_len = entry.encoding.encoded_length
            if entry_len is None:
                return None
            if entry_len != common_len:
                if common_len is None:
                    common_len = entry_len
                else:
                    return None
        assert common_len is not None, self
        return common_len


class Mode(ModeTable):
    """
    A pattern for operands, such as an addressing mode or a table defining
    register encoding.
    """

    @property
    def name(self) -> str:
        return self._name

    @property
    def semantics_type(self) -> IntType | ReferenceType:
        return self._sem_type

    @property
    def location(self) -> InputLocation:
        return self._location

    def __init__(
        self,
        name: str,
        enc_width: int | None,
        aux_enc_width: int | None,
        sem_type: IntType | ReferenceType,
        location: InputLocation,
        entries: Iterable[ModeEntry],
    ):
        ModeTable.__init__(self, enc_width, aux_enc_width, entries)
        self._name = name
        self._sem_type = sem_type
        self._location = location

    @override
    def __str__(self) -> str:
        return f"mode {self._sem_type} {self._name}"

    @override
    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self._name!r})"


def _rename_immediate(expr: Expression, name_map: Mapping[str, str]) -> Expression | None:
    if isinstance(expr, ImmediateValue):
        if (new_name := name_map.get(expr.name)) is not None:
            return ImmediateValue(new_name, expr.type)
    return None


@dataclass(frozen=True)
class MatchPlaceholder:
    """
    An element from a mode context that will be filled in by a match made
    in a different mode table.
    """

    name: str
    mode: Mode
    location: InputLocation | None = None

    @property
    def encoding_width(self) -> Width | None:
        """
        The number of bits used to encode this placeholder,
        or ``None`` if this placeholder is not encoded.
        """
        return self.mode.encoding_width

    @override
    def __str__(self) -> str:
        return f"{{{self.mode.name} {self.name}}}"

    def rename(self, name_map: Mapping[str, str]) -> MatchPlaceholder:
        return MatchPlaceholder(name_map[self.name], self.mode, self.location)


class EncodeMatch:
    """A match on the encoding field of a mode entry."""

    @property
    def entry(self) -> ModeEntry:
        return self._entry

    def __init__(self, entry: ModeEntry):
        self._entry = entry
        self._subs: dict[str, EncodeMatch] = {}
        self._values: dict[str, FixedValueReference] = {}

    @override
    def __repr__(self) -> str:
        return f"EncodeMatch({self._entry!r}, {self._subs!r}, {self._values!r})"

    def add_submatch(self, name: str, submatch: EncodeMatch) -> None:
        assert name not in self._subs, name
        assert name not in self._values, name
        self._subs[name] = submatch

    def add_value(self, name: str, value: FixedValueReference) -> None:
        assert name not in self._subs, name
        assert name not in self._values, name
        self._values[name] = value

    def get_submatch(self, name: str) -> EncodeMatch:
        return self._subs[name]

    def get_value(self, name: str) -> FixedValueReference:
        return self._values[name]

    def complete(self) -> ModeMatch:
        """Construct a ModeMatch using the captured data."""

        subs = {name: value.complete() for name, value in self._subs.items()}
        values = {name: value.bits for name, value in self._values.items()}

        def resolve_immediate(expr: Expression) -> Expression | None:
            if isinstance(expr, ImmediateValue):
                return values[expr.name].expr
            return None

        for name, ref in self._entry.value_placeholders.items():
            values[name] = ref.substitute(resolve_immediate).bits

        return ModeMatch(self._entry, values, subs)

    def fill_placeholders(self) -> ModeEntry:
        """
        Return a new entry, in which those placeholders that are present
        in this match are replaced by the mode/value they are mapped to.
        It is not necessary for the match to provide modes/values for every
        placeholder: whatever is not matched is left untouched.
        """

        entry = self._entry
        subs = self._subs
        values = self._values
        if not subs and not values:
            # Skip no-op substitution for efficiency's sake.
            return entry

        encoding = entry.encoding.fill_placeholders(self)
        mnemonic = entry.mnemonic.fill_placeholders(self)
        # TODO: Fill in placeholders in semantics too.
        semantics = entry.semantics
        unfilled_matches = (
            placeholder
            for placeholder in entry.match_placeholders
            if placeholder.name not in subs
        )
        unfilled_values = {
            name: ref for name, ref in entry.value_placeholders.items() if name not in values
        }

        return ModeEntry(encoding, mnemonic, semantics, unfilled_matches, unfilled_values)

    @const_property
    def flags_required(self) -> Set[str]:
        flags_required = self._entry.encoding.flags_required
        for match in self._subs.values():
            flags_required |= match.entry.encoding.flags_required
        return flags_required

    @const_property
    def encoded_length(self) -> int:
        enc_def = self._entry.encoding
        length = enc_def.encoded_length
        if length is not None:
            # Mode entry has fixed encoded length.
            return length

        # Mode entry has variable encoded length.
        subs = self._subs
        length = 0
        for enc_item in enc_def:
            match enc_item:
                case EncodingExpr():
                    length += 1
                case EncodingMultiMatch(name=name, start=start):
                    length += subs[name].encoded_length - start
                case obj:
                    bad_type(obj)
        return length
