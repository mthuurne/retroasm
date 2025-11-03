from __future__ import annotations

from collections.abc import Iterable, Iterator, Mapping, Sequence, Set
from dataclasses import dataclass
from functools import partial
from itertools import chain
from typing import TYPE_CHECKING, Self, override

from .codeblock import FunctionBody
from .encoding import Encoding, EncodingExpr, EncodingMultiMatch, determine_encoding_width
from .expression import Expression
from .input import ErrorCollector, InputLocation
from .reference import BitString, FixedValue, FixedValueReference
from .symbol import CurrentAddress, ImmediateValue, rename_immediate
from .types import IntType, ReferenceType, Width
from .utils import bad_type, const_property

if TYPE_CHECKING:
    from .decode import EncodeMatch


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
                        items += submatch.mode_row.mnemonic.fill_placeholders(submatch)
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
                    return item.substitute(partial(rename_immediate, name_map=name_map))
                case str():
                    return item
                case _:
                    bad_type(item)

        return Mnemonic(rename_item(item) for item in self._items)


class ModeRow:
    """One row in a mode table."""

    def __init__(
        self,
        encoding: Encoding,
        mnemonic: Mnemonic,
        semantics: FunctionBody | None,
        match_placeholders: Mapping[str, Mode],
        value_placeholders: Mapping[str, FixedValueReference],
    ):
        self.encoding = encoding
        self.mnemonic = mnemonic
        self._semantics = semantics
        self._match_placeholders = dict(match_placeholders)
        self._value_placeholders = dict(value_placeholders)

    @override
    def __repr__(self) -> str:
        return (
            f"ModeRow({self.encoding!r}, {self.mnemonic!r}, "
            f"{self._semantics!r}, {self._match_placeholders!r}, "
            f"{self._value_placeholders!r})"
        )

    @property
    def match_placeholders(self) -> Mapping[str, Mode]:
        return self._match_placeholders

    @property
    def value_placeholders(self) -> Mapping[str, FixedValueReference]:
        return self._value_placeholders

    @property
    def semantics(self) -> FunctionBody:
        """
        The semantics of this mode row.
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

    def rename(self, name_map: Mapping[str, str]) -> ModeRow:
        """
        Return a new row, in which all placeholder names are substituted by their value
        in the given mapping.
        """
        return ModeRow(
            self.encoding.rename(name_map),
            self.mnemonic.rename(name_map),
            # TODO: Rename immediates in semantics as well.
            self._semantics,
            {name_map[name]: mode for name, mode in self._match_placeholders.items()},
            {
                name_map[name]: ref.substitute(partial(rename_immediate, name_map=name_map))
                for name, ref in self._value_placeholders.items()
            },
        )

    def qualify_names(self, branch_name: str | None) -> ModeRow:
        """
        Return a renamed version of this mode row where each name starts with the given
        branch name.
        If `branch_name` is `None`, this mode row is returned unchanged.
        """

        if branch_name is None:
            return self

        match list(chain(self._match_placeholders, self._value_placeholders)):
            case []:
                # Nothing to rename.
                return self
            case [name]:
                # Replace current name with branch name.
                name_map = {name: branch_name}
            case names:
                # Prefix current names with branch name.
                name_map = {name: f"{branch_name}.{name}" for name in names}

        return self.rename(name_map)


class ModeMatch:
    """
    A flattened match of a mode row at a particular address.
    Flattened means that all submode matches have been resolved and substituted
    into this match.
    """

    __slots__ = ("_row", "_values", "_subs", "_mnemonic")

    def __init__(
        self, row: ModeRow, values: Mapping[str, BitString], subs: Mapping[str, ModeMatch]
    ):
        self._row = row
        self._values = values
        self._subs = subs

    @override
    def __repr__(self) -> str:
        return f"ModeMatch({self._row!r}, {self._values!r}, {self._subs!r})"

    @property
    def flags_required(self) -> Set[str]:
        """The prefix flags that must be set to match this mode row."""
        flags = self._row.encoding.flags_required
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

        for enc_item in self._row.encoding.items:
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

        row = self._row
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

        for name, ref in row.value_placeholders.items():
            values[name] = ref.substitute(resolve_immediate).bits

        subs = {name: submatch.subst_pc(pc_val) for name, submatch in self._subs.items()}

        return ModeMatch(row, values, subs)

    # TODO: This property has a lot of overlap with Mnemonic.fill_placeholders().
    @const_property
    def mnemonic(self) -> Iterator[str | FixedValueReference]:
        row = self._row
        subs = self._subs
        values = self._values

        def fill_immediate(expr: Expression) -> Expression | None:
            if isinstance(expr, ImmediateValue):
                value = values[expr.name]
                assert isinstance(value, FixedValue), value
                return value.expr
            return None

        for mnem_elem in row.mnemonic:
            match mnem_elem:
                case MatchPlaceholder(name=name):
                    yield from subs[name].mnemonic
                case FixedValueReference() as elem:
                    yield elem.substitute(fill_immediate)
                case str() as elem:
                    yield elem
                case elem:
                    bad_type(elem)


class ModeTable:
    """Abstract base class for mode tables."""

    @property
    def encoding_width(self) -> Width | None:
        return self._enc_width

    @property
    def aux_encoding_width(self) -> Width | None:
        return self._aux_enc_width

    @property
    def rows(self) -> Sequence[ModeRow]:
        return self._rows

    def __init__(
        self, enc_width: Width | None, aux_enc_width: Width | None, rows: Iterable[ModeRow]
    ):
        self._enc_width = enc_width
        self._aux_enc_width = aux_enc_width
        self._rows = tuple(rows)

    @const_property
    def encoded_length(self) -> int | None:
        """
        The number of encoded data units (bytes, words etc.) that all rows in this mode use,
        or `None` if that number may vary depending on which match is made.
        """
        if self._enc_width is None:
            return 0
        if self._aux_enc_width is None:
            return 1
        common_len: int | None = None
        for row in self._rows:
            row_len = row.encoding.encoded_length
            if row_len is None:
                return None
            if row_len != common_len:
                if common_len is None:
                    common_len = row_len
                else:
                    return None
        assert common_len is not None, self
        return common_len


class Mode(ModeTable):
    """
    A pattern for operands, such as an addressing mode or a table defining register encoding.
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

    @classmethod
    def create(
        cls,
        name: str,
        sem_type: IntType | ReferenceType,
        location: InputLocation,
        rows: Iterable[ModeRow],
        collector: ErrorCollector,
    ) -> Self:
        encodings = [row.encoding for row in rows]
        where_desc = f'in mode "{name}"'
        enc_width = determine_encoding_width(encodings, False, where_desc, collector)
        aux_enc_width = determine_encoding_width(encodings, True, where_desc, collector)
        return cls(name, enc_width, aux_enc_width, sem_type, location, rows)

    def __init__(
        self,
        name: str,
        enc_width: int | None,
        aux_enc_width: int | None,
        sem_type: IntType | ReferenceType,
        location: InputLocation,
        rows: Iterable[ModeRow],
    ):
        """
        Generally you should be using `create()` instead of calling the constructor directly,
        to get a more streamlined interface and consistency checks.
        """
        ModeTable.__init__(self, enc_width, aux_enc_width, rows)
        self._name = name
        self._sem_type = sem_type
        self._location = location

    @override
    def __str__(self) -> str:
        return f"mode {self._sem_type} {self._name}"

    @override
    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self._name!r})"


@dataclass(frozen=True)
class MatchPlaceholder:
    """
    An element from a mode context that will be filled in by a match made
    in a different mode table.
    """

    name: str
    mode: Mode

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
        return MatchPlaceholder(name_map[self.name], self.mode)
