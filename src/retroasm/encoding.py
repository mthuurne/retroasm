from __future__ import annotations

from collections import defaultdict
from collections.abc import Callable, Iterable, Iterator, Mapping, Sequence, Set
from dataclasses import dataclass
from functools import partial
from operator import itemgetter
from typing import TYPE_CHECKING, Final, Self, cast, override

from .expression import Expression, IntLiteral
from .input import BadInput, ErrorCollector, InputLocation
from .reference import (
    BitString,
    FixedValue,
    FixedValueReference,
    SingleStorage,
    TabooReference,
    Variable,
)
from .symbol import ImmediateValue, rename_immediate
from .types import IntType, Segment, Width
from .utils import bad_type, const_property

if TYPE_CHECKING:
    from .decode import EncodeMatch
    from .mode import Mode


class ModeMatchReference(TabooReference):
    __slots__ = ("mode",)

    def __init__(self, name: str, mode: Mode):
        super().__init__(
            # The type doesn't matter, as the fetch will be rejected,
            # but we must provide something.
            IntType.int,
            f'mode match placeholder "{name}" cannot be used in context value',
        )
        self.mode = mode


@dataclass(frozen=True, order=True)
class EncodedSegment:
    """Part of an instruction encoding."""

    enc_idx: int
    """Index of encoding unit that contains the segment."""

    segment: Segment
    """Bit range."""

    @override
    def __str__(self) -> str:
        return f"enc{self.enc_idx:d}{self.segment}"

    def adjust_unit(self, idx: int, adjust: int) -> EncodedSegment:
        """
        Adjust the encoding unit index by 'adjust' if the current index
        is below 'idx'.
        Return the adjusted segment.
        """
        enc_idx = self.enc_idx
        if enc_idx < idx:
            return self
        else:
            return EncodedSegment(enc_idx + adjust, self.segment)


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
        new_bits = bits.substitute(expression_func=partial(rename_immediate, name_map=name_map))
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


type EncodingItem = EncodingExpr | EncodingMultiMatch


@dataclass(frozen=True, order=True)
class FixedEncoding:
    """Constant part of an instruction encoding."""

    enc_idx: int
    fixed_mask: int
    fixed_value: int


def decompose_encoding(
    encoding_items: Iterable[EncodingItem],
) -> tuple[Sequence[FixedEncoding], Mapping[str, Sequence[tuple[int, EncodedSegment]]]]:
    """
    Decomposes the given Encoding into a matcher for the fixed bit strings
    and a decode map describing where the placeholders values can be found.
    Returns a pair of the fixed matcher and the decode map.
    The fixed matcher is a sequence of tuples, where each tuple contains the
    index in the encoding followed by the mask and value of the fixed bits
    pattern at that index.
    The decode map stores a sequence of tuples for each name, where each tuple
    contains the index in the placeholder's argument storage, the index of
    the encoding item, the index within the encoding item and the width.
    Raises BadInput if the given encoding cannot be decomposed.
    """
    fixed_matcher: list[FixedEncoding] = []
    decode_map = defaultdict[str, list[tuple[int, EncodedSegment]]](list)
    for enc_idx, enc_elem in enumerate(encoding_items):
        if not isinstance(enc_elem, EncodingExpr):
            continue
        fixed_mask = 0
        fixed_value = 0
        try:
            start = 0
            for base, base_seg in enc_elem.bits.decompose():
                segment = Segment(start, base_seg.width)
                match base:
                    case FixedValue(expr=IntLiteral(value=value)):
                        fixed_mask |= segment.mask
                        fixed_value |= base_seg.cut(value) << start
                    case FixedValue(expr=ImmediateValue(name=name)):
                        decode_map[name].append(
                            (base_seg.start, EncodedSegment(enc_idx, segment))
                        )
                    case FixedValue(expr=expr):
                        if (op := getattr(expr, "operator", None)) is not None:
                            raise ValueError(f"unsupported operator in encoding: {op}")
                        raise ValueError(
                            f"unsupported expression type in encoding: "
                            f"{expr.__class__.__name__}"
                        )
                    case SingleStorage(storage=storage):
                        raise ValueError(
                            f"unsupported storage type in encoding: "
                            f"{storage.__class__.__name__}"
                        )
                    case Variable():
                        raise ValueError("local variable cannot be used in encoding")
                    case base:
                        bad_type(base)
                # Note: It is possible for the width to be unlimited, but only
                #       at the end of a string, in which case we don't use
                #       the start offset anymore.
                start += cast(int, base_seg.width)
        except ValueError as ex:
            # TODO: This message is particularly unclear, because we do not
            #       have the exact location nor can we print the offending
            #       expression.
            #       We could store locations in non-simplified expressions
            #       or decompose parse trees instead of references.
            raise BadInput(str(ex), enc_elem.location) from ex
        else:
            if fixed_mask != 0:
                fixed_matcher.append(FixedEncoding(enc_idx, fixed_mask, fixed_value))
    return fixed_matcher, decode_map


def _calc_decoding(
    encoding_items: Sequence[EncodingItem],
    encoding_location: InputLocation | None,
    placeholders: Mapping[str, FixedValueReference],
    collector: ErrorCollector,
) -> tuple[Sequence[FixedEncoding], Mapping[str, Sequence[EncodedSegment]]]:
    """
    Construct a mapping that, given an encoded (part of an) instruction, produces the values
    for context placeholders.
    """

    with collector.check():
        try:
            # Decompose the encoding expressions.
            fixed_matcher, decode_map = decompose_encoding(encoding_items)
        except BadInput as ex:
            collector.add(ex)

    multi_matches = {
        enc_item.name for enc_item in encoding_items if isinstance(enc_item, EncodingMultiMatch)
    }

    with collector.check():
        # Check placeholders that should be encoded but aren't.
        # TODO: This is something we should check at the mode entry level,
        #       instead of at the encoding level, such that the encoding
        #       does not depend on the context.
        for name, ref in placeholders.items():
            # Skip placeholders that should not be encoded.
            match ref:
                case ModeMatchReference(mode=mode):
                    if mode.encoding_width is None:
                        continue
                    if name in multi_matches:
                        # Mode is matched using "X@" syntax.
                        continue
                case FixedValueReference(expr=ImmediateValue(name=imm_name)) if (
                    imm_name == name
                ):
                    pass
                case _:
                    continue

            if name not in decode_map:
                # Note: We could instead treat missing placeholders as a special
                #       case of insufficient bit coverage, but having a dedicated
                #       error message is more clear for end users.
                collector.error(
                    f'placeholder "{name}" does not occur in encoding',
                    location=encoding_location,
                )
            elif isinstance(ref, ModeMatchReference):
                if (mode := ref.mode).aux_encoding_width is not None:
                    collector.error(
                        f'mode "{mode.name}" matches auxiliary encoding units, '
                        f'but there is no "{name}@" placeholder for them',
                        location=encoding_location,
                    )

        # Create a mapping to extract immediate values from encoded items.
        sequential_map = dict(
            _combine_placeholder_encodings(
                decode_map, encoding_items, collector, encoding_location
            )
        )

    # Check whether unknown-length multi-matches are blocking decoding.
    match_placeholders = {
        name: ref.mode
        for name, ref in placeholders.items()
        if isinstance(ref, ModeMatchReference)
    }
    with collector.check():
        _check_decoding_order(encoding_items, sequential_map, match_placeholders, collector)

    return fixed_matcher, sequential_map


def _combine_placeholder_encodings(
    decode_map: Mapping[str, Sequence[tuple[int, EncodedSegment]]],
    encoding_items: Sequence[EncodingItem],
    collector: ErrorCollector,
    location: InputLocation | None,
) -> Iterator[tuple[str, Sequence[EncodedSegment]]]:
    """
    Yield pairs of placeholder name and the locations where the placeholder
    resides in the encoded items.
    Each such location is a triple of index in the encoded items, bit offset
    within that item and width in bits.
    """

    imm_widths = {}
    for enc_item in encoding_items:
        if isinstance(enc_item, EncodingExpr):
            for expr in enc_item.bits.iter_expressions():
                if isinstance(expr, ImmediateValue):
                    imm_widths[expr.name] = expr.type.width

    for name, slices in decode_map.items():
        imm_width = imm_widths[name]
        decoding = []
        problems = []
        prev: Width = 0
        for imm_idx, enc_segment in sorted(slices):
            width = enc_segment.segment.width
            if prev < imm_idx:
                problems.append(f"gap at [{prev:d}:{imm_idx:d}]")
            elif prev > imm_idx:
                problems.append(f"overlap at [{imm_idx:d}:{min(imm_idx + width, prev)}]")
            prev = max(imm_idx + width, prev)
            decoding.append(enc_segment)
        if prev < imm_width:
            problems.append(f"gap at [{prev:d}:{imm_width:d}]")
        if problems:
            collector.error(
                f'cannot decode value for "{name}": {", ".join(problems)}', location=location
            )
        else:
            yield name, tuple(decoding)


def _check_decoding_order(
    encoding_items: Sequence[EncodingItem],
    sequential_map: Mapping[str, Sequence[EncodedSegment]],
    match_placeholders: Mapping[str, Mode],
    collector: ErrorCollector,
) -> None:
    """
    Verifies that there is an order in which placeholders can be decoded.
    Such an order might not exist because of circular dependencies.
    """

    # Find indices of multi-matches.
    multi_match_indices = {
        enc_elem.name: enc_idx
        for enc_idx, enc_elem in enumerate(encoding_items)
        if isinstance(enc_elem, EncodingMultiMatch)
    }

    for name, decoding in sequential_map.items():
        # Are we dealing with a multi-match of unknown length?
        multi_idx = multi_match_indices.get(name)
        if multi_idx is None:
            continue
        matcher = encoding_items[multi_idx]
        if matcher.encoded_length is not None:
            continue

        # Are any parts of the placeholder are located after the multi-match?
        bad_idx = [
            enc_segment.enc_idx for enc_segment in decoding if enc_segment.enc_idx > multi_idx
        ]
        if bad_idx:
            mode = match_placeholders[name]
            collector.error(
                f'cannot match "{name}": mode "{mode.name}" has a variable encoding '
                f'length and (parts of) the placeholder "{name}" are placed after '
                f'the multi-match placeholder "{name}@"',
                location=[matcher.location] + [encoding_items[idx].location for idx in bad_idx],
            )


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


class Encoding:
    """
    Defines how (part of) an instruction is encoded.
    We call the elements of a definition 'items', these are represented by
    EncodingExpr and EncodingMultiMatch objects. We call the elements of an
    encoded instruction 'units', depending on the instruction set these are
    bytes or words or some other fixed-size bit strings.
    The items within an encoding definition are exposed as a sequence.
    """

    @property
    def items(self) -> Sequence[EncodingItem]:
        return self._items

    @classmethod
    def create(
        cls,
        items: Sequence[EncodingItem],
        flags_required: Iterable[str],
        placeholders: Mapping[str, FixedValueReference],
        collector: ErrorCollector,
        location: InputLocation | None = None,
    ) -> Self:
        """
        Create an encoding object and computes the corresponding decoding.
        If decoding is not possible, that's reported as an error.
        Raises `DelayedError` if any errors were logged on the given collector.
        """

        decoding = _calc_decoding(items, location, placeholders, collector)
        return cls(items, flags_required, *decoding, location)

    def __init__(
        self,
        items: Iterable[EncodingItem],
        flags_required: Iterable[str],
        fixed_matcher: Sequence[FixedEncoding],
        decoding: Mapping[str, Sequence[EncodedSegment]],
        location: InputLocation | None = None,
    ):
        # Filter out zero-length encoding items.
        # There is no good reason to ban them, but keeping them around would
        # unnecessarily complicate the code.
        non_empty_items: Sequence[EncodingItem] = tuple(
            item for item in items if item.encoded_length != 0
        )
        self._items = non_empty_items
        self._first_aux_index = _find_first_aux_index(non_empty_items)

        self.fixed_matcher = fixed_matcher
        self.decoding = decoding

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

    def fill_placeholders(self, match: EncodeMatch) -> Encoding:
        """
        Return a new encoding, in which placeholders are replaced by
        match results, if available.
        """

        # In the case of multi-matches, we might need a filled submode encoding
        # multiple times, so cache them.
        sub_encodings: dict[str, Encoding] = {}

        def get_sub_encoding(name: str, submatch: EncodeMatch) -> Sequence[EncodingItem]:
            try:
                return sub_encodings[name].items
            except KeyError:
                sub_enc = submatch.entry.encoding.fill_placeholders(submatch)
                sub_encodings[name] = sub_enc
                return sub_enc.items

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

        # TODO: Update the decoding as well.
        fixed_matcher = self.fixed_matcher
        decoding = self.decoding

        return Encoding(items, match.flags_required, fixed_matcher, decoding, self._location)

    def rename(self, name_map: Mapping[str, str]) -> Encoding:
        """
        Returns a new Encoding, in which all placeholder names are
        substituted by their value in the given mapping.
        """
        return Encoding(
            (item.rename(name_map) for item in self._items),
            self.flags_required,
            self.fixed_matcher,
            # TODO: Rename immediates in decoding as well.
            self.decoding,
            self._location,
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
    def location(self) -> InputLocation | None:
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


def _format_encoding_width(width: Width | None) -> str:
    return "empty" if width is None else f"{width} bits wide"


def determine_encoding_width(
    encodings: Iterable[Encoding], aux: bool, where_desc: str, collector: ErrorCollector
) -> int | None:
    """
    Return the common encoding width for the given list of mode entries.
    Entries with a deviating encoding width will be logged as errors on the given logger.
    If the 'aux' argument is False, the first matched unit width of each entry is checked,
    otherwise the width of auxiliary encoding units is checked.
    """

    width_attr = "aux_encoding_width" if aux else "encoding_width"

    width_freqs = defaultdict[int | None, int](int)
    for encoding in encodings:
        width_freqs[getattr(encoding, width_attr)] += 1
    if aux:
        width_freqs.pop(None, None)

    if len(width_freqs) == 0:
        # Empty mode, only errors or aux check with no aux items.
        enc_width = None
    elif len(width_freqs) == 1:
        # Single type.
        (enc_width,) = width_freqs.keys()
    else:
        # Multiple widths; use one with the maximum frequency.
        enc_width, _ = max(width_freqs.items(), key=itemgetter(1))
        valid_widths: Iterable[Width | None]
        if aux:
            valid_widths = (enc_width, None)
        else:
            valid_widths = (enc_width,)
        for encoding in encodings:
            width: Width = getattr(encoding, width_attr)
            if width not in valid_widths:
                match_type = f"{'auxiliary ' if aux else ''}encoding match"
                actual_width = _format_encoding_width(width)
                expected_width = _format_encoding_width(enc_width)
                collector.error(
                    f"{match_type} is {actual_width}, while {expected_width} is "
                    f"dominant {where_desc}",
                    location=(
                        encoding.aux_encoding_location if aux else encoding.encoding_location
                    ),
                )

    return enc_width
