from __future__ import annotations

from abc import ABC, abstractmethod
from collections import defaultdict
from collections.abc import Callable, Iterable, Iterator, Mapping, Sequence
from dataclasses import dataclass
from typing import cast, override

from .codeblock import FunctionBody
from .expression import IntLiteral
from .fetch import AfterModeFetcher, Fetcher, ModeFetcher
from .input import BadInput, ErrorCollector, InputLocation
from .mode import (
    EncodeMatch,
    Encoding,
    EncodingExpr,
    EncodingItem,
    EncodingMultiMatch,
    MatchPlaceholder,
    Mode,
    ModeEntry,
    ModeMatchReference,
    get_encoding_width,
)
from .reference import FixedValue, FixedValueReference, SingleStorage, Variable, int_reference
from .symbol import ImmediateValue
from .types import IntType, Segment, Width, mask_for_width, mask_to_segments
from .utils import SingletonFromABC, bad_type


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


@dataclass(frozen=True, order=True)
class FixedEncoding:
    """Constant part of an instruction encoding."""

    enc_idx: int
    fixed_mask: int
    fixed_value: int


def _decompose_encoding(
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


def _format_mask(name: str, mask: int, value: int | None = None) -> str:
    segments = list(mask_to_segments(mask))
    if len(segments) == 1:
        (segment,) = segments
        if value is None:
            return f"{name}{segment}"
        else:
            return f"{name}{segment}=${(value & mask) >> segment.start:x}"
    else:
        digits = []
        while mask:
            if mask & 1 == 0:
                digits.append("x")
            elif value is None:
                digits.append("?")
            else:
                digits.append(str(value & 1))
            mask >>= 1
            if value is not None:
                value >>= 1
        while not digits or len(digits) % 4 != 0:
            digits.append("x")
        digits.reverse()
        return (
            name + ":" + "_".join("".join(digits[i : i + 4]) for i in range(0, len(digits), 4))
        )


class Decoder(ABC):
    @abstractmethod
    def dump(self, indent: str = "", submodes: bool = True) -> None: ...

    @abstractmethod
    def try_decode(self, fetcher: Fetcher) -> EncodeMatch | None:
        """
        Attempts to decode an instruction from the given fetcher.
        Returns an encode match, or None if no match could be made.
        """


class SequentialDecoder(Decoder):
    """Decoder that tries other decoders in order, until the first match."""

    def __init__(self, decoders: Iterable[Decoder]):
        self._decoders = tuple(decoders)

    @override
    def dump(self, indent: str = "", submodes: bool = True) -> None:
        for decoder in self._decoders:
            decoder.dump(indent + "+ ", submodes)
            indent = " " * len(indent)

    @override
    def try_decode(self, fetcher: Fetcher) -> EncodeMatch | None:
        for decoder in self._decoders:
            match = decoder.try_decode(fetcher)
            if match is not None:
                return match
        return None


class TableDecoder(Decoder):
    """Decoder that performs a table lookup to find the next decoder to try."""

    def __init__(self, table: Iterable[Decoder], index: int, mask: int, offset: int):
        self._table = tuple(table)
        self._index = index
        self._mask = mask
        self._offset = offset
        assert (mask >> offset) == len(self._table) - 1

    @override
    def dump(self, indent: str = "", submodes: bool = True) -> None:
        name = f"enc{self._index:d}"
        print(indent + _format_mask(name, self._mask & (-1 << self._offset)))
        for idx, decoder in enumerate(self._table):
            decoder.dump(" " * len(indent) + f"${idx:02x}: ", submodes)

    @override
    def try_decode(self, fetcher: Fetcher) -> EncodeMatch | None:
        encoded = fetcher[self._index]
        if encoded is None:
            return None
        decoder = self._table[(encoded & self._mask) >> self._offset]
        return decoder.try_decode(fetcher)


class FixedPatternDecoder(Decoder):
    """
    Decoder that matches encoded bit strings by looking for a fixed pattern
    using a mask and value.
    """

    @property
    def index(self) -> int:
        return self._index

    @property
    def mask(self) -> int:
        return self._mask

    @property
    def value(self) -> int:
        return self._value

    @property
    def next(self) -> Decoder:
        return self._next

    @classmethod
    def create(cls, index: int, mask: int, value: int, nxt: Decoder) -> FixedPatternDecoder:
        match nxt:
            case FixedPatternDecoder(index=nindex, value=nvalue, mask=nmask, next=nnxt) if (
                nindex == index
            ):
                # Combine two masks checks into one decoder.
                assert mask & nmask == 0
                return cls(index, mask | nmask, value | nvalue, nnxt)
            case nxt:
                return cls(index, mask, value, nxt)

    def __init__(self, index: int, mask: int, value: int, nxt: Decoder):
        self._index = index
        self._mask = mask
        self._value = value
        self._next = nxt

    @override
    def dump(self, indent: str = "", submodes: bool = True) -> None:
        mask_str = _format_mask(f"enc{self._index:d}", self._mask, self._value)
        self._next.dump(indent + mask_str + " -> ", submodes)

    @override
    def try_decode(self, fetcher: Fetcher) -> EncodeMatch | None:
        encoded = fetcher[self._index]
        if encoded is None or encoded & self._mask != self._value:
            return None
        else:
            return self._next.try_decode(fetcher)


class PlaceholderDecoder(Decoder):
    """Decodes the value for one placeholder."""

    def __init__(
        self,
        name: str,
        encoded_segments: Sequence[EncodedSegment] | None,
        nxt: Decoder,
        sub: Decoder | None,
        aux_idx: int | None,
    ):
        self._name = name
        self._encoded_segments = encoded_segments
        self._next = nxt
        self._sub = sub
        self._aux_idx = aux_idx

    @override
    def dump(self, indent: str = "", submodes: bool = True) -> None:
        encoded_segments = self._encoded_segments
        if encoded_segments is None:
            enc_str = ""
        else:
            enc_str = ";".join(str(enc_seg) for enc_seg in reversed(encoded_segments))

        sub = self._sub
        if sub is None:
            val_str = enc_str
        else:
            items = [enc_str] if enc_str else []
            aux_idx = self._aux_idx
            if aux_idx is not None:
                items.append(f"enc{aux_idx:d}+")
            val_str = f"sub({', '.join(items)})"

        name = self._name
        self._next.dump(indent + f"{name}={val_str}, ", submodes)
        if submodes and sub is not None:
            sub.dump((len(indent) + len(name)) * " " + "`-> ")

    @override
    def try_decode(self, fetcher: Fetcher) -> EncodeMatch | None:
        encoded_segments = self._encoded_segments
        if encoded_segments is None:
            value = None
        else:
            # Compose encoded first value.
            value = 0
            offset = 0
            for enc_seg in encoded_segments:
                encoded = fetcher[enc_seg.enc_idx]
                if encoded is None:
                    return None
                value |= enc_seg.segment.cut(encoded) << offset
                width = enc_seg.segment.width
                assert isinstance(width, int), enc_seg
                offset += width

        # Decode placeholder.
        decoded: EncodeMatch | FixedValueReference | None
        sub = self._sub
        if sub is None:
            if value is None:
                decoded = None
            else:
                # TODO: We support signed placeholders in encoding as well.
                decoded = int_reference(value, IntType.u(offset))
        else:
            aux_idx = self._aux_idx
            decoded = sub.try_decode(ModeFetcher(value, fetcher, aux_idx))
            if decoded is None:
                return None
            if aux_idx is not None:
                # Note: When there are encoded segments, the multi-matcher
                #       won't match the first unit, since they have been
                #       matched by single matcher(s) already.
                delta = decoded.encoded_length - (1 if encoded_segments is None else 2)
                if delta != 0:
                    fetcher = AfterModeFetcher(fetcher, aux_idx, delta)

        # Decode remainder.
        match = self._next.try_decode(fetcher)
        if match is not None:
            match decoded:
                case EncodeMatch() as submatch:
                    match.add_submatch(self._name, submatch)
                case FixedValueReference() as ref:
                    match.add_value(self._name, ref)
                case None:
                    assert False
        return match


class MatchFoundDecoder(Decoder):
    """
    Decoder that is placed at the end of a decode tree.
    It always finds a match.
    """

    @property
    def entry(self) -> ModeEntry:
        return self._entry

    def __init__(self, entry: ModeEntry):
        self._entry = entry

    @override
    def dump(self, indent: str = "", submodes: bool = True) -> None:
        print(indent + " ".join(str(m) for m in self._entry.mnemonic))

    @override
    def try_decode(self, fetcher: Fetcher) -> EncodeMatch:
        return EncodeMatch(self._entry)


class NoMatchDecoder(Decoder, metaclass=SingletonFromABC):
    """
    Decoder that is placed at the end of a decode tree.
    It never finds a match.
    """

    @override
    def dump(self, indent: str = "", submodes: bool = True) -> None:
        print(indent + "(none)")

    @override
    def try_decode(self, fetcher: Fetcher) -> None:
        return None


type _EncodingMatcher = MatchPlaceholder | EncodingMultiMatch | FixedEncoding


def _create_entry_decoder(
    entry: ModeEntry,
    decoding: Mapping[str, Sequence[EncodedSegment]],
    fixed_matcher: Sequence[FixedEncoding],
    factory: DecoderFactory,
) -> Decoder:
    """Returns a Decoder instance that decodes this entry."""

    encoding = entry.encoding

    # Find all indices that contain multi-matches.
    multi_matches = {
        enc_item.name: enc_idx
        for enc_idx, enc_item in enumerate(encoding.items)
        if isinstance(enc_item, EncodingMultiMatch)
    }

    # Match placeholders that are not represented in the encoding.
    # Typically these are matched on decode flags.
    match = EncodeMatch(entry)
    for name, mode in entry.match_placeholders.items():
        if name not in decoding and name not in multi_matches:
            match factory.create_decoder(mode.name, name):
                case NoMatchDecoder() as no_match:
                    return no_match
                case MatchFoundDecoder(entry=entry):
                    match.add_submatch(name, EncodeMatch(entry))
                case sub:
                    # A submode match that is not represented in the encoding
                    # will either always match or never match, so if the
                    # simplifications of the sub-decoder were effective, only
                    # MatchFoundDecoder and NoMatchDecoder are possible.
                    assert False, sub
    entry = match.fill_placeholders()

    # Insert matchers at the last index they need.
    matchers_by_index: list[list[_EncodingMatcher]] = [[] for _ in range(len(encoding.items))]
    for name, mode in entry.match_placeholders.items():
        try:
            encoded_segments = decoding[name]
        except KeyError:
            pass
        else:
            last_idx = max(seg.enc_idx for seg in encoded_segments)
            multi_match_idx = multi_matches.get(name)
            if multi_match_idx is None:
                matcher: _EncodingMatcher = MatchPlaceholder(name, mode)
            else:
                last_idx = max(last_idx, multi_match_idx)
                matcher = cast(EncodingMultiMatch, encoding.items[multi_match_idx])
                if matcher.encoded_length is None and last_idx > multi_match_idx:
                    raise ValueError(
                        f"Variable-length matcher at index "
                        f"{multi_match_idx:d} depends on index {last_idx:d}"
                    )
            matchers_by_index[last_idx].append(matcher)
    # Insert multi-matchers without slices.
    for enc_idx in multi_matches.values():
        matcher = cast(EncodingMultiMatch, encoding.items[enc_idx])
        if matcher.start == 0:
            matchers_by_index[enc_idx].append(matcher)

    # Insert fixed pattern matchers as early as possible.
    for fixed_encoding in sorted(fixed_matcher, reverse=True):
        # Find the earliest index at which the given encoding index can be
        # fetched and the index that should be requested from the fetcher.
        when_idx = fetch_idx = fixed_encoding.enc_idx
        while when_idx != 0:
            match encoding.items[when_idx - 1]:
                case EncodingMultiMatch(encoded_length=enc_len):
                    if enc_len is None:
                        # Can't move past variable-length matcher.
                        break
                    else:
                        # Adjust index.
                        fetch_idx += enc_len - 1
            when_idx -= 1
        matchers_by_index[when_idx].append(
            FixedEncoding(fetch_idx, fixed_encoding.fixed_mask, fixed_encoding.fixed_value)
        )

    # Start with the leaf node and work towards the root.
    decoder: Decoder = MatchFoundDecoder(entry)

    # Add value placeholders.
    # Since these do not cause rejections, it is most efficient to do them last,
    # when we are certain that this entry matches.
    for name in entry.value_placeholders.keys():
        enc_segs = decoding.get(name)
        if enc_segs is not None:
            decoder = PlaceholderDecoder(name, enc_segs, decoder, None, None)

    # Add match placeholders, from high index to low.
    for enc_idx in reversed(range(len(matchers_by_index))):
        for matcher in matchers_by_index[enc_idx]:
            match matcher:
                case MatchPlaceholder():
                    multi_match = False
                case EncodingMultiMatch():
                    multi_match = True
                case FixedEncoding(enc_idx=idx, fixed_mask=mask, fixed_value=value):
                    # Add fixed pattern matcher.
                    decoder = FixedPatternDecoder.create(idx, mask, value, decoder)
                    continue
                case matcher:
                    bad_type(matcher)
            # Add submode matcher.
            name = matcher.name
            aux_idx = multi_matches[name] if multi_match else None
            enc_segs = decoding.get(name)
            if multi_match and aux_idx != enc_idx:
                # Some of the encoded segments are located behind the
                # multi-match; we should adjust their fetch indices accordingly.
                assert enc_segs is not None
                assert aux_idx is not None
                assert aux_idx < enc_idx, matcher
                aux_len = encoding.items[aux_idx].encoded_length
                assert aux_len is not None
                adjust = aux_len - 1
                if adjust != 0:
                    enc_segs = tuple(encSeg.adjust_unit(aux_idx, adjust) for encSeg in enc_segs)
            match factory.create_decoder(matcher.mode.name, name):
                case NoMatchDecoder() as no_match:
                    return no_match
                case sub:
                    decoder = PlaceholderDecoder(name, enc_segs, decoder, sub, aux_idx)

    return decoder


def _create_decoder(org_decoders: Iterable[Decoder]) -> Decoder:
    """
    Returns a decoder that will decode using the last matching decoder among
    the given decoders.
    """
    decoders: list[Decoder] = []
    for decoder in org_decoders:
        match decoder:
            case NoMatchDecoder():
                # Drop decoder that will never match.
                continue
            case MatchFoundDecoder():
                # Drop all decoders overridden by a guaranteed match.
                decoders = []
        decoders.append(decoder)

    # Handle edge cases.
    if len(decoders) == 0:
        return NoMatchDecoder()
    if len(decoders) == 1:
        return decoders[0]

    # Figure out the lowest fetch index.
    enc_idx = min(
        (decoder.index for decoder in decoders if isinstance(decoder, FixedPatternDecoder)),
        default=-1,
    )

    # Find segments that are present in all masks.
    common_mask = -1
    for decoder in decoders:
        match decoder:
            case FixedPatternDecoder(index=index, mask=mask) if index == enc_idx:
                common_mask &= mask
            case _:
                common_mask = 0
                break

    if common_mask != 0:
        # Pick the upper segment: for correctness any segment will do,
        # but instruction sets are typically designed with top-level
        # categories in the upper bits.
        segment = max(mask_to_segments(common_mask))
        start = segment.start
        table_mask = segment.mask

        # Limit size of table.
        width = min(segment.width, 8)
        assert isinstance(width, int)

        # Group decoders in buckets determined by the selected segment.
        table = [list[Decoder]() for index in range(1 << width)]
        for decoder in decoders:
            assert isinstance(decoder, FixedPatternDecoder), decoder
            value = decoder.value
            index = (value & table_mask) >> start
            mask = decoder.mask
            if mask == table_mask:
                # Table takes care of full fixed pattern check.
                assert value & mask == value, (value, mask)
                nxt = decoder.next
            else:
                # Table takes care of partial fixed pattern check.
                nxt = FixedPatternDecoder.create(
                    enc_idx, mask & ~table_mask, value & ~table_mask, decoder.next
                )
            table[index].append(nxt)

        # If all decoders were assigned to the same bucket, we don't need
        # a table.
        if sum(len(decs) != 0 for decs in table) == 1:
            return FixedPatternDecoder.create(
                enc_idx, table_mask, index << start, _create_decoder(table[index])
            )

        # Create lookup table.
        return TableDecoder((_create_decoder(d) for d in table), enc_idx, table_mask, start)

    # SequentialDecoder picks the first match, so reverse the order.
    decoders.reverse()
    return SequentialDecoder(decoders)


def _qualify_names(
    entry: ModeEntry, branch_name: str | None
) -> tuple[ModeEntry, Mapping[str, Sequence[EncodedSegment]]]:
    """
    Returns a pair containing a `ModeEntry` and decode mapping, where each
    name starts with the given branch name.
    If `branch_name` is `None`, no renaming is performed.
    """
    placeholder_names = list(entry.match_placeholders)
    placeholder_names += entry.value_placeholders
    if branch_name is None or len(placeholder_names) == 0:
        # Do not rename.
        return entry, entry.encoding.decoding
    elif len(placeholder_names) == 1:
        # Replace current name with branch name.
        name_map = {placeholder_names[0]: branch_name}
    else:
        # Prefix current names with branch name.
        name_map = {name: f"{branch_name}.{name}" for name in placeholder_names}

    renamed_entry = entry.rename(name_map)
    renamed_decoding = {
        name_map[name]: value for name, value in entry.encoding.decoding.items()
    }
    return renamed_entry, renamed_decoding


class DecoderFactory:
    def __init__(
        self, mode_entries: Mapping[str | None, Iterable[ModeEntry]], flags: Iterable[str]
    ):
        self._mode_entries = mode_entries
        self._flags = frozenset(flags)
        self._cache: dict[tuple[str | None, str | None], Decoder] = {}

    def create_decoder(self, mode_name: str | None, branch_name: str | None) -> Decoder:
        cache = self._cache
        key = (mode_name, branch_name)
        decoder = cache.get(key)
        if decoder is None:
            entries = self._mode_entries[mode_name]
            flags_are_set = self._flags.issuperset
            decoder = _create_decoder(
                _create_entry_decoder(
                    *_qualify_names(entry, branch_name),
                    entry.encoding.fixed_matcher,
                    factory=self,
                )
                for entry in entries
                if flags_are_set(entry.encoding.flags_required)
            )
            cache[key] = decoder
        return decoder


@dataclass(frozen=True)
class Prefix:
    encoding: Encoding
    semantics: FunctionBody


class _PrefixDecoder:
    """A node in a prefix decoder tree."""

    __slots__ = ("children", "prefix")

    def __init__(self) -> None:
        self.children: dict[int, _PrefixDecoder] = {}
        self.prefix: Prefix | None = None

    def add_prefix(self, values: Sequence[int], prefix: Prefix) -> None:
        if values:
            value = values[0]
            try:
                child = self.children[value]
            except KeyError:
                child = self.children[value] = _PrefixDecoder()
            child.add_prefix(values[1:], prefix)
        else:
            self.prefix = prefix

    def try_decode(self, fetcher: Fetcher) -> Prefix | None:
        """
        Attempts to decode an instruction prefix from the encoded data
        provided by the given fetcher.
        Returns the decoded prefix, or None if no prefix was found.
        """
        idx = 0
        node = self
        while True:
            prefix = node.prefix
            if prefix is not None:
                return prefix
            encoded = fetcher[idx]
            if encoded is None:
                break
            idx += 1
            child = node.children.get(encoded)
            if child is None:
                break
            node = child
        return None


def create_prefix_decoder(prefixes: Sequence[Prefix]) -> Callable[[Fetcher], Prefix | None]:
    if len(prefixes) == 0:
        # Many instruction sets don't use prefixes at all.
        # Return an optimized special case for those.
        return lambda fetcher: None

    root = _PrefixDecoder()
    for prefix in prefixes:
        # Note that since we have no placeholders in prefixes, the
        # errors that decomposeEncoding() could report cannot happen.
        encoding = prefix.encoding
        fixed_matcher, decode_map = _decompose_encoding(encoding.items)
        assert len(decode_map) == 0, decode_map
        values: list[int] = []
        for idx, fixed_encoding in enumerate(sorted(fixed_matcher)):
            enc_idx = fixed_encoding.enc_idx
            assert idx == enc_idx, (idx, enc_idx)
            assert encoding.encoding_width is not None
            assert fixed_encoding.fixed_mask == mask_for_width(encoding.encoding_width)
            values.append(fixed_encoding.fixed_value)
        assert len(values) == encoding.encoded_length
        root.add_prefix(values, prefix)
    return root.try_decode


def calc_mode_entry_decoding(
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
            fixed_matcher, decode_map = _decompose_encoding(encoding_items)
        except BadInput as ex:
            collector.add(ex)

    imm_widths = {name: get_encoding_width(name, ref) for name, ref in placeholders.items()}

    multi_matches = {
        enc_item.name for enc_item in encoding_items if isinstance(enc_item, EncodingMultiMatch)
    }

    with collector.check():
        # Check placeholders that should be encoded but aren't.
        # TODO: This is something we should check at the mode entry level,
        #       instead of at the encoding level, such that the encoding
        #       does not depend on the context.
        for name, ref in placeholders.items():
            if imm_widths[name] is None:
                # Placeholder should not be encoded.
                continue
            if name in multi_matches:
                # Mode is matched using "X@" syntax.
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
            _combine_placeholder_encodings(decode_map, imm_widths, collector, encoding_location)
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
    imm_widths: Mapping[str, Width | None],
    collector: ErrorCollector,
    location: InputLocation | None,
) -> Iterator[tuple[str, Sequence[EncodedSegment]]]:
    """
    Yield pairs of placeholder name and the locations where the placeholder
    resides in the encoded items.
    Each such location is a triple of index in the encoded items, bit offset
    within that item and width in bits.
    """

    for name, slices in decode_map.items():
        imm_width = imm_widths[name]
        # Note: Encoding width is only None for empty encoding sequences,
        #       in which case the decode map will be empty as well.
        assert imm_width is not None, name
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
