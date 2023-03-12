from __future__ import annotations

from collections import defaultdict
from collections.abc import Callable, Iterable, Mapping, Sequence
from dataclasses import dataclass
from typing import DefaultDict, TypeAlias, cast

from .codeblock import CodeBlock
from .expression import IntLiteral
from .fetch import AfterModeFetcher, Fetcher, ModeFetcher
from .linereader import BadInput
from .mode import (
    EncodeMatch,
    Encoding,
    EncodingExpr,
    EncodingMultiMatch,
    MatchPlaceholder,
    ModeEntry,
)
from .reference import FixedValue, FixedValueReference, SingleStorage, int_reference
from .storage import ArgStorage
from .types import IntType, Segment, mask_for_width, mask_to_segments
from .utils import Singleton, bad_type


@dataclass(frozen=True, order=True)
class EncodedSegment:
    """Part of an instruction encoding."""

    enc_idx: int
    """Index of encoding unit that contains the segment."""

    segment: Segment
    """Bit range."""

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


def decompose_encoding(
    encoding: Encoding,
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
    decode_map: DefaultDict[str, list[tuple[int, EncodedSegment]]] = defaultdict(list)
    for enc_idx, enc_elem in enumerate(encoding):
        if not isinstance(enc_elem, EncodingExpr):
            continue
        fixed_mask = 0
        fixed_value = 0
        try:
            start = 0
            for base, base_seg in enc_elem.bits.decompose():
                segment = Segment(start, base_seg.width)
                match base:
                    case SingleStorage(storage=ArgStorage(name=name)):
                        decode_map[name].append(
                            (base_seg.start, EncodedSegment(enc_idx, segment))
                        )
                    case SingleStorage(storage=storage):
                        raise ValueError(
                            f"unsupported storage type in encoding: "
                            f"{storage.__class__.__name__}"
                        )
                    case FixedValue(expr=IntLiteral(value=value)):
                        fixed_mask |= segment.mask
                        fixed_value |= base_seg.cut(value) << start
                    case FixedValue():
                        raise ValueError("unsupported operation in encoding")
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
            name
            + ":"
            + "_".join("".join(digits[i : i + 4]) for i in range(0, len(digits), 4))
        )


class Decoder:
    def dump(self, indent: str = "", submodes: bool = True) -> None:
        raise NotImplementedError

    def try_decode(self, fetcher: Fetcher) -> EncodeMatch | None:
        """
        Attempts to decode an instruction from the given fetcher.
        Returns an encode match, or None if no match could be made.
        """
        raise NotImplementedError


class SequentialDecoder(Decoder):
    """Decoder that tries other decoders in order, until the first match."""

    def __init__(self, decoders: Iterable[Decoder]):
        self._decoders = tuple(decoders)

    def dump(self, indent: str = "", submodes: bool = True) -> None:
        for decoder in self._decoders:
            decoder.dump(indent + "+ ", submodes)
            indent = " " * len(indent)

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

    def dump(self, indent: str = "", submodes: bool = True) -> None:
        name = f"enc{self._index:d}"
        print(indent + _format_mask(name, self._mask & (-1 << self._offset)))
        for idx, decoder in enumerate(self._table):
            decoder.dump(" " * len(indent) + f"${idx:02x}: ", submodes)

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
    def create(  # type: ignore[return]
        cls, index: int, mask: int, value: int, nxt: Decoder
    ) -> FixedPatternDecoder:
        match nxt:
            case FixedPatternDecoder(
                index=nindex, value=nvalue, mask=nmask, next=nnxt
            ) if nindex == index:
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

    def dump(self, indent: str = "", submodes: bool = True) -> None:
        mask_str = _format_mask(f"enc{self._index:d}", self._mask, self._value)
        self._next.dump(indent + mask_str + " -> ", submodes)

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

    def dump(self, indent: str = "", submodes: bool = True) -> None:
        print(indent + " ".join(str(m) for m in self._entry.mnemonic))

    def try_decode(self, fetcher: Fetcher) -> EncodeMatch:
        return EncodeMatch(self._entry)


class NoMatchDecoder(Decoder, metaclass=Singleton):
    """
    Decoder that is placed at the end of a decode tree.
    It never finds a match.
    """

    def dump(self, indent: str = "", submodes: bool = True) -> None:
        print(indent + "(none)")

    def try_decode(self, fetcher: Fetcher) -> None:
        return None


_EncodingMatcher: TypeAlias = MatchPlaceholder | EncodingMultiMatch | FixedEncoding


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
        for enc_idx, enc_item in enumerate(encoding)
        if isinstance(enc_item, EncodingMultiMatch)
    }

    # Match placeholders that are not represented in the encoding.
    # Typically these are matched on decode flags.
    match = EncodeMatch(entry)
    for match_placeholder in entry.match_placeholders:
        name = match_placeholder.name
        if name not in decoding and name not in multi_matches:
            match factory.create_decoder(match_placeholder.mode.name, name):
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
    matchers_by_index: list[list[_EncodingMatcher]] = [[] for _ in range(len(encoding))]
    for match_placeholder in entry.match_placeholders:
        name = match_placeholder.name
        try:
            encoded_segments = decoding[name]
        except KeyError:
            pass
        else:
            last_idx = max(seg.enc_idx for seg in encoded_segments)
            multi_match_idx = multi_matches.get(name)
            if multi_match_idx is None:
                matcher: _EncodingMatcher = match_placeholder
            else:
                last_idx = max(last_idx, multi_match_idx)
                matcher = cast(EncodingMultiMatch, encoding[multi_match_idx])
                if matcher.encoded_length is None and last_idx > multi_match_idx:
                    raise ValueError(
                        f"Variable-length matcher at index "
                        f"{multi_match_idx:d} depends on index {last_idx:d}"
                    )
            matchers_by_index[last_idx].append(matcher)
    # Insert multi-matchers without slices.
    for enc_idx in multi_matches.values():
        matcher = cast(EncodingMultiMatch, encoding[enc_idx])
        if matcher.start == 0:
            matchers_by_index[enc_idx].append(matcher)

    # Insert fixed pattern matchers as early as possible.
    for fixed_encoding in sorted(fixed_matcher, reverse=True):
        # Find the earliest index at which the given encoding index can be
        # fetched and the index that should be requested from the fetcher.
        when_idx = fetch_idx = fixed_encoding.enc_idx
        while when_idx != 0:
            match encoding[when_idx - 1]:
                case EncodingMultiMatch(encoded_length=enc_len):
                    if enc_len is None:
                        # Can't move past variable-length matcher.
                        break
                    else:
                        # Adjust index.
                        fetch_idx += enc_len - 1
            when_idx -= 1
        matchers_by_index[when_idx].append(
            FixedEncoding(
                fetch_idx, fixed_encoding.fixed_mask, fixed_encoding.fixed_value
            )
        )

    # Start with the leaf node and work towards the root.
    decoder: Decoder = MatchFoundDecoder(entry)

    # Add value placeholders.
    # Since these do not cause rejections, it is most efficient to do them last,
    # when we are certain that this entry matches.
    for placeholder in entry.value_placeholders:
        name = placeholder.name
        decoder = PlaceholderDecoder(name, decoding[name], decoder, None, None)

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
                aux_len = encoding[aux_idx].encoded_length
                assert aux_len is not None
                adjust = aux_len - 1
                if adjust != 0:
                    enc_segs = tuple(
                        encSeg.adjust_unit(aux_idx, adjust) for encSeg in enc_segs
                    )
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
        (
            decoder.index
            for decoder in decoders
            if isinstance(decoder, FixedPatternDecoder)
        ),
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
        return TableDecoder(
            (_create_decoder(d) for d in table), enc_idx, table_mask, start
        )

    # SequentialDecoder picks the first match, so reverse the order.
    decoders.reverse()
    return SequentialDecoder(decoders)


@dataclass(frozen=True)
class ParsedModeEntry:
    entry: ModeEntry
    fixed_matcher: Sequence[FixedEncoding]
    decoding: Mapping[str, Sequence[EncodedSegment]]


def _qualify_names(
    parsed_entry: ParsedModeEntry, branch_name: str | None
) -> tuple[ModeEntry, Mapping[str, Sequence[EncodedSegment]]]:
    """
    Returns a pair containing a ModeEntry and decode mapping, where each
    name starts with the given branch name.
    If branchName is None, no renaming is performed.
    """
    entry = parsed_entry.entry
    placeholders = entry.placeholders
    if branch_name is None or len(placeholders) == 0:
        # Do not rename.
        return parsed_entry.entry, parsed_entry.decoding
    elif len(placeholders) == 1:
        # Replace current name with branch name.
        name_map = {placeholders[0].name: branch_name}
    else:
        # Prefix current names with branch name.
        name_map = {(name := p.name): f"{branch_name}.{name}" for p in placeholders}

    renamed_entry = entry.rename(name_map)
    renamed_decoding = {
        name_map[name]: value for name, value in parsed_entry.decoding.items()
    }
    return renamed_entry, renamed_decoding


class DecoderFactory:
    def __init__(
        self,
        mode_entries: Mapping[str | None, Iterable[ParsedModeEntry]],
        flags: Iterable[str],
    ):
        self._mode_entries = mode_entries
        self._flags = frozenset(flags)
        self._cache: dict[tuple[str | None, str | None], Decoder] = {}

    def create_decoder(self, mode_name: str | None, branch_name: str | None) -> Decoder:
        cache = self._cache
        key = (mode_name, branch_name)
        decoder = cache.get(key)
        if decoder is None:
            parsed_entries = self._mode_entries[mode_name]
            flags_are_set = self._flags.issuperset
            decoder = _create_decoder(
                _create_entry_decoder(
                    *_qualify_names(parsedEntry, branch_name),
                    parsedEntry.fixed_matcher,
                    factory=self,
                )
                for parsedEntry in parsed_entries
                if flags_are_set(parsedEntry.entry.flags_required)
            )
            cache[key] = decoder
        return decoder


@dataclass(frozen=True)
class Prefix:
    encoding: Encoding
    semantics: CodeBlock


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


def create_prefix_decoder(
    prefixes: Sequence[Prefix],
) -> Callable[[Fetcher], Prefix | None]:
    if len(prefixes) == 0:
        # Many instruction sets don't use prefixes at all.
        # Return an optimized special case for those.
        return lambda fetcher: None

    root = _PrefixDecoder()
    for prefix in prefixes:
        # Note that since we have no placeholders in prefixes, the
        # errors that decomposeEncoding() could report cannot happen.
        encoding = prefix.encoding
        fixed_matcher, decode_map = decompose_encoding(encoding)
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
