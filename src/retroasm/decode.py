from __future__ import annotations

from abc import ABC, abstractmethod
from collections.abc import Callable, Iterable, Mapping, Sequence, Set
from dataclasses import dataclass
from typing import cast, override

from .codeblock import FunctionBody
from .encoding import (
    EncodedSegment,
    Encoding,
    EncodingExpr,
    EncodingMultiMatch,
    FixedEncoding,
    decompose_encoding,
)
from .expression import Expression
from .fetch import AfterModeFetcher, Fetcher, ModeFetcher
from .mode import MatchPlaceholder, ModeMatch, ModeRow, ModeTable
from .reference import FixedValueReference, int_reference
from .symbol import ImmediateValue
from .types import IntType, mask_for_width, mask_to_segments
from .utils import SingletonFromABC, bad_type, const_property


class EncodeMatch:
    """A match on the encoding field of a mode row."""

    @property
    def mode_row(self) -> ModeRow:
        return self._row

    def __init__(self, row: ModeRow):
        self._row = row
        self._subs: dict[str, EncodeMatch] = {}
        self._values: dict[str, FixedValueReference] = {}

    @override
    def __repr__(self) -> str:
        return f"EncodeMatch({self._row!r}, {self._subs!r}, {self._values!r})"

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

        for name, ref in self._row.value_placeholders.items():
            values[name] = ref.substitute(resolve_immediate).bits

        return ModeMatch(self._row, values, subs)

    def fill_placeholders(self) -> ModeRow:
        """
        Return a new mode row, in which those placeholders that are present
        in this match are replaced by the mode/value they are mapped to.
        It is not necessary for the match to provide modes/values for every
        placeholder: whatever is not matched is left untouched.
        """

        row = self._row
        subs = self._subs
        values = self._values
        if not subs and not values:
            # Skip no-op substitution for efficiency's sake.
            return row

        encoding = row.encoding.fill_placeholders(self)
        mnemonic = row.mnemonic.fill_placeholders(self)
        # TODO: Fill in placeholders in semantics too.
        semantics = row.semantics
        unfilled_matches = {
            name: mode for name, mode in row._match_placeholders.items() if name not in subs
        }
        unfilled_values = {
            name: ref for name, ref in row.value_placeholders.items() if name not in values
        }

        return ModeRow(encoding, mnemonic, semantics, unfilled_matches, unfilled_values)

    @const_property
    def flags_required(self) -> Set[str]:
        flags_required = self._row.encoding.flags_required
        for match in self._subs.values():
            flags_required |= match.mode_row.encoding.flags_required
        return flags_required

    @const_property
    def encoded_length(self) -> int:
        enc_def = self._row.encoding
        length = enc_def.encoded_length
        if length is not None:
            # Mode row has fixed encoded length.
            return length

        # Mode row  has variable encoded length.
        subs = self._subs
        length = 0
        for enc_item in enc_def.items:
            match enc_item:
                case EncodingExpr():
                    length += 1
                case EncodingMultiMatch(name=name, start=start):
                    length += subs[name].encoded_length - start
                case obj:
                    bad_type(obj)
        return length


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
    def mode_row(self) -> ModeRow:
        return self._row

    def __init__(self, row: ModeRow):
        self._row = row

    @override
    def dump(self, indent: str = "", submodes: bool = True) -> None:
        print(indent + " ".join(str(m) for m in self._row.mnemonic))

    @override
    def try_decode(self, fetcher: Fetcher) -> EncodeMatch:
        return EncodeMatch(self._row)


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


def _fill_unencoded_placeholders(
    row: ModeRow, decoding: Mapping[str, Sequence[EncodedSegment]], factory: DecoderFactory
) -> ModeRow | None:
    """
    Return a version of the given mode row in which all submode match placeholders that do not
    occur in the encoding are filled in, or `None` if an unencoded submode cannot match.

    A submode match can be possible without encoding when the submode only contains a single
    row or when decode flags exclusively decide the match.
    """

    encoding = row.encoding

    encoded_placeholders = set(decoding)
    for enc_item in encoding.items:
        if isinstance(enc_item, EncodingMultiMatch):
            encoded_placeholders.add(enc_item.name)

    match = EncodeMatch(row)
    for name, mode in row.match_placeholders.items():
        if name not in encoded_placeholders:
            match factory.create_decoder(mode, name):
                case NoMatchDecoder():
                    return None
                case MatchFoundDecoder(mode_row=row):
                    match.add_submatch(name, EncodeMatch(row))
                case sub:
                    # A submode match that is not represented in the encoding will either
                    # always match or never match, so if the simplifications of the sub-decoder
                    # were effective, only MatchFoundDecoder and NoMatchDecoder are possible.
                    assert False, sub

    return match.fill_placeholders()


def _create_row_decoder(
    row: ModeRow,
    decoding: Mapping[str, Sequence[EncodedSegment]],
    fixed_matcher: Sequence[FixedEncoding],
    factory: DecoderFactory,
) -> Decoder:
    """Return a Decoder instance that decodes the given mode row."""

    new_row = _fill_unencoded_placeholders(row, decoding, factory)
    if new_row is None:
        return NoMatchDecoder()
    row = new_row
    encoding = row.encoding

    # Find all indices that contain multi-matches.
    multi_matches = {
        enc_item.name: enc_idx
        for enc_idx, enc_item in enumerate(encoding.items)
        if isinstance(enc_item, EncodingMultiMatch)
    }

    # Insert matchers at the last index they need.
    matchers_by_index: list[list[_EncodingMatcher]] = [[] for _ in range(len(encoding.items))]
    for name, mode in row.match_placeholders.items():
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
    decoder: Decoder = MatchFoundDecoder(row)

    # Add value placeholders.
    # Since these do not cause rejections, it is most efficient to do them last,
    # when we are certain that this row matches.
    for name in row.value_placeholders.keys():
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
            match factory.create_decoder(row.match_placeholders[name], name):
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
    row: ModeRow, branch_name: str | None
) -> tuple[ModeRow, Mapping[str, Sequence[EncodedSegment]]]:
    """
    Return a pair containing a `ModeRow` and decode mapping, where each name starts with
    the given branch name.
    If `branch_name` is `None`, no renaming is performed.
    """
    placeholder_names = list(row.match_placeholders)
    placeholder_names += row.value_placeholders
    if branch_name is None or len(placeholder_names) == 0:
        # Do not rename.
        return row, row.encoding.decoding
    elif len(placeholder_names) == 1:
        # Replace current name with branch name.
        name_map = {placeholder_names[0]: branch_name}
    else:
        # Prefix current names with branch name.
        name_map = {name: f"{branch_name}.{name}" for name in placeholder_names}

    renamed_row = row.rename(name_map)
    renamed_decoding = {name_map[name]: value for name, value in row.encoding.decoding.items()}
    return renamed_row, renamed_decoding


class DecoderFactory:
    def __init__(self, flags: Iterable[str]):
        self._flags = frozenset(flags)
        self._cache: dict[tuple[ModeTable, str | None], Decoder] = {}

    def create_decoder(self, mode: ModeTable, branch_name: str | None) -> Decoder:
        cache = self._cache
        key = (mode, branch_name)
        decoder = cache.get(key)
        if decoder is None:
            flags_are_set = self._flags.issuperset
            decoder = _create_decoder(
                _create_row_decoder(
                    *_qualify_names(row, branch_name),
                    row.encoding.fixed_matcher,
                    factory=self,
                )
                for row in mode.rows
                if flags_are_set(row.encoding.flags_required)
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
        fixed_matcher, decode_map = decompose_encoding(encoding.items)
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
