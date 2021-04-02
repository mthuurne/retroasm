from __future__ import annotations

from collections import defaultdict
from dataclasses import dataclass
from functools import reduce
from typing import (
    AbstractSet, Any, DefaultDict, Dict, Iterable, Iterator, List, Mapping,
    NamedTuple, Optional, Sequence, Tuple, Union, cast
)

from .codeblock import CodeBlock
from .expression import IntLiteral
from .fetch import AfterModeFetcher, Fetcher, ModeFetcher
from .linereader import BadInput
from .mode import (
    EncodeMatch, Encoding, EncodingExpr, EncodingMultiMatch, MatchPlaceholder,
    ModeEntry, Placeholder, ValuePlaceholder
)
from .reference import (
    BitString, ConcatenatedBits, FixedValue, SingleStorage, SlicedBits
)
from .storage import ArgStorage
from .types import Width, maskForWidth, maskToSegments, segmentsToMask
from .utils import Singleton


def _decomposeBitString(bits: BitString
                        ) -> Iterator[Tuple[BitString, int, int, Width]]:
    '''Decomposes the given bit string into its base strings (FixedValue and
    SingleStorage).
    Yields a series of tuples, containing base string, index in the base
    string, index in the given string and width of each decomposed slice.
    Raises ValueError if a bit string cannot be decomposed because it contains
    a slice with an unknown offset.
    '''
    if isinstance(bits, (FixedValue, SingleStorage)):
        yield bits, 0, 0, bits.width
    elif isinstance(bits, ConcatenatedBits):
        start = 0
        for sub in bits:
            for base, baseIdx, compIdx, width in _decomposeBitString(sub):
                yield base, baseIdx, start + compIdx, width
            # Note: It is possible for the width to be unlimited, but only
            #       at the end of a string, in which case we don't use
            #       the start offset anymore.
            start += cast(int, sub.width)
    elif isinstance(bits, SlicedBits):
        # Note that SlicedBits has already simplified the offset.
        offset = bits.offset
        if isinstance(offset, IntLiteral):
            start = offset.value
            end = start + bits.width
            for base, baseIdx, compIdx, width in _decomposeBitString(bits.bits):
                # Clip to slice boundaries.
                compStart = max(compIdx, start)
                compEnd = min(compIdx + width, end)
                # Output if clipped slice is not empty.
                width = compEnd - compStart
                if width > 0:
                    baseShift = compStart - compIdx
                    yield base, baseIdx + baseShift, compStart - start, width
        else:
            raise ValueError('slices in encoding must have fixed offset')
    else:
        assert False, bits

class FixedEncoding(NamedTuple):
    """Constant part of an instruction encoding."""
    encIdx: int
    fixedMask: int
    fixedValue: int

def decomposeEncoding(
        encoding: Encoding
        ) -> Tuple[
            Sequence[FixedEncoding],
            Mapping[str, Sequence[Tuple[int, int, int, Width]]]
            ]:
    '''Decomposes the given Encoding into a matcher for the fixed bit strings
    and a decode map describing where the placeholders values can be found.
    Returns a pair of the fixed matcher and the decode map.
    The fixed matcher is a sequence of tuples, where each tuple contains the
    index in the encoding followed by the mask and value of the fixed bits
    pattern at that index.
    The decode map stores a sequence of tuples for each name, where each tuple
    contains the index in the placeholder's argument storage, the index of
    the encoding item, the index within the encoding item and the width.
    Raises BadInput if the given encoding cannot be decomposed.
    '''
    fixedMatcher: List[FixedEncoding] = []
    decodeMap: DefaultDict[str, List[Tuple[int, int, int, Width]]] = \
                                                            defaultdict(list)
    for encIdx, encElem in enumerate(encoding):
        if not isinstance(encElem, EncodingExpr):
            continue
        fixedMask = 0
        fixedValue = 0
        try:
            for base, baseIdx, compIdx, width in _decomposeBitString(
                    encElem.bits
                    ):
                if isinstance(base, SingleStorage):
                    storage = base.storage
                    if isinstance(storage, ArgStorage):
                        decodeMap[storage.name].append(
                            (baseIdx, encIdx, compIdx, width)
                            )
                    else:
                        raise ValueError(
                            f'unsupported storage type in encoding: '
                            f'{storage.__class__.__name__}'
                            )
                elif isinstance(base, FixedValue):
                    expr = base.expr
                    if isinstance(expr, IntLiteral):
                        mask = maskForWidth(width) << compIdx
                        fixedMask |= mask
                        value = expr.value
                        fixedValue |= ((value >> baseIdx) << compIdx) & mask
                    else:
                        raise ValueError('unsupported operation in encoding')
                else:
                    assert False, base
        except ValueError as ex:
            # TODO: This message is particularly unclear, because we do not
            #       have the exact location nor can we print the offending
            #       expression.
            #       We could store locations in non-simplified expressions
            #       or decompose parse trees instead of references.
            raise BadInput(str(ex), encElem.location)
        else:
            if fixedMask != 0:
                fixedMatcher.append(
                    FixedEncoding(encIdx, fixedMask, fixedValue)
                    )
    return fixedMatcher, decodeMap

def _formatSlice(start: int, end: int) -> str:
    if end == start + 1:
        return f'[{start:d}]'
    elif start == 0:
        return f'[:{end:d}]'
    else:
        return f'[{start:d}:{end:d}]'

def _formatMask(name: str, mask: int, value: Optional[int] = None) -> str:
    segments = list(maskToSegments(mask))
    if len(segments) == 1:
        (start, end), = segments
        assert isinstance(end, int), mask
        segStr = name + _formatSlice(start, end)
        if value is None:
            return segStr
        else:
            return f'{segStr}=${(value & mask) >> start:x}'
    else:
        digits = []
        while mask:
            if mask & 1 == 0:
                digits.append('x')
            elif value is None:
                digits.append('?')
            else:
                digits.append(str(value & 1))
            mask >>= 1
            if value is not None:
                value >>= 1
        while not digits or len(digits) % 4 != 0:
            digits.append('x')
        digits.reverse()
        return name + ':' + '_'.join(
            ''.join(digits[i:i+4]) for i in range(0, len(digits), 4)
            )

class Decoder:

    def dump(self, indent: str = '', submodes: bool = True) -> None:
        raise NotImplementedError

    def tryDecode(self, fetcher: Fetcher) -> Optional[EncodeMatch]:
        '''Attempts to decode an instruction from the given fetcher.
        Returns an encode match, or None if no match could be made.
        '''
        raise NotImplementedError

class SequentialDecoder(Decoder):
    '''Decoder that tries other decoders in order, until the first match.
    '''

    def __init__(self, decoders: Iterable[Decoder]):
        self._decoders = tuple(decoders)

    def dump(self, indent: str = '', submodes: bool = True) -> None:
        for decoder in self._decoders:
            decoder.dump(indent + '+ ', submodes)
            indent = ' ' * len(indent)

    def tryDecode(self, fetcher: Fetcher) -> Optional[EncodeMatch]:
        for decoder in self._decoders:
            match = decoder.tryDecode(fetcher)
            if match is not None:
                return match
        return None

class TableDecoder(Decoder):
    '''Decoder that performs a table lookup to find the next decoder to try.
    '''

    def __init__(self,
                 table: Iterable[Decoder],
                 index: int,
                 mask: int,
                 offset: int
                 ):
        self._table = tuple(table)
        self._index = index
        self._mask = mask
        self._offset = offset
        assert (mask >> offset) == len(self._table) - 1

    def dump(self, indent: str = '', submodes: bool = True) -> None:
        name = f'enc{self._index:d}'
        print(indent + _formatMask(name, self._mask & (-1 << self._offset)))
        for idx, decoder in enumerate(self._table):
            decoder.dump(' ' * len(indent) + f'${idx:02x}: ', submodes)

    def tryDecode(self, fetcher: Fetcher) -> Optional[EncodeMatch]:
        encoded = fetcher[self._index]
        if encoded is None:
            return None
        decoder = self._table[(encoded & self._mask) >> self._offset]
        return decoder.tryDecode(fetcher)

class FixedPatternDecoder(Decoder):
    '''Decoder that matches encoded bit strings by looking for a fixed pattern
    using a mask and value.
    '''

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
    def create(cls,
               index: int,
               mask: int,
               value: int,
               nxt: Decoder
               ) -> FixedPatternDecoder:
        if isinstance(nxt, FixedPatternDecoder) and nxt.index == index:
            # Combine two masks checks into one decoder.
            assert mask & nxt.mask == 0
            mask |= nxt.mask
            value |= nxt.value
            return cls(index, mask, value, nxt.next)
        else:
            return cls(index, mask, value, nxt)

    def __init__(self, index: int, mask: int, value: int, nxt: Decoder):
        self._index = index
        self._mask = mask
        self._value = value
        self._next = nxt

    def dump(self, indent: str = '', submodes: bool = True) -> None:
        maskStr = _formatMask(f'enc{self._index:d}', self._mask, self._value)
        self._next.dump(indent + maskStr + ' -> ', submodes)

    def tryDecode(self, fetcher: Fetcher) -> Optional[EncodeMatch]:
        encoded = fetcher[self._index]
        if encoded is None or encoded & self._mask != self._value:
            return None
        else:
            return self._next.tryDecode(fetcher)

class PlaceholderDecoder(Decoder):
    '''Decodes the value for one placeholder.
    '''

    def __init__(self,
                 name: str,
                 slices: Optional[Sequence[FixedEncoding]],
                 nxt: Decoder,
                 sub: Optional[Decoder],
                 auxIdx: Optional[int]
                 ):
        self._name = name
        self._slices = slices
        self._next = nxt
        self._sub = sub
        self._auxIdx = auxIdx

    def dump(self, indent: str = '', submodes: bool = True) -> None:
        slices = self._slices
        if slices is None:
            sliceStr = ''
        else:
            sliceStr = ';'.join(
                f'enc{encIdx:d}{_formatSlice(refIdx, refIdx + width)}'
                for encIdx, refIdx, width in reversed(slices)
                )

        sub = self._sub
        if sub is None:
            valStr = sliceStr
        else:
            items = [sliceStr] if sliceStr else []
            auxIdx = self._auxIdx
            if auxIdx is not None:
                items.append(f'enc{auxIdx:d}+')
            valStr = 'sub(%s)' % ', '.join(items)

        name = self._name
        self._next.dump(indent + f'{name}={valStr}, ', submodes)
        if submodes and sub is not None:
            sub.dump((len(indent) + len(name))* ' ' + '`-> ')

    def tryDecode(self, fetcher: Fetcher) -> Optional[EncodeMatch]:
        slices = self._slices
        if slices is None:
            value = None
        else:
            # Compose encoded first value.
            value = 0
            offset = 0
            for encIdx, refIdx, width in slices:
                encoded = fetcher[encIdx]
                if encoded is None:
                    return None
                value |= ((encoded >> refIdx) & maskForWidth(width)) << offset
                offset += width

        # Decode placeholder.
        sub = self._sub
        if sub is None:
            decoded: Optional[Union[EncodeMatch, int]] = value
        else:
            auxIdx = self._auxIdx
            decoded = sub.tryDecode(ModeFetcher(value, fetcher, auxIdx))
            if decoded is None:
                return None
            if auxIdx is not None:
                # Note: When there are slices, the multi-matcher won't match
                #       the first unit, since they have been matched by single
                #       matcher(s) already.
                delta = decoded.encodedLength - (1 if slices is None else 2)
                if delta != 0:
                    fetcher = AfterModeFetcher(fetcher, auxIdx, delta)

        # Decode remainder.
        match = self._next.tryDecode(fetcher)
        if match is not None:
            assert decoded is not None
            match[self._name] = decoded
        return match

class MatchFoundDecoder(Decoder):
    '''Decoder that is placed at the end of a decode tree.
    It always finds a match.
    '''

    @property
    def entry(self) -> ModeEntry:
        return self._entry

    @property
    def match(self) -> EncodeMatch:
        return EncodeMatch(self._entry)

    def __init__(self, entry: ModeEntry):
        self._entry = entry

    def dump(self, indent: str = '', submodes: bool = True) -> None:
        print(indent + ' '.join(str(m) for m in self._entry.mnemonic))

    def tryDecode(self, fetcher: Fetcher) -> EncodeMatch:
        return EncodeMatch(self._entry)

class NoMatchDecoder(Decoder, metaclass=Singleton):
    '''Decoder that is placed at the end of a decode tree.
    It never finds a match.
    '''

    def dump(self, indent: str = '', submodes: bool = True) -> None:
        print(indent + '(none)')

    def tryDecode(self, fetcher: Fetcher) -> None:
        return None

EncodingMatcher = Union[MatchPlaceholder, EncodingMultiMatch, FixedEncoding]

def _createEntryDecoder(
        entry: ModeEntry,
        decoding: Mapping[Optional[str], Sequence[FixedEncoding]],
        factory: DecoderFactory
        ) -> Decoder:
    '''Returns a Decoder instance that decodes this entry.
    '''
    name: Optional[str]
    matcher: EncodingMatcher
    slices: Optional[Sequence[FixedEncoding]]

    # Find all indices that contain multi-matches.
    multiMatches = {
        encItem.name: encIdx
        for encIdx, encItem in enumerate(entry.encoding)
        if isinstance(encItem, EncodingMultiMatch)
        }

    # Match placeholders that are not represented in the encoding.
    # Typically these are matched on decode flags.
    match = EncodeMatch(entry)
    for name, placeholder in entry.placeholders.items():
        if isinstance(placeholder, MatchPlaceholder):
            if name not in decoding and name not in multiMatches:
                # A submode match that is not represented in the encoding
                # will either always match or never match, so if the
                # simplifications of the sub-decoder were effective, only
                # MatchFoundDecoder and NoMatchDecoder are possible.
                sub = factory.createDecoder(placeholder.mode.name, name)
                if isinstance(sub, NoMatchDecoder):
                    return sub
                elif isinstance(sub, MatchFoundDecoder):
                    match[name] = sub.match
                else:
                    assert False, sub
    entry = match.fillPlaceholders()

    # Insert matchers at the last index they need.
    # Gather value placeholders them.
    placeholders = entry.placeholders
    encoding = entry.encoding
    matchersByIndex: List[List[EncodingMatcher]] = [
        [] for _ in range(len(encoding))
        ]
    valuePlaceholders = []
    for name, slices in decoding.items():
        if name is not None:
            placeholder = placeholders[name]
            if isinstance(placeholder, ValuePlaceholder):
                valuePlaceholders.append(placeholder)
            elif isinstance(placeholder, MatchPlaceholder):
                lastIdx = max(encIdx for encIdx, refIdx, width in slices)
                multiMatchIdx = multiMatches.get(placeholder.name)
                if multiMatchIdx is None:
                    matcher = placeholder
                else:
                    lastIdx = max(lastIdx, multiMatchIdx)
                    matcher = cast(EncodingMultiMatch, encoding[multiMatchIdx])
                    if matcher.encodedLength is None and \
                            lastIdx > multiMatchIdx:
                        raise ValueError(
                            f'Variable-length matcher at index '
                            f'{multiMatchIdx:d} depends on index {lastIdx:d}'
                            )
                matchersByIndex[lastIdx].append(matcher)
            else:
                assert False, placeholder
    # Insert multi-matchers without slices.
    for encIdx in multiMatches.values():
        matcher = cast(EncodingMultiMatch, encoding[encIdx])
        if matcher.start == 0:
            matchersByIndex[encIdx].append(matcher)

    # Sort matchers and value placeholders.
    # The sorting is just to make dumps more readable and consistent between
    # runs, it doesn't impact correctness.
    def slicesKey(placeholder: Placeholder) -> Tuple[Tuple[int, int], ...]:
        return tuple(
            (encIdx, -refIdx)
            for encIdx, refIdx, width in decoding[placeholder.name]
            )
    def matcherKey(matcher: EncodingMatcher) -> Tuple[Tuple[int, ...], ...]:
        if isinstance(matcher, MatchPlaceholder):
            return slicesKey(matcher)
        elif isinstance(matcher, EncodingMultiMatch):
            return ((matcher.start, ), )
        elif isinstance(matcher, FixedEncoding):
            return ((matcher.encIdx, -matcher.fixedMask), )
        else:
            assert False, type(matcher)
    for matchers in matchersByIndex:
        matchers.sort(key=matcherKey, reverse=True)
    valuePlaceholders.sort(key=slicesKey, reverse=True)

    # Insert fixed pattern matchers as early as possible.
    for encIdx, fixedMask, fixedValue in sorted(decoding[None], reverse=True):
        # Find the earliest index at which the given encoding index can be
        # fetched and the index that should be requested from the fetcher.
        whenIdx = fetchIdx = encIdx
        while whenIdx != 0:
            encItem = encoding[whenIdx - 1]
            if isinstance(encItem, EncodingMultiMatch):
                encodedLength = encItem.encodedLength
                if encodedLength is None:
                    # Can't move past variable-length matcher.
                    break
                else:
                    # Adjust index.
                    fetchIdx += encodedLength - 1
            whenIdx -= 1
        matchersByIndex[whenIdx].append(
            FixedEncoding(fetchIdx, fixedMask, fixedValue)
            )

    # Start with the leaf node and work towards the root.
    decoder: Decoder = MatchFoundDecoder(entry)

    # Add value placeholders.
    # Since these do not cause rejections, it is most efficient to do them
    # last, when we are certain that this entry matches.
    for placeholder in valuePlaceholders:
        name = placeholder.name
        slices = decoding[name]
        decoder = PlaceholderDecoder(name, slices, decoder, None, None)

    # Add match placeholders, from high index to low.
    for encIdx, matchers in reversed(list(enumerate(matchersByIndex))):
        for matcher in matchers:
            if isinstance(matcher, MatchPlaceholder):
                multiMatch = False
            elif isinstance(matcher, EncodingMultiMatch):
                multiMatch = True
            elif isinstance(matcher, FixedEncoding):
                # Add fixed pattern matcher.
                decoder = FixedPatternDecoder.create(
                    matcher.encIdx, matcher.fixedMask, matcher.fixedValue,
                    decoder
                    )
                continue
            else:
                assert False, matcher
            # Add submode matcher.
            name = matcher.name
            auxIdx = multiMatches[name] if multiMatch else None
            slices = decoding.get(name)
            if multiMatch and auxIdx != encIdx:
                # Some of the slices are located behind the multi-match;
                # we should adjust their fetch indices accordingly.
                assert slices is not None
                assert auxIdx is not None
                assert auxIdx < encIdx, matcher
                auxLen = encoding[auxIdx].encodedLength
                assert auxLen is not None
                adjust = auxLen - 1
                if adjust != 0:
                    slices = tuple(
                        FixedEncoding(idx if idx < auxIdx else idx + adjust,
                                      refIdx, width)
                        for idx, refIdx, width in slices
                        )
            sub = factory.createDecoder(matcher.mode.name, name)
            if isinstance(sub, NoMatchDecoder):
                return sub
            decoder = PlaceholderDecoder(name, slices, decoder, sub, auxIdx)

    return decoder

def _createDecoder(orgDecoders: Iterable[Decoder]) -> Decoder:
    '''Returns a decoder that will decode using the last matching decoder among
    the given decoders.
    '''
    decoders: List[Decoder] = []
    for decoder in orgDecoders:
        if isinstance(decoder, NoMatchDecoder):
            # Drop decoder that will never match.
            continue
        if isinstance(decoder, MatchFoundDecoder):
            # Drop all decoders overridden by a guaranteed match.
            decoders = []
        decoders.append(decoder)

    # Handle edge cases.
    if len(decoders) == 0:
        return NoMatchDecoder()
    if len(decoders) == 1:
        return decoders[0]

    # Figure out the lowest fetch index.
    encIdx = min((
        decoder.index
        for decoder in decoders
        if isinstance(decoder, FixedPatternDecoder)
        ), default=-1)

    # Gather stats about fixed patterns of our entries.
    maskFreqs: DefaultDict[int, int] = defaultdict(int)
    for decoder in decoders:
        if isinstance(decoder, FixedPatternDecoder) and decoder.index == encIdx:
            mask = decoder.mask
        else:
            mask = 0
        maskFreqs[mask] += 1

    # Find segments that are present in all masks.
    commonMask = reduce(int.__and__, maskFreqs.keys(), -1)
    segments = []
    if commonMask > 0:
        for start, end in maskToSegments(commonMask):
            assert isinstance(end, int), commonMask
            segments.append((start, end))

    if len(segments) != 0:
        # Pick the upper segment: for correctness any segment will do,
        # but instruction sets are typically designed with top-level
        # categories in the upper bits.
        segment = segments[-1]
        tableMask = segmentsToMask((segment,))
        start, end = segment

        # Limit size of table.
        end = min(end, start + 8)

        # Group decoders in buckets determined by the selected segment.
        table: List[List[Decoder]] = [
            [] for index in range(1 << (end - start))
            ]
        for decoder in decoders:
            assert isinstance(decoder, FixedPatternDecoder), decoder
            value = decoder.value
            index = (value & tableMask) >> start
            mask = decoder.mask
            if mask == tableMask:
                # Table takes care of full fixed pattern check.
                assert value & mask == value, (value, mask)
                nxt = decoder.next
            else:
                # Table takes care of partial fixed pattern check.
                nxt = FixedPatternDecoder.create(
                    encIdx, mask & ~tableMask, value & ~tableMask, decoder.next
                    )
            table[index].append(nxt)

        # If all decoders were assigned to the same bucket, we don't need
        # a table.
        if sum(len(decs) != 0 for decs in table) == 1:
            return FixedPatternDecoder.create(
                encIdx, tableMask, index << start, _createDecoder(table[index])
                )

        # Create lookup table.
        return TableDecoder(
            (_createDecoder(d) for d in table), encIdx, tableMask, start
            )

    # SequentialDecoder picks the first match, so reverse the order.
    decoders.reverse()
    return SequentialDecoder(decoders)

@dataclass(frozen=True)
class ParsedModeEntry:
    entry: ModeEntry
    decoding: Mapping[Optional[str], Sequence[FixedEncoding]]
    flagsRequired: AbstractSet[str]

def _qualifyNames(parsedEntry: ParsedModeEntry,
                  branchName: Optional[str]
                  ) -> Tuple[
                      ModeEntry,
                      Mapping[Optional[str], Sequence[FixedEncoding]]
                      ]:
    '''Returns a pair containing a ModeEntry and decode mapping, where each
    name starts with the given branch name.
    If branchName is None, no renaming is performed.
    '''
    entry = parsedEntry.entry
    placeholders = entry.placeholders
    if branchName is None or len(placeholders) == 0:
        # Do not rename.
        return parsedEntry.entry, parsedEntry.decoding
    elif len(placeholders) == 1:
        # Replace current name with branch name.
        name, = placeholders.keys()
        nameMap = {name: branchName}
    else:
        # Prefix current names with branch name.
        qualifier = branchName + '.'
        nameMap = {name: qualifier + name for name in placeholders.keys()}

    renamedEntry = entry.rename(nameMap)
    renamedDecoding = {
        None if name is None else nameMap[name]: value
        for name, value in parsedEntry.decoding.items()
        }
    return renamedEntry, renamedDecoding

class DecoderFactory:

    def __init__(self,
                 modeEntries: Mapping[Optional[str], Iterable[ParsedModeEntry]],
                 flags: Iterable[str]
                 ):
        self._modeEntries = modeEntries
        self._flags = frozenset(flags)
        self._cache: Dict[Tuple[Optional[str], Optional[str]], Decoder] = {}

    def createDecoder(self,
                      modeName: Optional[str],
                      branchName: Optional[str]
                      ) -> Decoder:
        cache = self._cache
        key = (modeName, branchName)
        decoder = cache.get(key)
        if decoder is None:
            parsedEntries = self._modeEntries[modeName]
            flagsAreSet = self._flags.issuperset
            decoder = _createDecoder(
                _createEntryDecoder(
                    *_qualifyNames(parsedEntry, branchName), factory=self
                    )
                for parsedEntry in parsedEntries
                if flagsAreSet(parsedEntry.flagsRequired)
                )
            cache[key] = decoder
        return decoder

@dataclass(frozen=True)
class Prefix:
    encoding: Encoding
    semantics: CodeBlock

class PrefixDecoder:

    def __init__(self, prefixes: Iterable[Prefix]):
        tree: Dict[Optional[int], Any] = {}
        def addPrefix(node: Dict[Optional[int], Any],
                      values: Sequence[int],
                      prefix: Prefix
                      ) -> None:
            if values:
                child = node.setdefault(values[0], {})
                addPrefix(child, values[1:], prefix)
            else:
                node[None] = prefix
        for prefix in prefixes:
            # Note that since we have no placeholders in prefixes, the
            # errors that decomposeEncoding() could report cannot happen.
            encoding = prefix.encoding
            fixedMatcher, decodeMap = decomposeEncoding(encoding)
            assert len(decodeMap) == 0, decodeMap
            values: List[int] = []
            for idx, (encIdx, fixedMask, fixedValue) \
                    in enumerate(sorted(fixedMatcher)):
                assert idx == encIdx, (idx, encIdx)
                assert encoding.encodingWidth is not None
                assert fixedMask == maskForWidth(encoding.encodingWidth)
                values.append(fixedValue)
            assert len(values) == encoding.encodedLength
            addPrefix(tree, values, prefix)
        self._tree = tree

    def tryDecode(self, fetcher: Fetcher) -> Optional[Prefix]:
        '''Attempts to decode an instruction prefix from the encoded data
        provided by the given fetcher.
        Returns the decoded prefix, or None if no prefix was found.
        '''
        idx = 0
        node: Optional[Dict[Optional[int], Any]] = self._tree
        assert node is not None
        while True:
            prefix: Optional[Prefix] = node.get(None)
            if prefix is None:
                encoded = fetcher[idx]
                idx += 1
                node = node.get(encoded)
                if node is None:
                    return None
            else:
                return prefix
