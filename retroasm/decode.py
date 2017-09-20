from .fetch import AfterModeFetcher, ModeFetcher
from .mode import (
    EncodingExpr, EncodingMultiMatch, MatchPlaceholder, ValuePlaceholder
    )
from .types import maskForWidth, maskToSegments, segmentsToMask
from .utils import Singleton, const_property

from collections import defaultdict, namedtuple
from functools import reduce

class EncodeMatch:
    '''A match on the encoding field of a mode entry.
    '''

    entry = property(lambda self: self._entry)

    def __init__(self, entry):
        self._entry = entry
        self._mapping = {}

    def __repr__(self):
        return 'EncodeMatch(%r, %r)' % (self._entry, self._mapping)

    def __getitem__(self, key):
        return self._mapping[key]

    def __setitem__(self, key, value):
        assert key not in self._mapping, key
        self._mapping[key] = value

    @const_property
    def encodedLength(self):
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
                length += mapping[encItem.name].encodedLength - encItem.start
            else:
                assert False, encItem
        return length

def _formatSlice(start, end):
    if end == start + 1:
        return '[%d]' % start
    elif start == 0:
        return '[:%d]' % end
    else:
        return '[%d:%d]' % (start, end)

def _formatMask(name, mask, value=None):
    segments = list(maskToSegments(mask))
    if len(segments) == 1:
        (start, end), = segments
        segStr = name + _formatSlice(start, end)
        if value is None:
            return segStr
        else:
            return '%s=$%x' % (segStr, (value & mask) >> start)
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

    def dump(self, indent='', submodes=True):
        raise NotImplementedError

    def tryDecode(self, fetcher):
        '''Attempts to decode an instruction from the given fetcher.
        Returns an EncodeMatch, or None if no match could be made.
        '''
        raise NotImplementedError

class SequentialDecoder(Decoder):
    '''Decoder that tries other decoders in order, until the first match.
    '''

    def __init__(self, decoders):
        self._decoders = tuple(decoders)

    def dump(self, indent='', submodes=True):
        for decoder in self._decoders:
            decoder.dump(indent + '+ ', submodes)
            indent = ' ' * len(indent)

    def tryDecode(self, fetcher):
        for decoder in self._decoders:
            match = decoder.tryDecode(fetcher)
            if match is not None:
                return match
        return None

class TableDecoder(Decoder):
    '''Decoder that performs a table lookup to find the next decoder to try.
    '''

    def __init__(self, table, index, mask, offset):
        self._table = tuple(table)
        self._index = index
        self._mask = mask
        self._offset = offset
        assert (mask >> offset) == len(self._table) - 1

    def dump(self, indent='', submodes=True):
        name = 'enc%d' % self._index
        print(indent + _formatMask(name, self._mask & (-1 << self._offset)))
        for idx, decoder in enumerate(self._table):
            decoder.dump(' ' * len(indent) + '$%02x: ' % idx, submodes)

    def tryDecode(self, fetcher):
        encoded = fetcher[self._index]
        if encoded is None:
            return None
        decoder = self._table[(encoded & self._mask) >> self._offset]
        return decoder.tryDecode(fetcher)

class FixedPatternDecoder(Decoder):
    '''Decoder that matches encoded bit strings by looking for a fixed pattern
    using a mask and value.
    '''

    index = property(lambda self: self._index)
    mask = property(lambda self: self._mask)
    value = property(lambda self: self._value)
    next = property(lambda self: self._next)

    @classmethod
    def create(cls, index, mask, value, nxt):
        if isinstance(nxt, FixedPatternDecoder) and nxt.index == index:
            # Combine two masks checks into one decoder.
            assert mask & nxt.mask == 0
            mask |= nxt.mask
            value |= nxt.value
            return cls(index, mask, value, nxt.next)
        else:
            return cls(index, mask, value, nxt)

    def __init__(self, index, mask, value, nxt):
        self._index = index
        self._mask = mask
        self._value = value
        self._next = nxt

    def dump(self, indent='', submodes=True):
        maskStr = _formatMask('enc%d' % self._index, self._mask, self._value)
        self._next.dump(indent + maskStr + ' -> ', submodes)

    def tryDecode(self, fetcher):
        encoded = fetcher[self._index]
        if encoded is None or encoded & self._mask != self._value:
            return None
        else:
            return self._next.tryDecode(fetcher)

class PlaceholderDecoder(Decoder):
    '''Decodes the value for one placeholder.
    '''

    def __init__(self, name, slices, nxt, sub, auxIdx):
        self._name = name
        self._slices = slices
        self._next = nxt
        self._sub = sub
        self._auxIdx = auxIdx

    def dump(self, indent='', submodes=True):
        slices = self._slices
        if slices is None:
            sliceStr = ''
        else:
            sliceStr = ';'.join(
                'enc%d%s' % (encIdx, _formatSlice(refIdx, refIdx + width))
                for encIdx, refIdx, width in reversed(slices)
                )

        sub = self._sub
        if sub is None:
            valStr = sliceStr
        else:
            items = [sliceStr] if sliceStr else []
            auxIdx = self._auxIdx
            if auxIdx is not None:
                items.append('enc%d+' % auxIdx)
            valStr = 'sub(%s)' % ', '.join(items)

        name = self._name
        self._next.dump(indent + '%s=%s, ' % (name, valStr), submodes)
        if submodes and sub is not None:
            sub.dump((len(indent) + len(name))* ' ' + '`-> ')

    def tryDecode(self, fetcher):
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
            decoded = value
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
            match[self._name] = decoded
        return match

class MatchFoundDecoder(Decoder):
    '''Decoder that is placed at the end of a decode tree.
    It always finds a match.
    '''

    def __init__(self, entry):
        self._entry = entry

    def dump(self, indent='', submodes=True):
        print(indent + ' '.join(str(m) for m in self._entry.mnemonic))

    def tryDecode(self, fetcher):
        return EncodeMatch(self._entry)

class NoMatchDecoder(Decoder, metaclass=Singleton):
    '''Decoder that is placed at the end of a decode tree.
    It never finds a match.
    '''

    def dump(self, indent='', submodes=True):
        print(indent + '(none)')

    def tryDecode(self, fetcher):
        return None

def _createEntryDecoder(entry, decoding, factory):
    '''Returns a Decoder instance that decodes this entry.
    '''
    encoding = entry.encoding
    placeholders = entry.placeholders

    def earlyFetch(encIdx):
        '''Returns a pair containing the earliest index at which the given
        encoding index can be fetched and the index that should be
        requested from the fetcher.
        '''
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
        return whenIdx, fetchIdx

    # Find all indices that contain multi-matches.
    multiMatches = {
        encItem.name: encIdx
        for encIdx, encItem in enumerate(encoding)
        if isinstance(encItem, EncodingMultiMatch)
        }

    # Insert matchers at the last index they need.
    # Gather value placeholders them.
    matchersByIndex = [[] for _ in range(len(encoding))]
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
                    matcher = encoding[multiMatchIdx]
                    if matcher.encodedLength is None and \
                            lastIdx > multiMatchIdx:
                        raise ValueError(
                            'Variable-length matcher at index %d depends '
                            'on index %d'
                            % (multiMatchIdx, lastIdx)
                            )
                matchersByIndex[lastIdx].append(matcher)
            else:
                assert False, placeholder
    # Insert multi-matchers without slices.
    for encIdx in multiMatches.values():
        matcher = encoding[encIdx]
        if matcher.start == 0:
            matchersByIndex[encIdx].append(matcher)

    # Sort matchers and value placeholders.
    # The sorting is just to make dumps more readable and consistent between
    # runs, it doesn't impact correctness.
    def slicesKey(placeholder):
        return tuple(
            (encIdx, -refIdx)
            for encIdx, refIdx, width in decoding[placeholder.name]
            )
    def matcherKey(matcher):
        return (
            slicesKey(matcher)
            if isinstance(matcher, MatchPlaceholder) else
            matcher
            )
    for matchers in matchersByIndex:
        matchers.sort(key=matcherKey, reverse=True)
    valuePlaceholders.sort(key=slicesKey, reverse=True)

    # Insert fixed pattern matchers as early as possible.
    for encIdx, fixedMask, fixedValue in sorted(
            decoding[None], reverse=True
            ):
        whenIdx, fetchIdx = earlyFetch(encIdx)
        matchersByIndex[whenIdx].append((fetchIdx, fixedMask, fixedValue))

    # Start with the leaf node and work towards the root.
    decoder = MatchFoundDecoder(entry)

    # Add value placeholders.
    # Since these do not cause rejections, it is most efficient to do them
    # last, when we are certain that this entry matches.
    for placeholder in valuePlaceholders:
        name = placeholder.name
        slices = decoding[name]
        decoder = PlaceholderDecoder(name, slices, decoder, None, None)

    # Add match placeholders that are not represented in the encoding.
    for name, placeholder in placeholders.items():
        if isinstance(placeholder, MatchPlaceholder):
            if name not in decoding and name not in multiMatches:
                sub = factory.createDecoder(placeholder.mode.name)
                if isinstance(sub, NoMatchDecoder):
                    return sub
                decoder = PlaceholderDecoder(name, None, decoder, sub, None)

    # Add match placeholders, from high index to low.
    for encIdx, matchers in reversed(list(enumerate(matchersByIndex))):
        for matcher in matchers:
            if isinstance(matcher, MatchPlaceholder):
                multiMatch = False
            elif isinstance(matcher, EncodingMultiMatch):
                multiMatch = True
            else:
                # Add fixed pattern matcher.
                encIdx, fixedMask, fixedValue = matcher
                decoder = FixedPatternDecoder.create(
                    encIdx, fixedMask, fixedValue, decoder
                    )
                continue
            # Add submode matcher.
            name = matcher.name
            auxIdx = multiMatches[name] if multiMatch else None
            slices = decoding.get(name)
            if multiMatch and auxIdx != encIdx:
                # Some of the slices are located behind the multi-match;
                # we should adjust their fetch indices accordingly.
                assert auxIdx < encIdx, matcher
                adjust = encoding[auxIdx].encodedLength - 1
                if adjust != 0:
                    slices = tuple(
                        (idx if idx < auxIdx else idx + adjust,
                            refIdx, width)
                        for idx, refIdx, width in slices
                        )
            sub = factory.createDecoder(matcher.mode.name)
            if isinstance(sub, NoMatchDecoder):
                return sub
            decoder = PlaceholderDecoder(name, slices, decoder, sub, auxIdx)

    return decoder

def _createDecoder(decoders):
    '''Returns a decoder that will decode using the last matching decoder among
    the given decoders.
    '''
    decoders = [
        decoder
        for decoder in decoders
        if not isinstance(decoder, NoMatchDecoder)
        ]

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
    maskFreqs = defaultdict(int)
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
        table = [[] for index in range(1 << (end - start))]
        for decoder in decoders:
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

ParsedModeEntry = namedtuple('ParsedModeEntry', (
    'entry', 'decoding', 'flagsRequired'
    ))

class DecoderFactory:

    def __init__(self, modeEntries):
        self._modeEntries = modeEntries
        self._cache = {}

    def createDecoder(self, modeName):
        cache = self._cache
        decoder = cache.get(modeName)
        if decoder is None:
            parsedEntries = self._modeEntries[modeName]
            decoder = _createDecoder(
                _createEntryDecoder(
                    parsedEntry.entry, parsedEntry.decoding, self
                    )
                for parsedEntry in parsedEntries
                # TODO: Add real prefix support.
                if not parsedEntry.flagsRequired
                )
            cache[modeName] = decoder
        return decoder
