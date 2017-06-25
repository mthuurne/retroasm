from .analysis import PlaceholderRole, determinePlaceholderRoles
from .expression import Expression, IntLiteral
from .expression_parser import DeclarationNode, ParseNode
from .expression_simplifier import simplifyExpression
from .fetch import AfterModeFetcher, ModeFetcher
from .linereader import mergeSpan
from .types import (
    IntType, maskForWidth, maskToSegments, segmentsToMask, unlimited
    )
from .utils import Singleton, checkType, const_property

from collections import defaultdict
from functools import reduce

class EncodingExpr:
    '''A single element in an encoding sequence that is specified using an
    expression.
    '''

    ref = property(lambda self: self._ref)
    value = property(lambda self: self._value)
    location = property(lambda self: self._location)

    encodingWidth = property(lambda self: self._ref.width)
    encodedLength = property(lambda self: 1)

    def __init__(self, ref, value, location):
        self._ref = ref
        self._value = value
        self._location = location

    def __repr__(self):
        return 'EncodingExpr(%r, %r, %r)' % (
            self._ref, self._value, self._location
            )

class EncodingMultiMatch:
    '''A segment in an encoding sequence of zero or more elements, that will
    be filled in by a matched entry from an included mode.
    '''

    name = property(lambda self: self._name)
    mode = property(lambda self: self._mode)
    start = property(lambda self: self._start)
    location = property(lambda self: self._location)

    encodingWidth = property(lambda self:
        self._mode.encodingWidth
        if self._start == 0 else
        self._mode.auxEncodingWidth
        )
    auxEncodingWidth = property(lambda self: self._mode.auxEncodingWidth)

    def __init__(self, name, mode, start, location):
        self._name = name
        self._mode = mode
        self._start = start
        self._location = location

    def __repr__(self):
        return 'EncodingMultiMatch(%r, %r, %r, %r)' % (
            self._name, self._mode, self._start, self._location
            )

    @const_property
    def encodedLength(self):
        length = self._mode.encodedLength
        return None if length is None else length - self._start

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

    def tryDecode(self, fetcher, flags):
        '''Attempts to decode an instruction from the given fetcher.
        The flags argument is a set containing the decode flags that apply to
        the instruction being decoded.
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

    def tryDecode(self, fetcher, flags):
        for decoder in self._decoders:
            match = decoder.tryDecode(fetcher, flags)
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

    def tryDecode(self, fetcher, flags):
        encoded = fetcher[self._index]
        if encoded is None:
            return None
        decoder = self._table[(encoded & self._mask) >> self._offset]
        return decoder.tryDecode(fetcher, flags)

def _findFirstAuxIndex(encoding):
    '''Returns the index of the first encoding item that can match auxiliary
    encoding units, or None if no auxiliary encoding units can be matched.
    The given encoding sequence must not contain matchers that never match
    any encoding units.
    '''
    if len(encoding) == 0:
        # No units matched because there are no matchers.
        return None
    firstLen = encoding[0].encodedLength
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

class ModeEntry:
    '''One row in a mode table.
    '''

    def __init__(
            self, encoding, decoding, mnemonic, semantics, placeholders,
            flagsRequired, location
            ):
        # Filter out zero-length encoding items.
        # There is no good reason to ban them, but keeping them around would
        # unnecessarily complicate the code.
        self.encoding = encoding = tuple(
            encItem
            for encItem in encoding
            if checkType(
                encItem, (EncodingExpr, EncodingMultiMatch), 'encoding element'
                ).encodedLength != 0
            )
        self._firstAuxIndex = firstAuxIndex = _findFirstAuxIndex(encoding)

        self.decoding = decoding
        self.mnemonic = mnemonic
        self.semantics = semantics
        self.placeholders = placeholders
        self.flagsRequired = frozenset(flagsRequired)
        self.location = location

        # Verify that all auxiliary units have the same width.
        auxWidth = self.auxEncodingWidth
        if auxWidth is not None:
            consistent = True
            for encIdx, encItem in enumerate(encoding):
                if encIdx != 0:
                    consistent &= encItem.encodingWidth == auxWidth
                if isinstance(encItem, EncodingMultiMatch):
                    consistent &= encItem.auxEncodingWidth in (None, auxWidth)
            if not consistent:
                raise ValueError(
                    'Inconsistent widths among auxiliary encoding units'
                    )

        # Perform some basic analysis.
        determinePlaceholderRoles(semantics, placeholders)

    def __repr__(self):
        return 'ModeEntry(%r, %r, %r, %r, %r, %r, %r)' % (
            self.encoding, self.decoding, self.mnemonic, self.semantics,
            self.placeholders, self.flagsRequired, self.location
            )

    @property
    def encodingWidth(self):
        '''The width in bits a first encoding unit matched by this mode entry
        would have, or None if this entry always matches zero encoding units.
        '''
        encoding = self.encoding
        return None if len(encoding) == 0 else encoding[0].encodingWidth

    @property
    def encodingLocation(self):
        '''The InputLocation of the first encoding element in this mode entry.
        '''
        encoding = self.encoding
        if len(encoding) == 0:
            return self.location.updateSpan((0, 1))
        else:
            return encoding[0].location

    @property
    def auxEncodingWidth(self):
        '''The width in bits that all non-first encoding units matched by this
        mode entry would have, or None if a match cannot contain more than one
        encoding unit.
        '''
        firstAuxIndex = self._firstAuxIndex
        if firstAuxIndex is None:
            return None
        elif firstAuxIndex == 0:
            return self.encoding[0].auxEncodingWidth
        else:
            assert firstAuxIndex == 1, firstAuxIndex
            return self.encoding[1].encodingWidth

    @property
    def auxEncodingLocation(self):
        '''The InputLocation of the auxiliary encoding elements in this mode
        entry. If there are no auxiliary encoding elements, the end of the
        encoding field is returned.
        '''
        encoding = self.encoding
        firstAuxIndex = self._firstAuxIndex
        if firstAuxIndex is None:
            end = 0 if len(encoding) == 0 else encoding[0].location.span[1]
            return self.location.updateSpan((end, end + 1))
        else:
            return mergeSpan(
                encoding[firstAuxIndex].location,
                encoding[-1].location
                )

    @const_property
    def encodedLength(self):
        '''The number of encoded data units (bytes, words etc.) that this mode
        entry matches, or None if that number may vary depending on which match
        is made in an included mode.
        '''
        total = 0
        for encItem in self.encoding:
            length = encItem.encodedLength
            if length is None:
                return None
            total += length
        return total

    @const_property
    def decoder(self):
        '''A Decoder instance that decodes this entry.
        '''
        decoding = self.decoding
        encoding = self.encoding
        placeholders = self.placeholders

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
        decoder = MatchFoundDecoder(self)

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
                    sub = placeholder.mode.decoder
                    decoder = PlaceholderDecoder(name, None, decoder, sub, None)

        # Add matcher that checks decode flags.
        flagsRequired = self.flagsRequired
        if flagsRequired:
            decoder = FlagsCheckDecoder(flagsRequired, decoder)

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
                sub = matcher.mode.decoder
                decoder = PlaceholderDecoder(name, slices, decoder, sub, auxIdx)

        return decoder

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

    def tryDecode(self, fetcher, flags):
        encoded = fetcher[self._index]
        if encoded is None or encoded & self._mask != self._value:
            return None
        else:
            return self._next.tryDecode(fetcher, flags)

class FlagsCheckDecoder(Decoder):
    '''Decoder that only returns a match if all the required decode flags are
    present.
    '''

    def __init__(self, flags, nxt):
        self._flags = flags
        self._next = nxt

    def dump(self, indent='', submodes=True):
        flagsStr = ' '.join('?%s' % flag for flag in sorted(self._flags))
        self._next.dump(indent + flagsStr + ' -> ', submodes)

    def tryDecode(self, fetcher, flags):
        for flag in self._flags:
            if flag not in flags:
                return None
        else:
            return self._next.tryDecode(fetcher, flags)

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

    def tryDecode(self, fetcher, flags):
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
            decoded = sub.tryDecode(ModeFetcher(value, fetcher, auxIdx), flags)
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
        match = self._next.tryDecode(fetcher, flags)
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

    def tryDecode(self, fetcher, flags):
        return EncodeMatch(self._entry)

class NoMatchDecoder(Decoder, metaclass=Singleton):
    '''Decoder that is placed at the end of a decode tree.
    It never finds a match.
    '''

    def dump(self, indent='', submodes=True):
        print(indent + '(none)')

    def tryDecode(self, fetcher, flags):
        return None

def createDecoder(decoders):
    '''Returns a decoder that will decode using the last matching decoder among
    the given decoders.
    '''
    decoders = list(decoders)

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
                encIdx, tableMask, index << start, createDecoder(table[index])
                )

        # Create lookup table.
        return TableDecoder(
            (createDecoder(d) for d in table), encIdx, tableMask, start
            )

    # SequentialDecoder picks the first match, so reverse the order.
    decoders.reverse()
    return SequentialDecoder(decoders)

class EncodeMatch:
    '''A match on the encoding field of a mode entry.
    '''

    def __init__(self, entry):
        self._entry = entry
        self._mapping = {}

    def __repr__(self):
        return 'EncodeMatch(%r, %r)' % (self._entry, self._mapping)

    def __setitem__(self, key, value):
        assert key not in self._mapping, key
        self._mapping[key] = value

    @const_property
    def encodedLength(self):
        entry = self._entry
        length = entry.encodedLength
        if length is not None:
            # Mode entry has fixed encoded length.
            return length

        # Mode entry has variable encoded length.
        mapping = self._mapping
        length = 0
        for encItem in entry.encoding:
            if isinstance(encItem, EncodingExpr):
                length += 1
            elif isinstance(encItem, EncodingMultiMatch):
                length += mapping[encItem.name].encodedLength - encItem.start
            else:
                assert False, encItem
        return length

    def _substMapping(self, expr, pc):
        if isinstance(expr, Immediate):
            name = expr.name
            return IntLiteral(pc if name == 'pc' else self._mapping[name])
        return None

    def iterMnemonic(self, pc):
        '''Yields a mnemonic representation of this match.
        The given program counter is the value the 'pc' register reads as,
        typically the address after the instruction.
        '''
        entry = self._entry
        mapping = self._mapping
        placeholders = entry.placeholders
        for mnemElem in entry.mnemonic:
            if isinstance(mnemElem, str):
                yield mnemElem
            elif isinstance(mnemElem, int):
                yield mnemElem, IntType.int, frozenset()
            elif isinstance(mnemElem, MatchPlaceholder):
                match = mapping[mnemElem.name]
                for subElem in match.iterMnemonic(pc):
                    if isinstance(subElem, str):
                        yield subElem
                    else:
                        value, typ, roles = subElem
                        yield value, typ, roles | mnemElem.roles
            elif isinstance(mnemElem, ValuePlaceholder):
                name = mnemElem.name
                typ = mnemElem.type
                value = mapping.get(name)
                if value is None:
                    expr = simplifyExpression(mnemElem.value.substitute(
                        lambda expr, pc=pc: self._substMapping(expr, pc)
                        ))
                    if isinstance(expr, IntLiteral):
                        yield expr.value, typ, mnemElem.roles
                    else:
                        # TODO: Is this a bug? A definition error?
                        #       Or can it happen normally?
                        yield name
                else:
                    if typ.signed:
                        width = typ.width
                        if value >= 1 << (width - 1):
                            value -= 1 << width
                    yield value, typ, mnemElem.roles
            else:
                assert False, mnemElem

def _formatEncodingWidth(width):
    return 'empty encoding' if width is None else 'encoding width %d' % width

def _formatAuxEncodingWidth(width):
    return (
        'no auxiliary encoding items'
        if width is None
        else 'auxiliary encoding width %d' % width
        )

class ModeTable:
    '''Abstract base class for mode tables.
    '''

    encodingWidth = property(lambda self: self._encWidth)
    auxEncodingWidth = property(lambda self: self._auxEncWidth)

    def __init__(self, encWidth, auxEncWidth, entries):
        if encWidth is unlimited or auxEncWidth is unlimited:
            raise ValueError('Unlimited width is not allowed for encoding')
        self._encWidth = encWidth
        self._auxEncWidth = auxEncWidth

        self._entries = entries = tuple(entries)
        for entry in entries:
            assert isinstance(entry, ModeEntry), entry
            if entry.encodingWidth != encWidth:
                raise ValueError(
                    'Mode with %s contains entry with %s' % (
                        _formatEncodingWidth(encWidth),
                        _formatEncodingWidth(entry.encodingWidth)
                        )
                    )
            if entry.auxEncodingWidth not in (None, auxEncWidth):
                raise ValueError(
                    'Mode with %s contains entry with %s' % (
                        _formatAuxEncodingWidth(auxEncWidth),
                        _formatAuxEncodingWidth(entry.auxEncodingWidth)
                        )
                    )

        self._mnemTree = ({}, [])
        for entry in entries:
            self._updateMnemTree(entry)

    def dumpMnemonicTree(self):
        def matchKey(match):
            if isinstance(match, str):
                return 0, match
            elif isinstance(match, Mode):
                return 1, match.name
            elif match is int:
                return 2, None
            else:
                assert False, match
        def dumpNode(node, indent):
            for entry in node[1]:
                tokens = ' '.join(str(token) for token in entry.mnemonic)
                if entry.flagsRequired:
                    flags = ' -- prefix ' + ' '.join(
                        sorted(entry.flagsRequired)
                        )
                else:
                    flags = ''
                print('%s= %s%s' % (indent, tokens, flags))
            for match in sorted(node[0].keys(), key=matchKey):
                print('%s+ %s' % (indent, match))
                dumpNode(node[0][match], ' ' * len(indent) + '`---')
        dumpNode(self._mnemTree, '')

    def _updateMnemTree(self, entry):
        # Update match tree for mnemonics.
        mnemonic = entry.mnemonic
        def addMnemonic(node, idx):
            if idx == len(mnemonic):
                node[1].append(entry)
            else:
                token = mnemonic[idx]
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
    def encodedLength(self):
        '''The number of encoded data units (bytes, words etc.) that all
        entries in this mode use, or None if that number may vary depending
        on which match is made.
        '''
        if self._encWidth is None:
            return 0
        if self._auxEncWidth is None:
            return 1
        commonLen = None
        for entry in self._entries:
            entryLen = entry.encodedLength
            if entryLen is None:
                return None
            if entryLen != commonLen:
                if commonLen is None:
                    commonLen = entryLen
                else:
                    return None
        assert commonLen is not None, self
        return commonLen

    @const_property
    def decoder(self):
        '''A Decoder instance that decodes the entries in this table.
        '''
        return createDecoder(entry.decoder for entry in self._entries)

class Mode(ModeTable):
    '''A pattern for operands, such as an addressing mode or a table defining
    register encoding.
    '''

    name = property(lambda self: self._name)
    semanticsType = property(lambda self: self._semType)
    location = property(lambda self: self._location)

    def __init__(self, name, encWidth, auxEncWidth, semType, location, entries):
        ModeTable.__init__(self, encWidth, auxEncWidth, entries)
        self._name = name
        self._semType = semType
        self._location = location

    def __str__(self):
        return 'mode %s %s' % (self._semType, self._name)

    def __iter__(self):
        return iter(self._entries)

class Placeholder:
    '''Abstract base class for a mode context element.
    '''

    name = property(lambda self: self._name)
    roles = property(lambda self: frozenset(self._roles))

    def __init__(self, name):
        self._name = name
        self._roles = set()

    def addRole(self, role):
        self._roles.add(checkType(role, PlaceholderRole, 'role'))

class ValuePlaceholder(Placeholder):
    '''An element from a mode context that represents a numeric value.
    '''

    type = property(lambda self: self._type)
    value = property(lambda self: self._value)

    def __init__(self, name, typ, value):
        Placeholder.__init__(self, name)
        self._type = typ
        self._value = None if value is None else simplifyExpression(value)

    def __repr__(self):
        return 'ValuePlaceholder(%r, %r, %r)' % (
            self._name, self._type, self._value
            )

    def __str__(self):
        value = self._value
        if value is None:
            return '{%s %s}' % (self._type, self._name)
        else:
            return '{%s %s = %s}' % (self._type, self._name, self._value)

class MatchPlaceholder(Placeholder):
    '''An element from a mode context that will be filled in by a match made
    in a different mode table.
    '''

    mode = property(lambda self: self._mode)

    def __init__(self, name, mode):
        Placeholder.__init__(self, name)
        self._mode = mode

    def __repr__(self):
        return 'MatchPlaceholder(%r, %r)' % (self._name, self._mode)

    def __str__(self):
        return '{%s %s}' % (self._mode.name, self._name)

class Immediate(Expression):
    '''A constant value defined as part of an instruction.
    Note that the name of an immediate is unique only within the mode entry
    that declares it, not in mode entries that include it.
    '''
    __slots__ = ('_name', '_type')

    name = property(lambda self: self._name)
    type = property(lambda self: self._type)
    width = property(lambda self: self._type.width)
    mask = property(lambda self: self._type.mask)

    def __init__(self, name, typ):
        Expression.__init__(self)
        self._name = name
        self._type = typ

    def _ctorargs(self):
        return self._name, self._type

    def __str__(self):
        return self._name

    def _equals(self, other):
        # pylint: disable=protected-access
        return self._name == other._name
