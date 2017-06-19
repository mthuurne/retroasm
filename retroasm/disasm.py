from .mode import EncodeMatch, PlaceholderRole
from .section import ByteOrder
from .types import IntType

class Fetcher:
    __slots__ = ('_image', '_offset', '_end', '_numBytes', '_cached')

    image = property(lambda self: self._image)
    offset = property(lambda self: self._offset)
    end = property(lambda self: self._end)
    numBytes = property(lambda self: self._numBytes)

    def __init__(self, image, start, end, numBytes):
        self._image = image
        self._offset = start
        self._end = end
        self._numBytes = numBytes
        self._cached = self._fetch(start)

    def __getitem__(self, key):
        if key is 0:
            # Fast path for most frequently used index.
            return self._cached
        elif isinstance(key, int):
            if key < 0:
                raise IndexError('fetcher index must not be negative: %d' % key)
            else:
                return self._fetch(self._offset + key * self._numBytes)
        else:
            raise TypeError(
                'fetcher index must be integer, not %s' % type(key).__name__
                )

    def _fetch(self, offset):
        '''Returns the data unit at the given byte offset, or None if the
        offset is out of range.
        '''
        raise NotImplementedError

    def advance(self, steps=1):
        '''Returns a new fetcher of the same type, with an offset that is one
        one data unit advanced beyond our offset.
        '''
        return self.__class__(
            self._image,
            self._offset + steps * self._numBytes,
            self._end,
            self._numBytes
            )

class ByteFetcher(Fetcher):
    __slots__ = ()

    def __init__(self, image, start, end, numBytes=1):
        Fetcher.__init__(self, image, start, end, numBytes)

    def _fetch(self, offset):
        return self._image[offset] if offset < self._end else None

class BigEndianFetcher(Fetcher):
    __slots__ = ()

    def _fetch(self, offset):
        after = offset + self._numBytes
        if after > self._end:
            return None
        else:
            image = self._image
            value = 0
            for byte in image[offset:after]:
                value <<= 8
                value |= byte
            return value

class LittleEndianFetcher(Fetcher):
    __slots__ = ()

    def _fetch(self, offset):
        after = offset + self._numBytes
        if after > self._end:
            return None
        else:
            image = self._image
            value = 0
            shift = 0
            for byte in image[offset:after]:
                value |= byte << shift
                shift += 8
            return value

def disassemble(instrSet, fetcher, startAddr, formatter):
    '''Disassemble instructions from the given fetcher.
    The fetched data is assumed to be code for the given instruction set,
    to be executed at the given address.
    '''
    decoder = instrSet.decoder
    numBytes = fetcher.numBytes
    encType = IntType.u(numBytes * 8)

    decoded = {}
    codeAddrs = set()
    dataAddrs = set()
    addr = startAddr
    while True:
        encoded = fetcher[0]
        if encoded is None:
            break
        match = decoder.tryDecode(encoded)
        decoded[addr] = encoded if match is None else match
        fetcher = fetcher.advance()
        addr += numBytes
        if match is not None:
            for mnemElem in match.iterMnemonic(addr):
                if not isinstance(mnemElem, str):
                    value, typ, roles = mnemElem
                    if PlaceholderRole.code_addr in roles:
                        codeAddrs.add(value)
                    if PlaceholderRole.data_addr in roles:
                        dataAddrs.add(value)

    addrWidth = instrSet.addrWidth
    labels = {}
    dataLabelFormat = 'data_{:0%dx}' % ((addrWidth + 3) // 4)
    for addr in dataAddrs:
        labels[addr] = dataLabelFormat.format(addr)
    codeLabelFormat = 'code_{:0%dx}' % ((addrWidth + 3) // 4)
    for addr in codeAddrs:
        labels[addr] = codeLabelFormat.format(addr)

    for addr in sorted(decoded.keys()):
        label = labels.get(addr)
        if label is not None:
            print(formatter.formatLabel(label))
        match = decoded[addr]
        if isinstance(match, EncodeMatch):
            print(formatter.formatMnemonic(
                match.iterMnemonic(addr + numBytes),
                labels
                ))
        else:
            print(formatter.formatData(match, encType))
