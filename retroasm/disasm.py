from .section import ByteOrder
from .types import IntType

class Fetcher:
    __slots__ = ('_image', '_offset', '_end', '_numBytes')

    image = property(lambda self: self._image)
    offset = property(lambda self: self._offset)
    end = property(lambda self: self._end)
    numBytes = property(lambda self: self._numBytes)

    def __init__(self, image, start, end, numBytes):
        self._image = image
        self._offset = start
        self._end = end
        self._numBytes = numBytes

    def fetch(self):
        '''Returns the data unit at the offset, or None if the offset is out
        of range.
        '''
        raise NotImplementedError

    def advance(self):
        '''Returns a new fetcher of the same type, with an offset that is one
        one data unit advanced beyond our offset.
        '''
        ret = Fetcher(
            self._image,
            self._offset + self._numBytes,
            self._end,
            self._numBytes
            )
        ret.__class__ = self.__class__
        return ret

class ByteFetcher(Fetcher):
    __slots__ = ()

    def __init__(self, image, start, end):
        Fetcher.__init__(self, image, start, end, 1)

    def fetch(self):
        offset = self._offset
        return self._image[offset] if offset < self._end else None

class BigEndianFetcher(Fetcher):
    __slots__ = ()

    def __init__(self, image, start, end, width):
        if width % 8 != 0:
            raise ValueError('width must be a multiple of 8: %d', width)
        Fetcher.__init__(self, image, start, end, width // 8)

    def fetch(self):
        offset = self._offset
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

    def __init__(self, image, start, end, width):
        if width % 8 != 0:
            raise ValueError('width must be a multiple of 8: %d', width)
        Fetcher.__init__(self, image, start, end, width // 8)

    def fetch(self):
        offset = self._offset
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

def disassemble(instrSet, fetcher, addr, formatter):
    '''Disassemble instructions from the given fetcher.
    The fetched data is assumed to be code for the given instruction set,
    to be executed at the given address.
    '''
    decoder = instrSet.decoder
    numBytes = fetcher.numBytes
    encType = IntType.u(numBytes * 8)
    while True:
        encoded = fetcher.fetch()
        if encoded is None:
            break
        match = decoder.tryDecode(encoded)
        fetcher = fetcher.advance()
        addr += numBytes
        if match is None:
            print(formatter.formatData(encoded, encType))
        else:
            print(formatter.formatMnemonic(match.iterMnemonic(addr)))
