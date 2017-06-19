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
