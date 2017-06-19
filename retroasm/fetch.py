class Fetcher:
    '''Abstract base class for instruction fetchers.
    '''
    __slots__ = ('_cached',)

    def __init__(self):
        self._cached = self._fetch(0)

    def __getitem__(self, key):
        if key is 0:
            # Fast path for most frequently used index.
            return self._cached
        elif isinstance(key, int):
            if key < 0:
                raise IndexError('fetcher index must not be negative: %d' % key)
            else:
                return self._fetch(key)
        else:
            raise TypeError(
                'fetcher index must be integer, not %s' % type(key).__name__
                )

    def _fetch(self, index):
        '''Returns the data unit at the given index, or None if the index is
        out of range.
        '''
        raise NotImplementedError

class ImageFetcher(Fetcher):
    '''Abstract base class for instruction fetchers that read from an image.
    '''
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
        Fetcher.__init__(self)

    def __repr__(self):
        return '%s(%r, %d, %d, %d)' % (
            self.__class__.__name__,
            self._image, self._offset, self._end, self._numBytes
            )

    def _fetch(self, index):
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

class ByteFetcher(ImageFetcher):
    '''Instruction fetcher that reads individual bytes from an image.
    '''
    __slots__ = ()

    def __init__(self, image, start, end, numBytes=1):
        ImageFetcher.__init__(self, image, start, end, numBytes)

    def _fetch(self, index):
        offset = self._offset + index
        return self._image[offset] if offset < self._end else None

class MultiByteFetcher(ImageFetcher):
    '''Abstract base class for instruction fetchers that read multi-byte units
    from an image.
    '''
    __slots__ = ()

    def _fetch(self, index):
        numBytes = self._numBytes
        offset = self._offset + index * numBytes
        after = offset + numBytes
        if after > self._end:
            return None
        else:
            return self._fetchRange(offset, after)

    def _fetchRange(self, start, end):
        '''Returns the data unit between the given byte offsets.
        '''
        raise NotImplementedError

class BigEndianFetcher(MultiByteFetcher):
    '''Instruction fetcher that reads multi-byte units in big endian byte order
    from an image.
    '''
    __slots__ = ()

    def _fetchRange(self, start, end):
        image = self._image
        value = 0
        for byte in image[start:end]:
            value <<= 8
            value |= byte
        return value

class LittleEndianFetcher(MultiByteFetcher):
    '''Instruction fetcher that reads multi-byte units in little endian byte
    order from an image.
    '''
    __slots__ = ()

    def _fetchRange(self, start, end):
        image = self._image
        value = 0
        shift = 0
        for byte in image[start:end]:
            value |= byte << shift
            shift += 8
        return value

class ModeFetcher(Fetcher):
    '''Instruction fetcher for looking up an entry in a mode table.
    '''
    __slots__ = ('_first',)

    def __init__(self, first):
        self._first = first
        Fetcher.__init__(self)

    def _fetch(self, index):
        return self._first if index == 0 else None
