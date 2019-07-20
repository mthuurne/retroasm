from __future__ import annotations

from typing import Optional, Sequence


class Fetcher:
    '''Abstract base class for instruction fetchers.
    '''
    __slots__ = ('_cached',)

    def __init__(self) -> None:
        self._cached = self._fetch(0)

    def __getitem__(self, key: int) -> Optional[int]:
        if key == 0:
            # Fast path for most frequently used index.
            return self._cached
        elif isinstance(key, int):
            if key < 0:
                raise IndexError(f'fetcher index must not be negative: {key:d}')
            else:
                return self._fetch(key)
        else:
            raise TypeError(
                f'fetcher index must be integer, not {type(key).__name__}'
                )

    def _fetch(self, index: int) -> Optional[int]:
        '''Returns the data unit at the given index, or None if the index is
        out of range.
        '''
        raise NotImplementedError

class ImageFetcher(Fetcher):
    '''Abstract base class for instruction fetchers that read from an image.
    '''
    __slots__ = ('_image', '_offset', '_end', '_numBytes')

    def __init__(self,
                 image: Sequence[int],
                 start: int,
                 end: int,
                 numBytes: int
                 ):
        self._image = image
        self._offset = start
        self._end = end
        self._numBytes = numBytes
        Fetcher.__init__(self)

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}({self._image!r}, ' \
                                         f'{self._offset:d}, ' \
                                         f'{self._end:d}, ' \
                                         f'{self._numBytes:d})'

    @property
    def image(self) -> Sequence[int]:
        return self._image

    @property
    def offset(self) -> int:
        return self._offset

    @property
    def end(self) -> int:
        return self._end

    @property
    def numBytes(self) -> int:
        return self._numBytes

    def _fetch(self, index: int) -> Optional[int]:
        raise NotImplementedError

    def advance(self, steps: int = 1) -> ImageFetcher:
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

    def __init__(self,
                 image: Sequence[int],
                 start: int,
                 end: int,
                 numBytes: int = 1
                 ):
        ImageFetcher.__init__(self, image, start, end, numBytes)

    def _fetch(self, index: int) -> Optional[int]:
        offset = self._offset + index
        return self._image[offset] if offset < self._end else None

class MultiByteFetcher(ImageFetcher):
    '''Abstract base class for instruction fetchers that read multi-byte units
    from an image.
    '''
    __slots__ = ()

    def _fetch(self, index: int) -> Optional[int]:
        numBytes = self._numBytes
        offset = self._offset + index * numBytes
        after = offset + numBytes
        if after > self._end:
            return None
        else:
            return self._fetchRange(offset, after)

    def _fetchRange(self, start: int, end: int) -> int:
        '''Returns the data unit between the given byte offsets.
        '''
        raise NotImplementedError

class BigEndianFetcher(MultiByteFetcher):
    '''Instruction fetcher that reads multi-byte units in big endian byte order
    from an image.
    '''
    __slots__ = ()

    def _fetchRange(self, start: int, end: int) -> int:
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

    def _fetchRange(self, start: int, end: int) -> int:
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
    __slots__ = ('_first', '_auxFetcher', '_auxIndex')

    def __init__(self,
                 first: Optional[int],
                 auxFetcher: Fetcher,
                 auxIndex: Optional[int]
                 ):
        self._first = first
        self._auxFetcher = auxFetcher
        self._auxIndex = auxIndex
        Fetcher.__init__(self)

    def _fetch(self, index: int) -> Optional[int]:
        first = self._first
        if first is not None:
            if index == 0:
                return first
            else:
                index -= 1

        auxIndex = self._auxIndex
        if auxIndex is None:
            return None
        else:
            return self._auxFetcher._fetch(auxIndex + index)

class AfterModeFetcher(Fetcher):
    '''Instruction fetcher that adjusts indexing after a multi-match.
    '''
    __slots__ = ('_fetcher', '_auxIndex', '_delta')

    def __init__(self,
                 fetcher: Fetcher,
                 auxIndex: int,
                 delta: int
                 ):
        self._fetcher = fetcher
        self._auxIndex = auxIndex
        self._delta = delta
        Fetcher.__init__(self)

    def _fetch(self, index: int) -> Optional[int]:
        if index >= self._auxIndex:
            assert index > self._auxIndex
            index += self._delta
        return self._fetcher._fetch(index)
