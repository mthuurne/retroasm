from __future__ import annotations

from collections.abc import Callable
from functools import partial

from .binfmt import Image
from .section import ByteOrder


class Fetcher:
    """
    Instruction fetcher interface.

    Each fetcher represents a specific location in the program.
    """

    __slots__ = ()

    def __getitem__(self, index: int, /) -> int | None:
        """
        Return the encoded item at the given index, or None if the index
        is out of range.

        An encoded item is the smallest unit in instruction encoding;
        typically it is 8, 16 or 32 bits wide.
        Some instruction sets have one encoded item per instruction,
        others have variable instruction length.
        """
        raise NotImplementedError


class AdvancingFetcher(Fetcher):
    """An instruction fetcher that can advance to a new program location."""

    __slots__ = ()

    def advance(self, steps: int = 1) -> AdvancingFetcher:
        """
        Return a new instruction fetcher of the same type, with a program location
        that is the given number of encoding units advanced beyond ours.
        """
        raise NotImplementedError


class FetcherBase(Fetcher):
    """Abstract base class for instruction fetchers."""

    __slots__ = ("_cached",)

    def __init__(self) -> None:
        super().__init__()
        self._cached = self._fetch(0)

    def __getitem__(self, index: int, /) -> int | None:
        if index == 0:
            # Fast path for most frequently used index.
            return self._cached
        elif index < 0:
            raise IndexError(f"fetcher index must not be negative: {index:d}")
        else:
            return self._fetch(index)

    def _fetch(self, index: int) -> int | None:
        """
        Return the data unit at the given index, or None if the index is out of range.
        """
        raise NotImplementedError


class ImageFetcher(FetcherBase, AdvancingFetcher):
    """Abstract base class for instruction fetchers that read from an image."""

    __slots__ = ("_image", "_offset", "_end", "_num_bytes")

    @staticmethod
    def factory(
        width: int, byte_order: ByteOrder
    ) -> Callable[[Image, int, int], ImageFetcher]:
        """
        Return a factory which builds instruction fetchers using the given instruction
        width and byte order.

        The returned factory function takes three positional arguments:
        the image, the start offset and the end offset.

        Raise ValueError if no fetcher can be made for the given width and byte order.
        """
        if width % 8 != 0:
            raise ValueError(f"expected width to be a multiple of 8 bits, got {width}")
        num_bytes = width // 8
        if num_bytes == 1:
            return ByteFetcher
        elif byte_order is ByteOrder.big:
            return partial(BigEndianFetcher, num_bytes=num_bytes)
        elif byte_order is ByteOrder.little:
            return partial(LittleEndianFetcher, num_bytes=num_bytes)
        else:
            assert byte_order is ByteOrder.undefined, byte_order
            raise ValueError("byte order unknown")

    def __init__(self, image: Image, start: int, end: int, num_bytes: int):
        self._image = image
        self._offset = start
        self._end = end
        self._num_bytes = num_bytes
        super().__init__()

    def __repr__(self) -> str:
        return (
            f"{self.__class__.__name__}({self._image!r}, "
            f"{self._offset:d}, "
            f"{self._end:d}, "
            f"{self._num_bytes:d})"
        )

    @property
    def image(self) -> Image:
        return self._image

    @property
    def offset(self) -> int:
        return self._offset

    @property
    def end(self) -> int:
        return self._end

    @property
    def num_bytes(self) -> int:
        return self._num_bytes

    def _fetch(self, index: int) -> int | None:
        raise NotImplementedError

    def advance(self, steps: int = 1) -> ImageFetcher:
        """
        Returns a new fetcher of the same type, with an offset that is one
        one data unit advanced beyond our offset.
        """
        return self.__class__(
            self._image,
            self._offset + steps * self._num_bytes,
            self._end,
            self._num_bytes,
        )


class ByteFetcher(ImageFetcher):
    """Instruction fetcher that reads individual bytes from an image."""

    __slots__ = ()

    def __init__(self, image: Image, start: int, end: int, num_bytes: int = 1):
        ImageFetcher.__init__(self, image, start, end, num_bytes)

    def _fetch(self, index: int) -> int | None:
        offset = self._offset + index
        return self._image[offset] if offset < self._end else None


class MultiByteFetcher(ImageFetcher):
    """
    Abstract base class for instruction fetchers that read multi-byte units
    from an image.
    """

    __slots__ = ()

    def _fetch(self, index: int) -> int | None:
        num_bytes = self._num_bytes
        offset = self._offset + index * num_bytes
        after = offset + num_bytes
        if after > self._end:
            return None
        else:
            return self._fetch_range(offset, after)

    def _fetch_range(self, start: int, end: int) -> int:
        """Returns the data unit between the given byte offsets."""
        raise NotImplementedError


class BigEndianFetcher(MultiByteFetcher):
    """
    Instruction fetcher that reads multi-byte units in big endian byte order
    from an image.
    """

    __slots__ = ()

    def _fetch_range(self, start: int, end: int) -> int:
        image = self._image
        value = 0
        for byte in image[start:end]:
            value <<= 8
            value |= byte
        return value


class LittleEndianFetcher(MultiByteFetcher):
    """
    Instruction fetcher that reads multi-byte units in little endian byte
    order from an image.
    """

    __slots__ = ()

    def _fetch_range(self, start: int, end: int) -> int:
        image = self._image
        value = 0
        shift = 0
        for byte in image[start:end]:
            value |= byte << shift
            shift += 8
        return value


class ModeFetcher(FetcherBase):
    """Instruction fetcher for looking up an entry in a mode table."""

    __slots__ = ("_first", "_aux_fetcher", "_aux_index")

    def __init__(self, first: int | None, aux_fetcher: Fetcher, aux_index: int | None):
        self._first = first
        self._aux_fetcher = aux_fetcher
        self._aux_index = aux_index
        super().__init__()

    def _fetch(self, index: int) -> int | None:
        first = self._first
        if first is not None:
            if index == 0:
                return first
            else:
                index -= 1

        aux_index = self._aux_index
        if aux_index is None:
            return None
        else:
            return self._aux_fetcher[aux_index + index]


class AfterModeFetcher(FetcherBase):
    """Instruction fetcher that adjusts indexing after a multi-match."""

    __slots__ = ("_fetcher", "_auxIndex", "_delta")

    def __init__(self, fetcher: Fetcher, aux_index: int, delta: int):
        self._fetcher = fetcher
        self._auxIndex = aux_index
        self._delta = delta
        super().__init__()

    def _fetch(self, index: int) -> int | None:
        if index >= self._auxIndex:
            assert index > self._auxIndex
            index += self._delta
        return self._fetcher[index]
