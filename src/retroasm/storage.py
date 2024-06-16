from __future__ import annotations

from collections.abc import Callable, Iterator
from typing import cast

from .expression import Expression
from .types import IntType, Width


class IOChannel:
    """A channel through which a CPU can do input and output."""

    __slots__ = ("_name", "_elem_type", "_addr_type")

    @property
    def name(self) -> str:
        return self._name

    @property
    def elem_type(self) -> IntType:
        return self._elem_type

    @property
    def addr_type(self) -> IntType:
        return self._addr_type

    def __init__(self, name: str, elem_type: IntType, addr_type: IntType):
        self._name = name
        self._elem_type = elem_type
        self._addr_type = addr_type

    def __repr__(self) -> str:
        return f"IOChannel({self._name!r}, {self._elem_type!r}, {self._addr_type!r})"

    def __str__(self) -> str:
        return f"{self._elem_type} {self._name}[{self._addr_type}]"

    # TODO: Allow the system model to provide a more accurate responses
    #       by examining the index.

    # pylint: disable=unused-argument

    def can_load_have_side_effect(self, index: Expression) -> bool:
        """
        Returns True if reading from this channel at the given index
        might have an effect other than fetching the value. For example
        reading a peripheral's status register might reset a flag.
        The index is an Expression which might provide some additional
        information about which part of the channel is being read.
        """
        return True

    def can_store_have_side_effect(self, index: Expression) -> bool:
        """
        Returns True if writing to this channel at the given index
        might have an effect other than setting the value. For example
        writing a peripheral's control register might change its output.
        The index is an Expression which might provide some additional
        information about which part of the channel is being written.
        """
        return True

    def is_load_consistent(self, index: Expression) -> bool:
        """
        Returns True if reading from this channel at the given index
        twice in succession will return the same value both times.
        The index is an Expression which might provide some additional
        information about which part of the channel is being read.
        """
        return False

    def is_sticky(self, index: Expression) -> bool:
        """
        Returns True if reading from this channel at the give index after
        it is written at that same index will return the written value.
        If access at another index inbetween the write and read can change
        the value, the given index is not considered sticky (return False).
        The index is an Expression which might provide some additional
        information about which part of the channel is being accessed.
        """
        return False

    def might_be_same(self, index1: Expression, index2: Expression) -> bool:
        """
        Returns True if the storages at the two given indices might be the
        same, either because the indices might be equal or because multiple
        indices can point to the same storage.
        """
        return True


class Storage:
    """A location in which bits can be stored."""

    __slots__ = ("_width",)

    @property
    def width(self) -> Width:
        return self._width

    def __init__(self, width: Width):
        self._width = width
        if width < 0:
            raise ValueError(
                f"storage width must not be negative: {cast(int, width):d}"
            )

    def can_load_have_side_effect(self) -> bool:
        """
        Returns True if reading from this storage might have an effect
        other than fetching the value. For example reading a peripheral's
        status register might reset a flag.
        """
        raise NotImplementedError

    def can_store_have_side_effect(self) -> bool:
        """
        Returns True if writing to this storage might have an effect
        other than setting the value. For example writing a peripheral's
        control register might change its output.
        """
        raise NotImplementedError

    def is_load_consistent(self) -> bool:
        """
        Returns True if reading this storage twice in succession will
        return the same value both times.
        """
        raise NotImplementedError

    def is_sticky(self) -> bool:
        """
        Returns True if reading this storage after it is written will
        return the written value.
        """
        raise NotImplementedError

    def might_be_same(self, other: Storage) -> bool:
        """
        Returns True if the given storage might be the same storage as
        this one: if it is either certainly the same or if it might be an
        alias.
        """
        raise NotImplementedError

    def iter_expressions(self) -> Iterator[Expression]:
        """Iterates through the expressions in this storage, if any."""
        return iter(())

    def substitute_expressions(
        # pylint: disable=unused-argument
        # The default implementation doesn't use the argument, but subclasses do.
        self,
        func: Callable[[Expression], Expression | None],
    ) -> Storage:
        """
        Applies the given substitution function to the expressions in this
        storage, if any.
        See Expression.substitute() for details about the substitution function.
        Returns a new version of storage with its expressions replaced if any
        substitution occurred, or this storage otherwise.
        """
        return self


class Register(Storage):
    """
    A processor register.
    """

    __slots__ = ("_name",)

    @property
    def name(self) -> str:
        return self._name

    def __init__(self, name: str, width: Width):
        Storage.__init__(self, width)
        self._name = name

    def __str__(self) -> str:
        return f"reg{self._width} {self._name}"

    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self._name}, {self._width})"

    def can_load_have_side_effect(self) -> bool:
        return False

    def can_store_have_side_effect(self) -> bool:
        return False

    def is_load_consistent(self) -> bool:
        return True

    def is_sticky(self) -> bool:
        return True

    def might_be_same(self, other: Storage) -> bool:
        # Register might be passed by reference.
        return self is other or isinstance(other, ArgStorage)


class ArgStorage(Storage):
    """
    A placeholder storage location for a storage passed to a function.
    The storage properties depend on which concrete storage will be passed,
    so until we know the concrete storage we have to assume the worst case.
    """

    __slots__ = ("_name",)

    @property
    def name(self) -> str:
        return self._name

    def __init__(self, name: str, width: Width):
        self._name = name
        Storage.__init__(self, width)

    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self._name!r}, {self._width})"

    def __str__(self) -> str:
        return self._name

    def can_load_have_side_effect(self) -> bool:
        return True

    def can_store_have_side_effect(self) -> bool:
        return True

    def is_load_consistent(self) -> bool:
        return False

    def is_sticky(self) -> bool:
        return False

    def might_be_same(self, other: Storage) -> bool:
        return True


class IOStorage(Storage):
    """Storage location accessed via an I/O channel at a particular index."""

    __slots__ = ("_channel", "_index")

    @property
    def channel(self) -> IOChannel:
        return self._channel

    @property
    def index(self) -> Expression:
        return self._index

    def __init__(self, channel: IOChannel, index: Expression):
        self._channel = channel
        self._index = index
        Storage.__init__(self, channel.elem_type.width)

    def __repr__(self) -> str:
        return f"IOStorage({self._channel!r}, {self._index!r})"

    def __str__(self) -> str:
        return f"{self._channel.name}[{self._index}]"

    def __eq__(self, other: object) -> bool:
        return (
            isinstance(other, IOStorage)
            and self._channel is other._channel
            and self._index == other._index
        )

    def __hash__(self) -> int:
        return hash((self._channel, self._index))

    def can_load_have_side_effect(self) -> bool:
        return self._channel.can_load_have_side_effect(self._index)

    def can_store_have_side_effect(self) -> bool:
        return self._channel.can_store_have_side_effect(self._index)

    def is_load_consistent(self) -> bool:
        return self._channel.is_load_consistent(self._index)

    def is_sticky(self) -> bool:
        return self._channel.is_sticky(self._index)

    def might_be_same(self, other: Storage) -> bool:
        if isinstance(other, IOStorage):
            # TODO: This is an oversimplification: some MSX devices have their
            #       registers both I/O-mapped and memory-mapped.
            return self._channel == other._channel and self._channel.might_be_same(
                self._index, other._index
            )
        else:
            return isinstance(other, ArgStorage)

    def iter_expressions(self) -> Iterator[Expression]:
        yield self._index

    def substitute_expressions(
        self, func: Callable[[Expression], Expression | None]
    ) -> IOStorage:
        index = self._index
        new_index = index.substitute(func)
        if new_index is index:
            return self
        else:
            return IOStorage(self._channel, new_index)


class Keeper(Storage):
    """
    Storage location used to artificially force a load or store
    to not be optimized out.
    """

    __slots__ = ()

    def __repr__(self) -> str:
        return f"Keeper({self._width})"

    def __str__(self) -> str:
        return f"keep{self._width}"

    def can_load_have_side_effect(self) -> bool:
        return True

    def can_store_have_side_effect(self) -> bool:
        return True

    def is_load_consistent(self) -> bool:
        return False

    def is_sticky(self) -> bool:
        return False

    def might_be_same(self, other: Storage) -> bool:
        return self is other
