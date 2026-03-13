from __future__ import annotations

from abc import ABC, abstractmethod
from collections.abc import Callable, Iterator
from typing import Final, cast, override

from .expression import Expression, XorOperator, ZeroTest, is_literal_false
from .expression_simplifier import simplify_expression
from .types import IntType, Width


class IOChannel:
    """A channel through which a CPU can do input and output."""

    __slots__ = ("name", "elem_type", "addr_type")

    def __init__(self, name: str, elem_type: IntType, addr_type: IntType):
        self.name: Final[str] = name
        self.elem_type: Final[IntType] = elem_type
        self.addr_type: Final[IntType] = addr_type

    @override
    def __repr__(self) -> str:
        return (
            f"{self.__class__.__name__}({self.name!r}, {self.elem_type!r}, {self.addr_type!r})"
        )

    @override
    def __str__(self) -> str:
        return f"{self.elem_type} {self.name}[{self.addr_type}]"

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


class RAMChannel(IOChannel):
    """An I/O channel connected to a read/write memory without side effects and aliasing."""

    __slots__ = ()

    @override
    def can_load_have_side_effect(self, index: Expression) -> bool:
        return False

    @override
    def can_store_have_side_effect(self, index: Expression) -> bool:
        return False

    @override
    def is_load_consistent(self, index: Expression) -> bool:
        return True

    @override
    def is_sticky(self, index: Expression) -> bool:
        return True

    @override
    def might_be_same(self, index1: Expression, index2: Expression) -> bool:
        return not is_literal_false(simplify_expression(ZeroTest(XorOperator(index1, index2))))


class Storage(ABC):
    """A location in which bits can be stored."""

    __slots__ = ("width", "traceable")

    def __init__(self, width: Width):
        if width < 0:
            raise ValueError(f"storage width ({cast(int, width):d}) cannot be negative")
        self.width: Final[Width] = width

        self.traceable: Final[bool] = (
            not self.can_load_have_side_effect()
            and not self.can_store_have_side_effect()
            and self.is_load_consistent()
            and self.is_sticky()
        )
        """
        Can we eliminate loads and stores on this storage by tracing its value?
        For this to be possible, the storage must be side-effect free, have a consistent load
        and be sticky.
        Local variables, most CPU registers and RAM satisfy these criteria.
        """

    @abstractmethod
    def can_load_have_side_effect(self) -> bool:
        """
        Returns True if reading from this storage might have an effect
        other than fetching the value. For example reading a peripheral's
        status register might reset a flag.
        """

    @abstractmethod
    def can_store_have_side_effect(self) -> bool:
        """
        Returns True if writing to this storage might have an effect
        other than setting the value. For example writing a peripheral's
        control register might change its output.
        """

    @abstractmethod
    def is_load_consistent(self) -> bool:
        """
        Returns True if reading this storage twice in succession will
        return the same value both times.
        """

    @abstractmethod
    def is_sticky(self) -> bool:
        """
        Returns True if reading this storage after it is written will
        return the written value.
        """

    @abstractmethod
    def might_be_same(self, other: Storage) -> bool:
        """
        Returns True if the given storage might be the same storage as
        this one: if it is either certainly the same or if it might be an
        alias.
        """

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

    __slots__ = ("name",)

    def __init__(self, name: str, width: Width):
        self.name: Final[str] = name
        Storage.__init__(self, width)

    @override
    def __str__(self) -> str:
        return f"reg{self.width} {self.name}"

    @override
    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self.name}, {self.width})"

    @override
    def can_load_have_side_effect(self) -> bool:
        return False

    @override
    def can_store_have_side_effect(self) -> bool:
        return False

    @override
    def is_load_consistent(self) -> bool:
        return True

    @override
    def is_sticky(self) -> bool:
        return True

    @override
    def might_be_same(self, other: Storage) -> bool:
        # Register might be passed by reference.
        return self is other or isinstance(other, ArgStorage)


class Variable(Storage):
    """
    A local variable in which intermediate results can be stored.
    Variables don't persist into the final code graph.
    """

    __slots__ = ("name",)

    def __init__(self, name: str, width: Width):
        self.name: Final[str] = name
        Storage.__init__(self, width)

    @override
    def __str__(self) -> str:
        return f"var{self.width} {self.name}"

    @override
    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self.name!r}, {self.width!r})"

    @override
    def can_load_have_side_effect(self) -> bool:
        return False

    @override
    def can_store_have_side_effect(self) -> bool:
        return False

    @override
    def is_load_consistent(self) -> bool:
        return True

    @override
    def is_sticky(self) -> bool:
        return True

    @override
    def might_be_same(self, other: Storage) -> bool:
        return self is other


class ArgStorage(Storage):
    """
    A placeholder storage location for a storage passed to a function.
    The storage properties depend on which concrete storage will be passed,
    so until we know the concrete storage we have to assume the worst case.
    """

    __slots__ = ("name",)

    def __init__(self, name: str, width: Width):
        self.name: Final[str] = name
        Storage.__init__(self, width)

    @override
    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self.name!r}, {self.width})"

    @override
    def __str__(self) -> str:
        return self.name

    @override
    def can_load_have_side_effect(self) -> bool:
        return True

    @override
    def can_store_have_side_effect(self) -> bool:
        return True

    @override
    def is_load_consistent(self) -> bool:
        return False

    @override
    def is_sticky(self) -> bool:
        return False

    @override
    def might_be_same(self, other: Storage) -> bool:
        # A caller's local variables can be passed via reference arguments,
        # but those are separate from the callee's local variables.
        return not isinstance(other, Variable)


class IOStorage(Storage):
    """Storage location accessed via an I/O channel at a particular index."""

    __slots__ = ("channel", "index")

    def __init__(self, channel: IOChannel, index: Expression):
        self.channel: Final[IOChannel] = channel
        self.index: Final[Expression] = index
        Storage.__init__(self, channel.elem_type.width)

    @override
    def __repr__(self) -> str:
        return f"IOStorage({self.channel!r}, {self.index!r})"

    @override
    def __str__(self) -> str:
        return f"{self.channel.name}[{self.index}]"

    @override
    def __eq__(self, other: object) -> bool:
        return (
            isinstance(other, IOStorage)
            and self.channel is other.channel
            and self.index == other.index
        )

    @override
    def __hash__(self) -> int:
        return hash((self.channel, self.index))

    @override
    def can_load_have_side_effect(self) -> bool:
        return self.channel.can_load_have_side_effect(self.index)

    @override
    def can_store_have_side_effect(self) -> bool:
        return self.channel.can_store_have_side_effect(self.index)

    @override
    def is_load_consistent(self) -> bool:
        return self.channel.is_load_consistent(self.index)

    @override
    def is_sticky(self) -> bool:
        return self.channel.is_sticky(self.index)

    @override
    def might_be_same(self, other: Storage) -> bool:
        if isinstance(other, IOStorage):
            # TODO: This is an oversimplification: some MSX devices have their
            #       registers both I/O-mapped and memory-mapped.
            return self.channel == other.channel and self.channel.might_be_same(
                self.index, other.index
            )
        else:
            return isinstance(other, ArgStorage)

    @override
    def iter_expressions(self) -> Iterator[Expression]:
        yield self.index

    @override
    def substitute_expressions(
        self, func: Callable[[Expression], Expression | None]
    ) -> IOStorage:
        index = self.index
        new_index = index.substitute(func)
        if new_index is index:
            return self
        else:
            return IOStorage(self.channel, new_index)


class Keeper(Storage):
    """
    Storage location used to artificially force a load or store
    to not be optimized out.
    """

    __slots__ = ()

    @override
    def __repr__(self) -> str:
        return f"Keeper({self.width})"

    @override
    def __str__(self) -> str:
        return f"keep{self.width}"

    @override
    def can_load_have_side_effect(self) -> bool:
        return True

    @override
    def can_store_have_side_effect(self) -> bool:
        return True

    @override
    def is_load_consistent(self) -> bool:
        return False

    @override
    def is_sticky(self) -> bool:
        return False

    @override
    def might_be_same(self, other: Storage) -> bool:
        return self is other
