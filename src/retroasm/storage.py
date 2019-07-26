from __future__ import annotations

from typing import Callable, Iterator, Optional, cast

from .expression import Expression
from .types import IntType, Width, unlimited


class IOChannel:
    '''A channel through which a CPU can do input and output.
    '''
    __slots__ = ('_name', '_elemType', '_addrType')

    @property
    def name(self) -> str:
        return self._name

    @property
    def elemType(self) -> IntType:
        return self._elemType

    @property
    def addrType(self) -> IntType:
        return self._addrType

    @staticmethod
    def checkInstance(channel: IOChannel) -> IOChannel:
        if not isinstance(channel, IOChannel):
            raise TypeError(
                f'expected IOChannel subclass, got {type(channel).__name__}'
                )
        return channel

    def __init__(self, name: str, elemType: IntType, addrType: IntType):
        self._name = name
        self._elemType = elemType
        self._addrType = addrType

    def __repr__(self) -> str:
        return f'IOChannel({self._name!r}, {self._elemType!r}, ' \
                         f'{self._addrType!r})'

    def __str__(self) -> str:
        return f'{self._elemType} {self._name}[{self._addrType}]'

    # TODO: Allow the system model to provide a more accurate responses
    #       by examining the index.

    # pylint: disable=unused-argument

    def canLoadHaveSideEffect(self, index: Expression) -> bool:
        '''Returns True if reading from this channel at the given index
        might have an effect other than fetching the value. For example
        reading a peripheral's status register might reset a flag.
        The index is an Expression which might provide some additional
        information about which part of the channel is being read.
        '''
        return True

    def canStoreHaveSideEffect(self, index: Expression) -> bool:
        '''Returns True if writing to this channel at the given index
        might have an effect other than setting the value. For example
        writing a peripheral's control register might change its output.
        The index is an Expression which might provide some additional
        information about which part of the channel is being written.
        '''
        return True

    def isLoadConsistent(self, index: Expression) -> bool:
        '''Returns True if reading from this channel at the given index
        twice in succession will return the same value both times.
        The index is an Expression which might provide some additional
        information about which part of the channel is being read.
        '''
        return False

    def isSticky(self, index: Expression) -> bool:
        '''Returns True if reading from this channel at the give index after
        it is written at that same index will return the written value.
        If access at another index inbetween the write and read can change
        the value, the given index is not considered sticky (return False).
        The index is an Expression which might provide some additional
        information about which part of the channel is being accessed.
        '''
        return False

    def mightBeSame(self, index1: Expression, index2: Expression) -> bool:
        '''Returns True if the storages at the two given indices might be the
        same, either because the indices might be equal or because multiple
        indices can point to the same storage.
        '''
        return True

class Storage:
    '''A location in which a typed value can be stored.
    '''
    __slots__ = ('_width',)

    @property
    def width(self) -> Width:
        return self._width

    def __init__(self, width: Width):
        self._width = width
        if width < 0:
            raise ValueError(
                f'storage width must not be negative: {cast(int, width):d}'
                )

    def canLoadHaveSideEffect(self) -> bool:
        '''Returns True if reading from this storage might have an effect
        other than fetching the value. For example reading a peripheral's
        status register might reset a flag.
        '''
        raise NotImplementedError

    def canStoreHaveSideEffect(self) -> bool:
        '''Returns True if writing to this storage might have an effect
        other than setting the value. For example writing a peripheral's
        control register might change its output.
        '''
        raise NotImplementedError

    def isLoadConsistent(self) -> bool:
        '''Returns True if reading this storage twice in succession will
        return the same value both times.
        '''
        raise NotImplementedError

    def isSticky(self) -> bool:
        '''Returns True if reading this storage after it is written will
        return the written value.
        '''
        raise NotImplementedError

    def mightBeSame(self, other: Storage) -> bool:
        '''Returns True if the given storage might be the same storage as
        this one: if it is either certainly the same or if it might be an
        alias.
        '''
        raise NotImplementedError

    def iterExpressions(self) -> Iterator[Expression]:
        '''Iterates through the expressions in this storage, if any.
        '''
        return iter(())

    def substituteExpressions(
            self,
            func: Callable[[Expression], Optional[Expression]]
            ) -> Storage:
        '''Applies the given substitution function to the expressions in this
        storage, if any.
        See Expression.substitute() for details about the substitution function.
        Returns a new version of storage with its expressions replaced if any
        substitution occurred, or this storage otherwise.
        '''
        return self

class Variable(Storage):
    '''A simple piece of named storage.
    Is used for registers as well as variables.
    '''
    __slots__ = ('_scope',)

    @property
    def scope(self) -> int:
        return self._scope

    def __init__(self, width: Width, scope: int):
        Storage.__init__(self, width)
        self._scope = scope

    def __repr__(self) -> str:
        return f'Variable({self._width}, {self._scope:d})'

    def __str__(self) -> str:
        return f'var{self._width}@{id(self):x}'

    def canLoadHaveSideEffect(self) -> bool:
        return False

    def canStoreHaveSideEffect(self) -> bool:
        return False

    def isLoadConsistent(self) -> bool:
        return True

    def isSticky(self) -> bool:
        return True

    def mightBeSame(self, other: Storage) -> bool:
        return self is other or (
            # Global variable might be passed by reference.
            self._scope == 0 and isinstance(other, ArgStorage)
            )

class ArgStorage(Storage):
    '''A placeholder storage location for a storage passed to a function.
    The storage properties depend on which concrete storage will be passed,
    so until we know the concrete storage we have to assume the worst case.
    '''
    __slots__ = ('_name',)

    @property
    def name(self) -> str:
        return self._name

    def __init__(self, name: str, width: Width):
        self._name = name
        Storage.__init__(self, width)

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}({self._name!r}, {self._width})'

    def __str__(self) -> str:
        return self._name

    def canLoadHaveSideEffect(self) -> bool:
        return True

    def canStoreHaveSideEffect(self) -> bool:
        return True

    def isLoadConsistent(self) -> bool:
        return False

    def isSticky(self) -> bool:
        return False

    def mightBeSame(self, other: Storage) -> bool:
        # We don't have nested function scopes, so a variable that is known
        # in our scope can only be referenced via arguments if it is defined
        # in the global scope.
        return not isinstance(other, Variable) or other._scope == 0

class IOStorage(Storage):
    '''Storage location accessed via an I/O channel at a particular index.
    '''
    __slots__ = ('_channel', '_index')

    @property
    def channel(self) -> IOChannel:
        return self._channel

    @property
    def index(self) -> Expression:
        return self._index

    def __init__(self, channel: IOChannel, index: Expression):
        self._channel = IOChannel.checkInstance(channel)
        self._index = index
        Storage.__init__(self, channel.elemType.width)

    def __repr__(self) -> str:
        return f'IOStorage({self._channel!r}, {self._index!r})'

    def __str__(self) -> str:
        return f'{self._channel.name}[{self._index}]'

    def __eq__(self, other: object) -> bool:
        return ( # pylint: disable=protected-access
            isinstance(other, IOStorage) and
            self._channel is other._channel and
            self._index == other._index
            )

    def __hash__(self) -> int:
        return hash((self._channel, self._index))

    def canLoadHaveSideEffect(self) -> bool:
        return self._channel.canLoadHaveSideEffect(self._index)

    def canStoreHaveSideEffect(self) -> bool:
        return self._channel.canStoreHaveSideEffect(self._index)

    def isLoadConsistent(self) -> bool:
        return self._channel.isLoadConsistent(self._index)

    def isSticky(self) -> bool:
        return self._channel.isSticky(self._index)

    def mightBeSame(self, other: Storage) -> bool:
        if isinstance(other, IOStorage):
            # pylint: disable=protected-access
            return self._channel == other._channel \
                and self._channel.mightBeSame(self._index, other._index)
        else:
            return isinstance(other, ArgStorage)

    def iterExpressions(self) -> Iterator[Expression]:
        yield self._index

    def substituteExpressions(
            self,
            func: Callable[[Expression], Optional[Expression]]
            ) -> IOStorage:
        index = self._index
        newIndex = index.substitute(func)
        if newIndex is index:
            return self
        else:
            return IOStorage(self._channel, newIndex)

class Keeper(Storage):
    '''Storage location used to artificially force a load or store
    to not be optimized out.
    '''
    __slots__ = ()

    def __repr__(self) -> str:
        return f'Keeper({self._width})'

    def __str__(self) -> str:
        return f'keep{self._width}'

    def canLoadHaveSideEffect(self) -> bool:
        return True

    def canStoreHaveSideEffect(self) -> bool:
        return True

    def isLoadConsistent(self) -> bool:
        return False

    def isSticky(self) -> bool:
        return False

    def mightBeSame(self, other: Storage) -> bool:
        return self is other
