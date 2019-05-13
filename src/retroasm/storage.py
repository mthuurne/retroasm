from __future__ import annotations

from typing import Callable, Iterator, Optional, Union, cast

from .expression import Expression
from .types import IntType, Unlimited, unlimited
from .utils import checkType


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
                'expected IOChannel subclass, got %s' % type(channel).__name__
                )
        return channel

    def __init__(self, name: str, elemType: IntType, addrType: IntType):
        self._name = checkType(name, str, 'channel name')
        self._elemType = checkType(elemType, IntType, 'element type')
        self._addrType = checkType(addrType, IntType, 'address type')

    def __repr__(self) -> str:
        return 'IOChannel(%r, %r, %r)' % (
            self._name, self._elemType, self._addrType
            )

    def __str__(self) -> str:
        return '%s %s[%s]' % (self._elemType, self._name, self._addrType)

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
    def width(self) -> Union[int, Unlimited]:
        return self._width

    def __init__(self, width: Union[int, Unlimited]):
        self._width = checkType(width, (int, type(unlimited)), 'storage width')
        if width < 0:
            raise ValueError(
                'storage width must not be negative: %d' % cast(int, width)
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

    def __init__(self, width: Union[int, Unlimited], scope: int):
        Storage.__init__(self, width)
        self._scope = checkType(scope, int, 'scope level')

    def __repr__(self) -> str:
        return 'Variable(%s, %d)' % (self._width, self._scope)

    def __str__(self) -> str:
        return 'var%s@%x' % (self._width, id(self))

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
            self._scope == 0 and isinstance(other, RefArgStorage)
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

    def __init__(self, name: str, width: Union[int, Unlimited]):
        self._name = checkType(name, str, 'storage name')
        Storage.__init__(self, width)

    def __repr__(self) -> str:
        return '%s(%r, %s)' % (self.__class__.__name__, self._name, self._width)

    def __str__(self) -> str:
        return self._name

    def canLoadHaveSideEffect(self) -> bool:
        raise NotImplementedError

    def canStoreHaveSideEffect(self) -> bool:
        raise NotImplementedError

    def isLoadConsistent(self) -> bool:
        raise NotImplementedError

    def isSticky(self) -> bool:
        raise NotImplementedError

    def mightBeSame(self, other: Storage) -> bool:
        raise NotImplementedError

class RefArgStorage(ArgStorage):
    '''A placeholder storage location for a storage passed to a function by
    reference.
    '''
    __slots__ = ()

    def canLoadHaveSideEffect(self) -> bool:
        return True

    def canStoreHaveSideEffect(self) -> bool:
        return True

    def isLoadConsistent(self) -> bool:
        return False

    def isSticky(self) -> bool:
        return False

    def mightBeSame(self, other: Storage) -> bool:
        # A variable can only be referenced via arguments if it exists in
        # the global scope.
        return not isinstance(other, Variable) or other._scope == 0

class ValArgStorage(ArgStorage):
    '''A placeholder storage location for a storage passed to a function by
    value.
    '''
    __slots__ = ()

    def canLoadHaveSideEffect(self) -> bool:
        return False

    def canStoreHaveSideEffect(self) -> bool:
        return False

    def isLoadConsistent(self) -> bool:
        return True

    def isSticky(self) -> bool:
        return False

    def mightBeSame(self, other: Storage) -> bool:
        return False

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
        self._index = checkType(index, Expression, 'index')
        Storage.__init__(self, channel.elemType.width)

    def __repr__(self) -> str:
        return 'IOStorage(%r, %r)' % (self._channel, self._index)

    def __str__(self) -> str:
        return '%s[%s]' % (self._channel.name, self._index)

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
            return isinstance(other, RefArgStorage)

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
