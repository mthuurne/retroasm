from .expression import Expression
from .types import IntType

from inspect import signature
import re

class IOChannel:
    '''A channel through which a CPU can do input and output.
    '''
    __slots__ = ('_name', '_elemType', '_addrType')

    name = property(lambda self: self._name)
    elemType = property(lambda self: self._elemType)
    addrType = property(lambda self: self._addrType)

    @staticmethod
    def checkInstance(channel):
        if not isinstance(channel, IOChannel):
            raise TypeError(
                'expected IOChannel subclass, got %s' % type(channel)
                )
        return channel

    def __init__(self, name, elemType, addrType):
        if not isinstance(name, str):
            raise TypeError('name must be string, got %s' % type(name))
        if not isinstance(elemType, IntType):
            raise TypeError(
                'element type must be IntType, got %s' % type(elemType))
        if not isinstance(addrType, IntType):
            raise TypeError(
                'address type must be IntType, got %s' % type(addrType))
        self._name = name
        self._elemType = elemType
        self._addrType = addrType

    def __repr__(self):
        return 'IOChannel(%s, %s, %s)' % (
            repr(self._name), repr(self._elemType), repr(self._addrType)
            )

    def __str__(self):
        return '%s %s[%s]' % (self._elemType, self._name, self._addrType)

    # TODO: Allow the system model to provide a more accurate responses
    #       by examining the index.

    # pylint: disable=unused-argument

    def canLoadHaveSideEffect(self, index):
        '''Returns True if reading from this channel at the given index
        might have an effect other than fetching the value. For example
        reading a peripheral's status register might reset a flag.
        The index is an Expression which might provide some additional
        information about which part of the channel is being read.
        '''
        return True

    def canStoreHaveSideEffect(self, index):
        '''Returns True if writing to this channel at the given index
        might have an effect other than setting the value. For example
        writing a peripheral's control register might change its output.
        The index is an Expression which might provide some additional
        information about which part of the channel is being written.
        '''
        return True

    def isLoadConsistent(self, index):
        '''Returns True if reading from this channel at the given index
        twice in succession will return the same value both times.
        The index is an Expression which might provide some additional
        information about which part of the channel is being read.
        '''
        return False

    def isSticky(self, index):
        '''Returns True if reading from this channel at the give index after
        it is written at that same index will return the written value.
        If access at another index inbetween the write and read can change
        the value, the given index is not considered sticky (return False).
        The index is an Expression which might provide some additional
        information about which part of the channel is being accessed.
        '''
        return False

    def mightBeSame(self, index1, index2):
        '''Returns True if the storages at the two given indices might be the
        same, either because the indices might be equal or because multiple
        indices can point to the same storage.
        '''
        return True

namePat = r"[A-Za-z_][A-Za-z0-9_]*'?"
reName = re.compile(namePat + '$')

class NamedValue(Expression):
    '''Base class for named values that exist in a global or local context.
    '''
    __slots__ = ('_name',)

    name = property(lambda self: self._name)

    def __init__(self, name, typ):
        if not isinstance(name, str):
            raise TypeError('name must be string, got %s' % type(name))
        if not reName.match(name):
            raise ValueError('invalid name: "%s"', name)
        Expression.__init__(self, typ)
        self._name = name

    def _ctorargs(self, *exprs, **kwargs):
        cls = self.__class__
        if exprs:
            raise ValueError('%s does not take expression args' % cls.__name__)
        kwargs.setdefault('name', self._name)
        kwargs.setdefault('typ', self._type)
        return signature(cls).bind(**kwargs)

    def __str__(self):
        return self._name

    def formatDecl(self):
        return '%s %s' % (self._type, self._name)

    def _equals(self, other):
        # There must be one only instance of a class for each name.
        if self._name == other._name: # pylint: disable=protected-access
            assert self is other
            return True
        else:
            return False

    def _complexity(self):
        return 2

class Storage:
    '''A location in which a typed value can be stored.
    '''
    __slots__ = ()

    def canLoadHaveSideEffect(self):
        '''Returns True if reading from this storage might have an effect
        other than fetching the value. For example reading a peripheral's
        status register might reset a flag.
        '''
        raise NotImplementedError

    def canStoreHaveSideEffect(self):
        '''Returns True if writing to this storage might have an effect
        other than setting the value. For example writing a peripheral's
        control register might change its output.
        '''
        raise NotImplementedError

    def isLoadConsistent(self):
        '''Returns True if reading this storage twice in succession will
        return the same value both times.
        '''
        raise NotImplementedError

    def isSticky(self):
        '''Returns True if reading this storage after it is written will
        return the written value.
        '''
        raise NotImplementedError

    def mightBeSame(self, other):
        '''Returns True if the given storage might be the same storage as
        this one: if it is either certainly the same or if it might be an
        alias.
        '''
        raise NotImplementedError

def checkStorage(storage):
    '''Returns True if the given expression is a storage or a concatenation
    of storages, False otherwise.
    '''
    return isinstance(storage, (Concatenation, ReferencedValue, Storage))

def decomposeConcat(storage):
    '''Iterates through the storage locations inside a concatenation.
    Each element is a pair of a Storage and an offset.
    '''
    if isinstance(storage, FixedValue):
        pass
    elif isinstance(storage, (IOReference, ReferencedValue)):
        yield storage, 0
    elif isinstance(storage, Concatenation):
        concatOffset = 0
        for concatTerm in reversed(storage.exprs):
            for subStorage, offset in decomposeConcat(concatTerm):
                yield subStorage, concatOffset + offset
            concatOffset += concatTerm.width
    else:
        assert False, storage

class Variable(NamedValue, Storage):
    '''A variable in the local context.
    '''
    __slots__ = ()

    def canLoadHaveSideEffect(self):
        return False

    def canStoreHaveSideEffect(self):
        return False

    def isLoadConsistent(self):
        return True

    def isSticky(self):
        return True

    def mightBeSame(self, other):
        return self is other

class LocalReference(NamedValue, Storage):
    '''A reference in the local context to a storage location.
    The storage properties depend on which concrete storage will be bound
    to this reference, so we have to assume the worst case.
    '''
    __slots__ = ()

    def canLoadHaveSideEffect(self):
        return True

    def canStoreHaveSideEffect(self):
        return True

    def isLoadConsistent(self):
        return False

    def isSticky(self):
        return False

    def mightBeSame(self, other):
        # A variable has a limited scope, so references passed from outside
        # that scope cannot possibly alias it, while inside the scope there
        # is no need to create aliases.
        return not isinstance(other, Variable)

    def formatDecl(self):
        return '%s& %s' % (self._type, self._name)

class Register(NamedValue, Storage):
    '''A CPU register.
    '''
    __slots__ = ()

    def canLoadHaveSideEffect(self):
        return False

    def canStoreHaveSideEffect(self):
        return False

    def isLoadConsistent(self):
        return True

    def isSticky(self):
        return True

    def mightBeSame(self, other):
        return self is other or isinstance(other, LocalReference)

class IOReference(Expression, Storage):
    '''Reference to a particular index on an I/O channel.
    '''
    __slots__ = ('_channel', '_index')

    channel = property(lambda self: self._channel)
    index = property(lambda self: self._index)

    # pylint: disable=protected-access

    def __init__(self, channel, index):
        self._channel = IOChannel.checkInstance(channel)
        self._index = Expression.checkScalar(index)
        Expression.__init__(self, self._channel.elemType)

    def canLoadHaveSideEffect(self):
        return self._channel.canLoadHaveSideEffect(self._index)

    def canStoreHaveSideEffect(self):
        return self._channel.canStoreHaveSideEffect(self._index)

    def isLoadConsistent(self):
        return self._channel.isLoadConsistent(self._index)

    def isSticky(self):
        return self._channel.isSticky(self._index)

    def mightBeSame(self, other):
        if isinstance(other, IOReference):
            return self._channel == other._channel \
                and self._channel.mightBeSame(self._index, other._index)
        else:
            return isinstance(other, LocalReference)

    def _ctorargs(self, *exprs, **kwargs):
        cls = self.__class__
        if exprs:
            raise ValueError('%s does not take expression args' % cls.__name__)
        kwargs.setdefault('channel', self._channel)
        kwargs.setdefault('index', self._index)
        return signature(cls).bind(**kwargs)

    def __str__(self):
        return '%s[%s]' % (self._channel.name, self._index)

    def _equals(self, other):
        return self._channel is other._channel and self._index == other._index

    def _complexity(self):
        return 4 + self._index._complexity()

class FixedValue(Storage):
    '''A storage that always reads as the same value and ignores writes.
    '''
    __slots__ = ('_cid', '_type')

    cid = property(lambda self: self._cid)
    type = property(lambda self: self._type)
    width = property(lambda self: self._type.width)

    def __init__(self, cid, typ):
        self._cid = cid
        self._type = typ

    def canLoadHaveSideEffect(self):
        return False

    def canStoreHaveSideEffect(self):
        return False

    def isLoadConsistent(self):
        return True

    def isSticky(self):
        return False

    def mightBeSame(self, other):
        # Since we don't store any state, we can pretend to be unique.
        return self is other

class ReferencedValue(Expression):
    '''A value in a storage location accessed through a reference.
    '''
    __slots__ = ('_rid',)

    rid = property(lambda self: self._rid)

    def __init__(self, rid, typ):
        Expression.__init__(self, typ)
        self._rid = rid

    def _ctorargs(self, *exprs, **kwargs):
        cls = self.__class__
        if exprs:
            raise ValueError('%s does not take expression args' % cls.__name__)
        kwargs.setdefault('rid', self._rid)
        kwargs.setdefault('typ', self.type)
        return signature(cls).bind(**kwargs)

    def __str__(self):
        return 'R%d' % self._rid

    def _equals(self, other):
        # pylint: disable=protected-access
        return self._rid is other._rid

    def _complexity(self):
        return 4

class Concatenation:
    '''Concatenates the bit strings of storages.
    '''
    __slots__ = ('_exprs',)

    exprs = property(lambda self: self._exprs)
    width = property(lambda self: sum(expr.width for expr in self._exprs))
    type = property(lambda self: IntType(self.width))

    def __init__(self, exprs):
        self._exprs = exprs = tuple(exprs)
        for expr in exprs:
            if isinstance(expr, Concatenation):
                raise TypeError('nested concatenations not allowed')
            if not checkStorage(expr):
                raise TypeError('expected storage, got %s' % type(expr))
