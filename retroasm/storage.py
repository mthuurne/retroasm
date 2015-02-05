from .expression import Expression
from .types import IntType
from .utils import checkType

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
                'expected IOChannel subclass, got %s' % type(channel).__name__
                )
        return channel

    def __init__(self, name, elemType, addrType):
        self._name = checkType(name, str, 'channel name')
        self._elemType = checkType(elemType, IntType, 'element type')
        self._addrType = checkType(addrType, IntType, 'address type')

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

class Storage:
    '''A location in which a typed value can be stored.
    '''
    __slots__ = ('_type',)

    type = property(lambda self: self._type)
    width = property(lambda self: self._type.width)

    def __init__(self, typ):
        self._type = checkType(typ, IntType, 'storage type')

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

class NamedStorage(Storage):
    '''Base class for named storages that exist in a global or local context.
    '''
    __slots__ = ('_name',)

    name = property(lambda self: self._name)
    decl = property(lambda self: '%s %s' % (self._type, self._name))

    def __init__(self, name, typ):
        self._name = checkType(name, str, 'storage name')
        if not reName.match(name):
            raise ValueError('invalid name: "%s"', name)
        Storage.__init__(self, typ)

    def __repr__(self):
        return '%s(%s, %s)' % (
            self.__class__.__name__, repr(self._name), repr(self._type)
            )

    def __str__(self):
        return self._name

    def canLoadHaveSideEffect(self):
        raise NotImplementedError

    def canStoreHaveSideEffect(self):
        raise NotImplementedError

    def isLoadConsistent(self):
        raise NotImplementedError

    def isSticky(self):
        raise NotImplementedError

    def mightBeSame(self, other):
        raise NotImplementedError

def isStorage(storage):
    '''Returns True if the given expression is a storage or a concatenation
    of storages, False otherwise.
    '''
    return isinstance(storage, (Concatenation, ReferencedValue, Slice, Storage))

def _decomposeStorage(storage):
    '''Iterates through the basic storages inside the given composed storage.
    Each element is a triple of a reference ID and the start index and the width
    of the slice of the storage that is affected.
    '''
    if isinstance(storage, Concatenation):
        for concatTerm in reversed(storage.exprs):
            yield from _decomposeStorage(concatTerm)
    elif isinstance(storage, Slice):
        sliceStart = storage.index
        sliceEnd = sliceStart + storage.width
        offset = 0
        for rid, subStart, subWidth in _decomposeStorage(storage.expr):
            # Clip slice indices to substorage range.
            start = max(sliceStart, offset)
            end = min(sliceEnd, offset + subWidth)
            if start < end:
                yield rid, subStart + start - offset, end - start
            offset += subWidth
    else:
        assert isinstance(storage, ReferencedValue), repr(storage)
        yield storage.rid, 0, storage.width

class ComposedStorage:
    __slots__ = ('_decomposed',)

    @classmethod
    def decompose(cls, storage):
        return cls(_decomposeStorage(storage))

    def __init__(self, decomposed):
        self._decomposed = tuple(decomposed)

    def __iter__(self):
        return iter(self._decomposed)

class Variable(NamedStorage):
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

class LocalReference(NamedStorage):
    '''A reference in the local context to a storage location.
    The storage properties depend on which concrete storage will be bound
    to this reference, so we have to assume the worst case.
    '''
    __slots__ = ()

    decl = property(lambda self: '%s& %s' % (self._type, self._name))

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

class Register(NamedStorage):
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

class IOReference(Storage):
    '''Reference to a particular index on an I/O channel.
    '''
    __slots__ = ('_channel', '_index')

    channel = property(lambda self: self._channel)
    index = property(lambda self: self._index)

    def __init__(self, channel, index):
        self._channel = IOChannel.checkInstance(channel)
        self._index = Expression.checkScalar(index)
        Storage.__init__(self, channel.elemType)

    def __repr__(self):
        return 'IOReference(%s, %s)' % (repr(self._channel), repr(self._index))

    def __str__(self):
        return '%s[%s]' % (self._channel.name, self._index)

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
            # pylint: disable=protected-access
            return self._channel == other._channel \
                and self._channel.mightBeSame(self._index, other._index)
        else:
            return isinstance(other, LocalReference)

class FixedValue(Storage):
    '''A storage that always reads as the same value and ignores writes.
    '''
    __slots__ = ('_cid',)

    cid = property(lambda self: self._cid)

    def __init__(self, cid, typ):
        self._cid = cid
        Storage.__init__(self, typ)

    def __repr__(self):
        return 'FixedValue(%d, %s)' % (self._cid, repr(self._type))

    def __str__(self):
        return 'C%d' % self._cid

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

class ReferencedValue:
    '''A value in a storage location accessed through a reference.
    '''
    __slots__ = ('_rid', '_type')

    rid = property(lambda self: self._rid)
    type = property(lambda self: self._type)
    width = property(lambda self: self._type.width)

    def __init__(self, rid, typ):
        self._rid = rid
        self._type = typ

    def __repr__(self):
        return 'ReferencedValue(%d, %s)' % (self._rid, repr(self._type))

    def __str__(self):
        return 'R%d' % self._rid

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
            if not isStorage(expr):
                raise TypeError(
                    'expected storage, got %s' % type(expr).__name__
                    )

    def __repr__(self):
        return 'Concatenation(%s)' % ', '.join(
            repr(expr) for expr in self._exprs
            )

    def __str__(self):
        return '(%s)' % ' ; '.join(str(expr) for expr in self._exprs)

class Slice:
    '''Slices the bit strings of a storage.
    '''
    __slots__ = ('_expr', '_index', '_width')

    expr = property(lambda self: self._expr)
    index = property(lambda self: self._index)
    width = property(lambda self: self._width)
    type = property(lambda self: IntType(self.width))

    def __init__(self, expr, index, width):
        self._expr = expr
        self._index = checkType(index, int, 'slice index')
        self._width = checkType(width, int, 'slice width')
        if index < 0:
            raise ValueError('slice index must not be negative: %d' % index)
        if width < 0:
            raise ValueError('slice width must not be negative: %d' % width)

    def __repr__(self):
        return 'Slice(%s, %d, %d)' % (
            repr(self._expr), self._index, self._width
            )

    def __str__(self):
        return '%s[%d:%d]' % (
            self._expr, self._index, self._index + self._width
            )
