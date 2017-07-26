from .expression import (
    AddOperator, AndOperator, Expression, IntLiteral, LShift, LVShift,
    OrOperator, RVShift, SignExtension, XorOperator, optSlice, truncate
    )
from .expression_simplifier import simplifyExpression
from .storage import Storage
from .types import IntType, maskForWidth, unlimited
from .utils import checkType

class BitString:
    '''Abstract base class for bit strings.
    '''
    __slots__ = ('_width',)

    width = property(lambda self: self._width)

    def __init__(self, width):
        self._width = checkType(width, (int, type(unlimited)), 'width')

    def iterExpressions(self):
        '''Iterates through the expressions contained in this reference.
        '''
        raise NotImplementedError

    def iterStorages(self):
        '''Iterates through the storages accessed through this reference.
        '''
        raise NotImplementedError

    def substitute(self, storageFunc=None, expressionFunc=None):
        '''Applies the given substitution functions to each applicable
        subreference of this reference and returns the resulting reference.
        The storage function passed a storage as its argument and must return
        None if no substitution is to take place and a reference to the
        replacement otherwise.
        If no storage substitution took place, the expression function is
        applied. This function should follow the same contract as the function
        passed to Expression.substitute().
        '''
        raise NotImplementedError

    def emitLoad(self, builder, location):
        '''Emits load nodes for loading a bit string from the referenced
        storage(s).
        Returns the value of the bit string as an Expression.
        '''
        raise NotImplementedError

    def emitStore(self, builder, value, location):
        '''Emits store nodes for storing a bit string into the referenced
        storage(s).
        '''
        raise NotImplementedError

class FixedValue(BitString):
    '''A bit string that always reads as the same value and ignores writes.
    '''
    __slots__ = ('_expr',)

    expr = property(lambda self: self._expr)

    def __init__(self, expr, width):
        BitString.__init__(self, width)
        self._expr = Expression.checkScalar(expr)

    def __repr__(self):
        return 'FixedValue(%r, %s)' % (self._expr, self._width)

    def __str__(self):
        return str(self._expr)

    def iterExpressions(self):
        yield self._expr

    def iterStorages(self):
        return iter(())

    def substitute(self, storageFunc=None, expressionFunc=None):
        if expressionFunc is None:
            return self
        expr = self._expr
        newExpr = expr.substitute(expressionFunc)
        if newExpr is expr:
            return self
        else:
            return FixedValue(newExpr, self._width)

    def emitLoad(self, builder, location):
        return self._expr

    def emitStore(self, builder, value, location):
        pass

class SingleStorage(BitString):
    __slots__ = ('_storage',)

    storage = property(lambda self: self._storage)

    def __init__(self, storage):
        self._storage = checkType(storage, Storage, 'storage')
        BitString.__init__(self, storage.width)

    def __repr__(self):
        return 'SingleStorage(%r)' % self._storage

    def __str__(self):
        return str(self._storage)

    def iterExpressions(self):
        return self._storage.iterExpressions()

    def iterStorages(self):
        yield self._storage

    def substitute(self, storageFunc=None, expressionFunc=None):
        storage = self._storage
        if storageFunc is not None:
            newRef = storageFunc(storage)
            if newRef is not None:
                if isinstance(newRef, SingleStorage) and \
                        newRef._storage is storage:
                    return self
                else:
                    return newRef

        if expressionFunc is None:
            return self
        newStorage = storage.substituteExpressions(expressionFunc)
        if newStorage is storage:
            return self
        else:
            return SingleStorage(newStorage)

    def emitLoad(self, builder, location):
        return builder.emitLoadBits(self._storage, location)

    def emitStore(self, builder, value, location):
        builder.emitStoreBits(self._storage, value, location)

class ConcatenatedBits(BitString):
    '''A concatenation of a bit strings.
    '''
    __slots__ = ('_subs',)

    def __init__(self, *subs):
        '''Creates a concatenation of the given bit strings, in order from least
        to most significant.
        '''
        width = 0
        for sub in subs:
            if width is unlimited:
                raise ValueError(
                    'unlimited width is only allowed on most significant '
                    'bit string'
                    )
            checkType(sub, BitString, 'bit string')
            width += sub.width
        BitString.__init__(self, width)
        self._subs = subs

    def __repr__(self):
        return 'ConcatenatedBits(%s)' % ', '.join(
            repr(sub) for sub in self._subs
            )

    def __str__(self):
        return '(%s)' % ' ; '.join(str(sub) for sub in reversed(self._subs))

    def __iter__(self):
        return iter(self._subs)

    def iterExpressions(self):
        for sub in self._subs:
            yield from sub.iterExpressions()

    def iterStorages(self):
        for sub in self._subs:
            yield from sub.iterStorages()

    def substitute(self, storageFunc=None, expressionFunc=None):
        changed = False
        subs = []
        for sub in self._subs:
            newBits = sub.substitute(storageFunc, expressionFunc)
            subs.append(newBits)
            changed |= newBits is not sub
        return ConcatenatedBits(*subs) if changed else self

    def emitLoad(self, builder, location):
        terms = []
        offset = 0
        for sub in self._subs:
            value = sub.emitLoad(builder, location)
            terms.append(value if offset == 0 else LShift(value, offset))
            offset += sub.width
        return OrOperator(*terms)

    def emitStore(self, builder, value, location):
        offset = 0
        for sub in self._subs:
            width = sub.width
            valueSlice = optSlice(value, offset, width)
            sub.emitStore(builder, valueSlice, location)
            offset += width

class SlicedBits(BitString):
    '''A slice of a bit string.
    '''
    __slots__ = ('_bits', '_offset')

    bits = property(lambda self: self._bits)
    offset = property(lambda self: self._offset)

    def __init__(self, bits, offset, width):
        '''Creates a slice of the given bit string.
        '''
        self._bits = checkType(bits, BitString, 'bit string')

        offset = simplifyExpression(Expression.checkScalar(offset))
        # Some invalid offsets can only be detected upon use, but others we
        # can detect on definition and rejecting them early is likely helpful
        # towards the user.
        if isinstance(offset, IntLiteral) and offset.value < 0:
            raise ValueError('slice offset must not be negative')
        self._offset = offset

        BitString.__init__(self, width)

    def __repr__(self):
        return 'SlicedBits(%r, %r, %s)' % (
            self._bits, self._offset, self._width
            )

    def __str__(self):
        offset = self._offset
        width = self._width
        if isinstance(offset, IntLiteral):
            offsetVal = offset.value
            return '%s[%s:%s]' % (
                self._bits,
                '' if offsetVal == 0 else offsetVal,
                '' if width is unlimited else offsetVal + width
                )
        else:
            if width is unlimited:
                end = ''
            else:
                end = AddOperator(offset, IntLiteral(width))
            return '%s[%s:%s]' % (self._bits, offset, end)

    def iterExpressions(self):
        return self._bits.iterExpressions()

    def iterStorages(self):
        return self._bits.iterStorages()

    def substitute(self, storageFunc=None, expressionFunc=None):
        bits = self._bits
        newBits = bits.substitute(storageFunc, expressionFunc)
        if newBits is bits:
            return self
        else:
            return SlicedBits(newBits, self._offset, self._width)

    def emitLoad(self, builder, location):
        # Load value from our bit string.
        value = self._bits.emitLoad(builder, location)

        # Slice the loaded value.
        return truncate(RVShift(value, self._offset), self.width)

    def emitStore(self, builder, value, location):
        offset = self._offset
        width = self.width
        valueMask = LVShift(IntLiteral(maskForWidth(width)), offset)

        # Get mask and previous value of our bit string.
        bits = self._bits
        fullMask = IntLiteral(maskForWidth(bits.width))
        prevValue = bits.emitLoad(builder, location)

        # Combine previous value with new value.
        maskLit = AndOperator(fullMask, XorOperator(IntLiteral(-1), valueMask))
        combined = OrOperator(
            AndOperator(prevValue, maskLit),
            LVShift(value, offset)
            )

        combined = simplifyExpression(combined)
        bits.emitStore(builder, combined, location)

class Reference:
    '''Typed access to a bit string.
    '''
    __slots__ = ('_bits', '_type')

    bits = property(lambda self: self._bits)
    type = property(lambda self: self._type)
    width = property(lambda self: self._type.width)

    def __init__(self, bits, typ):
        self._bits = checkType(bits, BitString, 'bit string')
        self._type = checkType(typ, IntType, 'value type')
        if bits.width != typ.width:
            raise ValueError(
                'bit string of %s bits used for reference of type %s'
                % (bits.width, typ)
                )

    def emitLoad(self, builder, location):
        '''Emits load nodes for loading a typed value from the referenced
        bit string.
        Returns the loaded value as an Expression.
        '''
        value = self._bits.emitLoad(builder, location)

        # Apply sign extension, if necessary.
        typ = self._type
        if typ.signed:
            width = typ.width
            if width is not unlimited:
                return SignExtension(value, width)
        return value

    def emitStore(self, builder, value, location):
        '''Emits store nodes for storing a value into the referenced bit string.
        '''
        self._bits.emitStore(builder, truncate(value, self.width), location)
