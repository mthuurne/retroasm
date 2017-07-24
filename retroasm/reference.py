from .expression import (
    AddOperator, AndOperator, Expression, IntLiteral, LShift, LVShift,
    OrOperator, RVShift, SignExtension, XorOperator, optSlice, truncate
    )
from .expression_simplifier import simplifyExpression
from .storage import Storage
from .types import IntType, maskForWidth, unlimited
from .utils import checkType

class Reference:
    '''Abstract base class for references.
    '''
    __slots__ = ('_type',)

    type = property(lambda self: self._type)
    width = property(lambda self: self._type.width)

    def __init__(self, typ):
        self._type = checkType(typ, IntType, 'value type')

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
        '''Emits load nodes for loading a typed value from the referenced
        storage(s).
        Returns the loaded value as an Expression.
        '''
        value = self._emitLoadBits(builder, location)

        # Apply sign extension, if necessary.
        typ = self._type
        if typ.signed:
            width = typ.width
            if width is not unlimited:
                return SignExtension(value, width)
        return value

    def _emitLoadBits(self, builder, location):
        '''Emits load nodes for loading a bit string from the referenced
        storage(s).
        Returns the value of the bit string as an Expression.
        '''
        raise NotImplementedError

    def emitStore(self, builder, value, location):
        '''Emits store nodes for storing a value into the referenced storage(s).
        '''
        self._emitStoreBits(builder, truncate(value, self.width), location)

    def _emitStoreBits(self, builder, value, location):
        '''Emits store nodes for storing a bit string into the referenced
        storage(s).
        '''
        raise NotImplementedError

class FixedValue(Reference):
    '''A reference that always reads as the same value and ignores writes.
    '''
    __slots__ = ('_expr',)

    expr = property(lambda self: self._expr)

    def __init__(self, expr, typ):
        Reference.__init__(self, typ)
        self._expr = Expression.checkScalar(expr)

    def __repr__(self):
        return 'FixedValue(%r, %r)' % (self._expr, self._type)

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
            return FixedValue(newExpr, self._type)

    def _emitLoadBits(self, builder, location):
        return self._expr

    def _emitStoreBits(self, builder, value, location):
        pass

class SingleReference(Reference):
    __slots__ = ('_storage',)

    storage = property(lambda self: self._storage)

    def __init__(self, storage, typ):
        Reference.__init__(self, typ)
        self._storage = checkType(storage, Storage, 'storage')

    def __repr__(self):
        return 'SingleReference(%r, %r)' % (self._storage, self._type)

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
                if isinstance(newRef, SingleReference) and \
                        newRef._storage is storage and \
                        newRef._type is self._type:
                    return self
                else:
                    return newRef

        if expressionFunc is None:
            return self
        newStorage = storage.substituteExpressions(expressionFunc)
        if newStorage is storage:
            return self
        else:
            return SingleReference(newStorage, self._type)

    def _emitLoadBits(self, builder, location):
        return builder.emitLoadBits(self._storage, location)

    def _emitStoreBits(self, builder, value, location):
        builder.emitStoreBits(self._storage, value, location)

class ConcatenatedReference(Reference):
    __slots__ = ('_refs',)

    def __init__(self, *refs):
        '''Creates a concatenation of the given references, in order from least
        to most significant.
        '''
        width = 0
        for ref in refs:
            if width is unlimited:
                raise ValueError(
                    'unlimited width is only allowed on most significant '
                    'storage'
                    )
            checkType(ref, Reference, 'reference')
            width += ref.width
        typ = IntType(width, width != 0 and refs[-1].type.signed)
        Reference.__init__(self, typ)
        self._refs = refs

    def __repr__(self):
        return 'ConcatenatedReference(%s)' % ', '.join(
            repr(ref) for ref in self._refs
            )

    def __str__(self):
        return '(%s)' % ' ; '.join(str(ref) for ref in reversed(self._refs))

    def __iter__(self):
        return iter(self._refs)

    def iterExpressions(self):
        for ref in self._refs:
            yield from ref.iterExpressions()

    def iterStorages(self):
        for ref in self._refs:
            yield from ref.iterStorages()

    def substitute(self, storageFunc=None, expressionFunc=None):
        changed = False
        refs = []
        for ref in self._refs:
            newRef = ref.substitute(storageFunc, expressionFunc)
            refs.append(newRef)
            changed |= newRef is not ref
        return ConcatenatedReference(*refs) if changed else self

    def _emitLoadBits(self, builder, location):
        terms = []
        offset = 0
        for ref in self._refs:
            value = ref._emitLoadBits(builder, location)
            terms.append(value if offset == 0 else LShift(value, offset))
            offset += ref.width
        return OrOperator(*terms)

    def _emitStoreBits(self, builder, value, location):
        offset = 0
        for ref in self._refs:
            width = ref.width
            valueSlice = optSlice(value, offset, width)
            ref._emitStoreBits(builder, valueSlice, location)
            offset += width

class SlicedReference(Reference):
    __slots__ = ('_ref', '_offset')

    ref = property(lambda self: self._ref)
    offset = property(lambda self: self._offset)

    def __init__(self, ref, offset, width):
        '''Creates a bitwise slice of the given reference.
        '''
        self._ref = checkType(ref, Reference, 'reference')

        offset = simplifyExpression(Expression.checkScalar(offset))
        # Some invalid offsets can only be detected upon use, but others we
        # can detect on definition and rejecting them early is likely helpful
        # towards the user.
        if isinstance(offset, IntLiteral) and offset.value < 0:
            raise ValueError('slice offset must not be negative')
        self._offset = offset

        typ = IntType.int if width is unlimited else IntType.u(width)
        Reference.__init__(self, typ)

    def __repr__(self):
        return 'SlicedReference(%r, %r, %s)' % (
            self._ref, self._offset, self.width
            )

    def __str__(self):
        offset = self._offset
        width = self.width
        if isinstance(offset, IntLiteral):
            offsetVal = offset.value
            return '%s[%s:%s]' % (
                self._ref,
                '' if offsetVal == 0 else offsetVal,
                '' if width is unlimited else offsetVal + width
                )
        else:
            if width is unlimited:
                end = ''
            else:
                end = AddOperator(offset, IntLiteral(width))
            return '%s[%s:%s]' % (self._ref, offset, end)

    def iterExpressions(self):
        return self._ref.iterExpressions()

    def iterStorages(self):
        return self._ref.iterStorages()

    def substitute(self, storageFunc=None, expressionFunc=None):
        ref = self._ref
        newRef = ref.substitute(storageFunc, expressionFunc)
        if newRef is ref:
            return self
        else:
            return SlicedReference(newRef, self._offset, self.width)

    def _emitLoadBits(self, builder, location):
        # Load value from our reference.
        value = self._ref._emitLoadBits(builder, location)

        # Slice the loaded value.
        return truncate(RVShift(value, self._offset), self.width)

    def _emitStoreBits(self, builder, value, location):
        offset = self._offset
        width = self.width
        valueMask = LVShift(IntLiteral(maskForWidth(width)), offset)

        # Get mask and previous value of our reference.
        ref = self._ref
        fullMask = IntLiteral(maskForWidth(ref.width))
        prevValue = ref._emitLoadBits(builder, location)

        # Combine previous value with new value.
        maskLit = AndOperator(fullMask, XorOperator(IntLiteral(-1), valueMask))
        combined = OrOperator(
            AndOperator(prevValue, maskLit),
            LVShift(value, offset)
            )

        combined = simplifyExpression(combined)
        self._ref._emitStoreBits(builder, combined, location)
