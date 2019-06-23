from __future__ import annotations

from typing import (
    TYPE_CHECKING, Callable, Iterator, Optional, Sequence, Union, cast
)

from .expression import (
    AddOperator, AndOperator, BadValue, Expression, IntLiteral, LShift,
    LVShift, OrOperator, RVShift, SignExtension, XorOperator, optSlice,
    truncate
)
from .expression_simplifier import simplifyExpression
from .linereader import InputLocation
from .storage import Storage
from .types import (
    IntType, ReferenceType, Width, maskForWidth, unlimited, widthForMask
)
from .utils import checkType

if TYPE_CHECKING:
    from .codeblock_builder import CodeBlockBuilder
else:
    CodeBlockBuilder = object


class BitString:
    '''Abstract base class for bit strings.
    '''
    __slots__ = ('_width',)

    @property
    def width(self) -> Width:
        return self._width

    def __init__(self, width: Width):
        self._width = checkType(width, (int, type(unlimited)), 'width')

    def iterExpressions(self) -> Iterator[Expression]:
        '''Iterates through the expressions contained in this bit string.
        '''
        raise NotImplementedError

    def iterStorages(self) -> Iterator[Storage]:
        '''Iterates through the storages accessed through this bit string.
        '''
        raise NotImplementedError

    def substitute(
            self,
            storageFunc:
                Optional[Callable[[Storage], Optional[BitString]]] = None,
            expressionFunc:
                Optional[Callable[[Expression], Optional[Expression]]] = None
            ) -> BitString:
        '''Applies the given substitution functions to each applicable
        term in this bit string and returns the resulting bit string.
        The storage function passed a storage as its argument and must return
        None if no substitution is to take place and a replacement bit string
        otherwise.
        If no storage substitution took place, the expression function is
        applied. This function should follow the same contract as the function
        passed to Expression.substitute().
        '''
        raise NotImplementedError

    def emitLoad(self,
                 builder: CodeBlockBuilder,
                 location: Optional[InputLocation]
                 ) -> Expression:
        '''Emits load nodes for loading a bit string from the underlying
        storage(s).
        Returns the value of the bit string as an Expression.
        '''
        raise NotImplementedError

    def emitStore(self,
                  builder: CodeBlockBuilder,
                  value: Expression,
                  location: Optional[InputLocation]
                  ) -> None:
        '''Emits store nodes for storing a bit string into the underlying
        storage(s).
        '''
        raise NotImplementedError

class FixedValue(BitString):
    '''A bit string that always reads as the same value and ignores writes.
    '''
    __slots__ = ('_expr',)

    @property
    def expr(self) -> Expression:
        return self._expr

    def __init__(self, expr: Expression, width: Width):
        '''Construct a FixedValue with the given value and width.
        The mask of the value Expression must fit within the given width.
        '''
        BitString.__init__(self, width)
        self._expr = checkType(expr, Expression, 'value')
        assert widthForMask(expr.mask) <= width, expr

    def __repr__(self) -> str:
        return 'FixedValue(%r, %s)' % (self._expr, self._width)

    def __str__(self) -> str:
        return str(self._expr)

    def iterExpressions(self) -> Iterator[Expression]:
        yield self._expr

    def iterStorages(self) -> Iterator[Storage]:
        return iter(())

    def substitute(
            self,
            storageFunc:
                Optional[Callable[[Storage], Optional[BitString]]] = None,
            expressionFunc:
                Optional[Callable[[Expression], Optional[Expression]]] = None
            ) -> FixedValue:
        if expressionFunc is None:
            return self
        expr = self._expr
        newExpr = expr.substitute(expressionFunc)
        if newExpr is expr:
            return self
        else:
            return FixedValue(newExpr, self._width)

    def emitLoad(self,
                 builder: CodeBlockBuilder,
                 location: Optional[InputLocation]
                 ) -> Expression:
        return self._expr

    def emitStore(self,
                  builder: CodeBlockBuilder,
                  value: Expression,
                  location: Optional[InputLocation]
                  ) -> None:
        pass

class SingleStorage(BitString):
    __slots__ = ('_storage',)

    @property
    def storage(self) -> Storage:
        return self._storage

    def __init__(self, storage: Storage):
        self._storage = checkType(storage, Storage, 'storage')
        BitString.__init__(self, storage.width)

    def __repr__(self) -> str:
        return 'SingleStorage(%r)' % self._storage

    def __str__(self) -> str:
        return str(self._storage)

    def iterExpressions(self) -> Iterator[Expression]:
        return self._storage.iterExpressions()

    def iterStorages(self) -> Iterator[Storage]:
        yield self._storage

    def substitute(
            self,
            storageFunc:
                Optional[Callable[[Storage], Optional[BitString]]] = None,
            expressionFunc:
                Optional[Callable[[Expression], Optional[Expression]]] = None
            ) -> BitString:
        storage = self._storage
        if storageFunc is not None:
            newBits = storageFunc(storage)
            if newBits is not None:
                if isinstance(newBits, SingleStorage) and \
                        newBits._storage is storage:
                    return self
                else:
                    return newBits

        if expressionFunc is None:
            return self
        newStorage = storage.substituteExpressions(expressionFunc)
        if newStorage is storage:
            return self
        else:
            return SingleStorage(newStorage)

    def emitLoad(self,
                 builder: CodeBlockBuilder,
                 location: Optional[InputLocation]
                 ) -> Expression:
        return builder.emitLoadBits(self._storage, location)

    def emitStore(self,
                  builder: CodeBlockBuilder,
                  value: Expression,
                  location: Optional[InputLocation]
                  ) -> None:
        builder.emitStoreBits(self._storage, value, location)

class ConcatenatedBits(BitString):
    '''A concatenation of a bit strings.
    '''
    __slots__ = ('_subs',)

    def __init__(self, *subs: BitString):
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
            width += cast(int, sub.width)
        BitString.__init__(self, width)
        self._subs: Sequence[BitString] = subs

    def __repr__(self) -> str:
        return 'ConcatenatedBits(%s)' % ', '.join(
            repr(sub) for sub in self._subs
            )

    def __str__(self) -> str:
        return '(%s)' % ' ; '.join(str(sub) for sub in reversed(self._subs))

    def __iter__(self) -> Iterator[BitString]:
        return iter(self._subs)

    def iterExpressions(self) -> Iterator[Expression]:
        for sub in self._subs:
            yield from sub.iterExpressions()

    def iterStorages(self) -> Iterator[Storage]:
        for sub in self._subs:
            yield from sub.iterStorages()

    def substitute(
            self,
            storageFunc:
                Optional[Callable[[Storage], Optional[BitString]]] = None,
            expressionFunc:
                Optional[Callable[[Expression], Optional[Expression]]] = None
            ) -> ConcatenatedBits:
        changed = False
        subs = []
        for sub in self._subs:
            newBits = sub.substitute(storageFunc, expressionFunc)
            subs.append(newBits)
            changed |= newBits is not sub
        return ConcatenatedBits(*subs) if changed else self

    def emitLoad(self,
                 builder: CodeBlockBuilder,
                 location: Optional[InputLocation]
                 ) -> Expression:
        terms = []
        offset = 0
        for sub in self._subs:
            value = sub.emitLoad(builder, location)
            terms.append(value if offset == 0 else LShift(value, offset))
            offset += cast(int, sub.width)
        return OrOperator(*terms)

    def emitStore(self,
                  builder: CodeBlockBuilder,
                  value: Expression,
                  location: Optional[InputLocation]
                  ) -> None:
        offset = 0
        for sub in self._subs:
            width = cast(int, sub.width)
            valueSlice = optSlice(value, offset, width)
            sub.emitStore(builder, valueSlice, location)
            offset += width

class SlicedBits(BitString):
    '''A slice of a bit string.
    '''
    __slots__ = ('_bits', '_offset')

    @property
    def bits(self) -> BitString:
        return self._bits

    @property
    def offset(self) -> Expression:
        return self._offset

    def __init__(self, bits: BitString, offset: Expression, width: Width):
        '''Creates a slice of the given bit string.
        '''
        self._bits = checkType(bits, BitString, 'bit string')

        offset = simplifyExpression(checkType(offset, Expression, 'offset'))
        # Some invalid offsets can only be detected upon use, but others we
        # can detect on definition and rejecting them early is likely helpful
        # towards the user.
        if isinstance(offset, IntLiteral) and offset.value < 0:
            raise ValueError('slice offset must not be negative')
        self._offset = offset

        BitString.__init__(self, width)

    def __repr__(self) -> str:
        return 'SlicedBits(%r, %r, %s)' % (
            self._bits, self._offset, self._width
            )

    def __str__(self) -> str:
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
                end = str(AddOperator(offset, IntLiteral(cast(int, width))))
            return '%s[%s:%s]' % (self._bits, offset, end)

    def iterExpressions(self) -> Iterator[Expression]:
        return self._bits.iterExpressions()

    def iterStorages(self) -> Iterator[Storage]:
        return self._bits.iterStorages()

    def substitute(
            self,
            storageFunc:
                Optional[Callable[[Storage], Optional[BitString]]] = None,
            expressionFunc:
                Optional[Callable[[Expression], Optional[Expression]]] = None
            ) -> SlicedBits:
        bits = self._bits
        newBits = bits.substitute(storageFunc, expressionFunc)
        if newBits is bits:
            return self
        else:
            return SlicedBits(newBits, self._offset, self._width)

    def emitLoad(self,
                 builder: CodeBlockBuilder,
                 location: Optional[InputLocation]
                 ) -> Expression:
        # Load value from our bit string.
        value = self._bits.emitLoad(builder, location)

        # Slice the loaded value.
        return truncate(RVShift(value, self._offset), self.width)

    def emitStore(self,
                  builder: CodeBlockBuilder,
                  value: Expression,
                  location: Optional[InputLocation]
                  ) -> None:
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

        bits.emitStore(builder, simplifyExpression(combined), location)

class BadBits(BitString):
    '''A dummy bit string that can be used when an error has been discovered
    in the input but we don't want to abort parsing immediately.
    '''
    __slots__ = ()

    def __init__(self, width: Width):
        '''Construct a BadBits with the given width.
        '''
        BitString.__init__(self, width)

    def __repr__(self) -> str:
        return 'BadBits(%s)' % self._width

    def __str__(self) -> str:
        return '(%s bad bits)' % self._width

    def iterExpressions(self) -> Iterator[Expression]:
        return iter(())

    def iterStorages(self) -> Iterator[Storage]:
        return iter(())

    def substitute(
            self,
            storageFunc:
                Optional[Callable[[Storage], Optional[BitString]]] = None,
            expressionFunc:
                Optional[Callable[[Expression], Optional[Expression]]] = None
            ) -> BadBits:
        return self

    def emitLoad(self,
                 builder: CodeBlockBuilder,
                 location: Optional[InputLocation]
                 ) -> Expression:
        return BadValue(self._width)

    def emitStore(self,
                  builder: CodeBlockBuilder,
                  value: Expression,
                  location: Optional[InputLocation]
                  ) -> None:
        pass

def badReference(decl: Union[ReferenceType, IntType]) -> Reference:
    '''Returns a dummy reference to the given declared reference/value type,
    with a BadBits instance as the underlying bit string.
    '''
    typ = decl.type if isinstance(decl, ReferenceType) else decl
    return Reference(BadBits(typ.width), typ)

def decodeInt(encoded: Expression, typ: IntType) -> Expression:
    '''Decodes the given encoded representation as an integer of the given type.
    Returns the decoded value.
    '''
    if typ.signed:
        width = typ.width
        if width is not unlimited:
            return SignExtension(encoded, cast(int, width))
    return encoded

class Reference:
    '''Typed access to a bit string.
    '''
    __slots__ = ('_bits', '_type')

    @property
    def bits(self) -> BitString:
        return self._bits

    @property
    def type(self) -> IntType:
        return self._type

    @property
    def width(self) -> Width:
        return self._type.width

    def __init__(self, bits: BitString, typ: IntType):
        self._bits = checkType(bits, BitString, 'bit string')
        self._type = checkType(typ, IntType, 'value type')
        if bits.width != typ.width:
            raise ValueError(
                'bit string of %s bits used for reference of type %s'
                % (bits.width, typ)
                )

    def __str__(self) -> str:
        return '%s& %s' % (self._type, self._bits)

    def emitLoad(self,
                 builder: CodeBlockBuilder,
                 location: Optional[InputLocation]
                 ) -> Expression:
        '''Emits load nodes for loading a typed value from the referenced
        bit string.
        Returns the loaded value as an Expression.
        '''
        encoded = self._bits.emitLoad(builder, location)
        return decodeInt(encoded, self._type)

    def emitStore(self,
                  builder: CodeBlockBuilder,
                  value: Expression,
                  location: Optional[InputLocation]
                  ) -> None:
        '''Emits store nodes for storing a value into the referenced bit string.
        '''
        self._bits.emitStore(builder, truncate(value, self.width), location)
