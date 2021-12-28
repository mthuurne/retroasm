from __future__ import annotations

from typing import TYPE_CHECKING, Callable, Iterator, Sequence, cast

from .expression import (
    AddOperator,
    AndOperator,
    BadValue,
    Expression,
    IntLiteral,
    LShift,
    LVShift,
    OrOperator,
    RVShift,
    SignExtension,
    XorOperator,
    optSlice,
    truncate,
)
from .expression_simplifier import simplifyExpression
from .linereader import InputLocation
from .storage import Storage
from .symbol import SymbolValue
from .types import (
    IntType,
    ReferenceType,
    Segment,
    Width,
    mask_for_width,
    unlimited,
    width_for_mask,
)

if TYPE_CHECKING:
    from .codeblock_builder import CodeBlockBuilder
else:
    CodeBlockBuilder = object


class BitString:
    """Abstract base class for bit strings."""

    __slots__ = ("_width",)

    @property
    def width(self) -> Width:
        return self._width

    def __init__(self, width: Width):
        self._width = width

    def iterExpressions(self) -> Iterator[Expression]:
        """Iterates through the expressions contained in this bit string."""
        raise NotImplementedError

    def iterStorages(self) -> Iterator[Storage]:
        """Iterates through the storages accessed through this bit string."""
        raise NotImplementedError

    def substitute(
        self,
        storageFunc: Callable[[Storage], BitString | None] | None = None,
        expressionFunc: Callable[[Expression], Expression | None] | None = None,
    ) -> BitString:
        """
        Applies the given substitution functions to each applicable
        term in this bit string and returns the resulting bit string.
        The storage function passed a storage as its argument and must return
        None if no substitution is to take place and a replacement bit string
        otherwise.
        If no storage substitution took place, the expression function is
        applied. This function should follow the same contract as the function
        passed to Expression.substitute().
        """
        raise NotImplementedError

    def emitLoad(
        self, builder: CodeBlockBuilder, location: InputLocation | None
    ) -> Expression:
        """
        Emits load nodes for loading a bit string from the underlying
        storage(s).
        Returns the value of the bit string as an Expression.
        """
        raise NotImplementedError

    def emitStore(
        self,
        builder: CodeBlockBuilder,
        value: Expression,
        location: InputLocation | None,
    ) -> None:
        """
        Emits store nodes for storing a bit string into the underlying
        storage(s).
        """
        raise NotImplementedError

    def decompose(self) -> Iterator[tuple[FixedValue | SingleStorage, Segment]]:
        """
        Decomposes the given bit string into its leaf nodes.
        Yields a series of pairs of base string and segment in the base string.
        When concatenated, with the first yielded as the least significant bits,
        these slices produce the given bit string.
        Raises ValueError if a bit string cannot be decomposed because it
        contains a slice with an unknown offset.
        """
        raise NotImplementedError

    @property
    def intValue(self) -> int:
        """
        The value of a bit string that consists solely of integer literals.

        Raises TypeError if one of the leaves is not a FixedValue or does not
        wrap an IntLiteral.
        """
        value = 0
        width = 0
        for leaf, segment in self.decompose():
            if not isinstance(leaf, FixedValue):
                raise TypeError(f"Not a fixed value: {leaf!r}")
            expr = leaf.expr
            if not isinstance(expr, IntLiteral):
                raise TypeError(f"Not an integer literal: {expr!r}")
            value |= segment.cut(expr.value) << width
            # The width can be unlimited for the last element, but we don't use
            # it anymore after that.
            width += cast(int, segment.width)
        return value


class FixedValue(BitString):
    """A bit string that always reads as the same value and ignores writes."""

    __slots__ = ("_expr",)

    @property
    def expr(self) -> Expression:
        return self._expr

    def __init__(self, expr: Expression, width: Width):
        """
        Construct a FixedValue with the given value and width.
        The mask of the value Expression must fit within the given width.
        """
        super().__init__(width)
        self._expr = expr
        assert width_for_mask(expr.mask) <= width, expr

    def __repr__(self) -> str:
        return f"FixedValue({self._expr!r}, {self._width})"

    def __str__(self) -> str:
        return str(self._expr)

    def iterExpressions(self) -> Iterator[Expression]:
        yield self._expr

    def iterStorages(self) -> Iterator[Storage]:
        return iter(())

    def substitute(
        self,
        storageFunc: Callable[[Storage], BitString | None] | None = None,
        expressionFunc: Callable[[Expression], Expression | None] | None = None,
    ) -> FixedValue:
        if expressionFunc is None:
            return self
        expr = self._expr
        newExpr = expr.substitute(expressionFunc)
        if newExpr is expr:
            return self
        else:
            return FixedValue(newExpr, self._width)

    def emitLoad(
        self, builder: CodeBlockBuilder, location: InputLocation | None
    ) -> Expression:
        return self._expr

    def emitStore(
        self,
        builder: CodeBlockBuilder,
        value: Expression,
        location: InputLocation | None,
    ) -> None:
        pass

    def decompose(self) -> Iterator[tuple[FixedValue, Segment]]:
        yield self, Segment(0, self.width)


class SingleStorage(BitString):
    """A bit string that is read from or written to a storage location."""

    __slots__ = ("_storage",)

    @property
    def storage(self) -> Storage:
        return self._storage

    def __init__(self, storage: Storage):
        self._storage = storage
        super().__init__(storage.width)

    def __repr__(self) -> str:
        return f"SingleStorage({self._storage!r})"

    def __str__(self) -> str:
        return str(self._storage)

    def iterExpressions(self) -> Iterator[Expression]:
        return self._storage.iterExpressions()

    def iterStorages(self) -> Iterator[Storage]:
        yield self._storage

    def substitute(
        self,
        storageFunc: Callable[[Storage], BitString | None] | None = None,
        expressionFunc: Callable[[Expression], Expression | None] | None = None,
    ) -> BitString:
        storage = self._storage
        if storageFunc is not None:
            newBits = storageFunc(storage)
            if newBits is not None:
                if isinstance(newBits, SingleStorage) and newBits._storage is storage:
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

    def emitLoad(
        self, builder: CodeBlockBuilder, location: InputLocation | None
    ) -> Expression:
        return builder.emitLoadBits(self._storage, location)

    def emitStore(
        self,
        builder: CodeBlockBuilder,
        value: Expression,
        location: InputLocation | None,
    ) -> None:
        builder.emitStoreBits(self._storage, value, location)

    def decompose(self) -> Iterator[tuple[SingleStorage, Segment]]:
        yield self, Segment(0, self.width)


class ConcatenatedBits(BitString):
    """A concatenation of a bit strings."""

    __slots__ = ("_subs",)

    def __init__(self, *subs: BitString):
        """
        Creates a concatenation of the given bit strings, in order from least
        to most significant.
        """
        width = 0
        for sub in subs:
            if width is unlimited:
                raise ValueError(
                    "unlimited width is only allowed on most significant " "bit string"
                )
            width += cast(int, sub.width)
        BitString.__init__(self, width)
        self._subs: Sequence[BitString] = subs

    def __repr__(self) -> str:
        return f"ConcatenatedBits({', '.join(repr(sub) for sub in self._subs)})"

    def __str__(self) -> str:
        return f"({' ; '.join(str(sub) for sub in reversed(self._subs))})"

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
        storageFunc: Callable[[Storage], BitString | None] | None = None,
        expressionFunc: Callable[[Expression], Expression | None] | None = None,
    ) -> ConcatenatedBits:
        changed = False
        subs = []
        for sub in self._subs:
            newBits = sub.substitute(storageFunc, expressionFunc)
            subs.append(newBits)
            changed |= newBits is not sub
        return ConcatenatedBits(*subs) if changed else self

    def emitLoad(
        self, builder: CodeBlockBuilder, location: InputLocation | None
    ) -> Expression:
        terms = []
        offset = 0
        for sub in self._subs:
            value = sub.emitLoad(builder, location)
            terms.append(value if offset == 0 else LShift(value, offset))
            offset += cast(int, sub.width)
        return OrOperator(*terms)

    def emitStore(
        self,
        builder: CodeBlockBuilder,
        value: Expression,
        location: InputLocation | None,
    ) -> None:
        offset = 0
        for sub in self._subs:
            width = cast(int, sub.width)
            valueSlice = optSlice(value, offset, width)
            sub.emitStore(builder, valueSlice, location)
            offset += width

    def decompose(self) -> Iterator[tuple[FixedValue | SingleStorage, Segment]]:
        for sub in self._subs:
            yield from sub.decompose()


class SlicedBits(BitString):
    """A slice of a bit string."""

    __slots__ = ("_bits", "_offset")

    @property
    def bits(self) -> BitString:
        return self._bits

    @property
    def offset(self) -> Expression:
        return self._offset

    def __init__(self, bits: BitString, offset: Expression, width: Width):
        """Creates a slice of the given bit string."""
        self._bits = bits

        offset = simplifyExpression(offset)
        # Some invalid offsets can only be detected upon use, but others we
        # can detect on definition and rejecting them early is likely helpful
        # towards the user.
        if isinstance(offset, IntLiteral) and offset.value < 0:
            raise ValueError("slice offset must not be negative")
        self._offset = offset

        BitString.__init__(self, width)

    def __repr__(self) -> str:
        return f"SlicedBits({self._bits!r}, {self._offset!r}, {self._width})"

    def __str__(self) -> str:
        offset = self._offset
        width = self._width
        if isinstance(offset, IntLiteral):
            offsetVal = offset.value
            start = "" if offsetVal == 0 else offsetVal
            end = "" if width is unlimited else offsetVal + width
        else:
            start = str(offset)
            end = (
                ""
                if width is unlimited
                else str(AddOperator(offset, IntLiteral(cast(int, width))))
            )
        return f"{self._bits}[{start}:{end}]"

    def iterExpressions(self) -> Iterator[Expression]:
        return self._bits.iterExpressions()

    def iterStorages(self) -> Iterator[Storage]:
        return self._bits.iterStorages()

    def substitute(
        self,
        storageFunc: Callable[[Storage], BitString | None] | None = None,
        expressionFunc: Callable[[Expression], Expression | None] | None = None,
    ) -> SlicedBits:
        bits = self._bits
        newBits = bits.substitute(storageFunc, expressionFunc)
        if newBits is bits:
            return self
        else:
            return SlicedBits(newBits, self._offset, self._width)

    def emitLoad(
        self, builder: CodeBlockBuilder, location: InputLocation | None
    ) -> Expression:
        # Load value from our bit string.
        value = self._bits.emitLoad(builder, location)

        # Slice the loaded value.
        return truncate(RVShift(value, self._offset), self.width)

    def emitStore(
        self,
        builder: CodeBlockBuilder,
        value: Expression,
        location: InputLocation | None,
    ) -> None:
        offset = self._offset
        width = self.width
        valueMask = LVShift(IntLiteral(mask_for_width(width)), offset)

        # Get mask and previous value of our bit string.
        bits = self._bits
        fullMask = IntLiteral(mask_for_width(bits.width))
        prevValue = bits.emitLoad(builder, location)

        # Combine previous value with new value.
        maskLit = AndOperator(fullMask, XorOperator(IntLiteral(-1), valueMask))
        combined = OrOperator(AndOperator(prevValue, maskLit), LVShift(value, offset))

        bits.emitStore(builder, simplifyExpression(combined), location)

    def decompose(self) -> Iterator[tuple[FixedValue | SingleStorage, Segment]]:
        # Note that the offset was already simplified.
        offset = self.offset
        if not isinstance(offset, IntLiteral):
            raise ValueError("Cannot decompose bit string with a variable slice offset")

        slice_seg = Segment(offset.value, self.width)
        for base, base_seg in self.bits.decompose():
            # Clip to slice boundaries.
            clipped = (slice_seg << base_seg.start) & base_seg
            if clipped:
                yield base, clipped
            # Shift slice segment to match remaining string.
            width = base_seg.width
            if not isinstance(width, int):
                # Width can only be unlimited on last component.
                break
            slice_seg >>= width


def decodeInt(encoded: Expression, typ: IntType) -> Expression:
    """
    Decodes the given encoded representation as an integer of the given type.
    Returns the decoded value.
    """
    if typ.signed:
        width = typ.width
        if width is not unlimited:
            return SignExtension(encoded, cast(int, width))
    return encoded


class Reference:
    """Typed access to a bit string."""

    __slots__ = ("_bits", "_type")

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
        self._bits = bits
        self._type = typ
        if bits.width != typ.width:
            raise ValueError(
                f"bit string of {bits.width} bits used for reference of type {typ}"
            )

    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self._bits!r}, {self._type!r})"

    def __str__(self) -> str:
        return f"{self._type}& {self._bits}"

    def emitLoad(
        self, builder: CodeBlockBuilder, location: InputLocation | None
    ) -> Expression:
        """
        Emits load nodes for loading a typed value from the referenced
        bit string.
        Returns the loaded value as an Expression.
        """
        encoded = self._bits.emitLoad(builder, location)
        return decodeInt(encoded, self._type)

    def emitStore(
        self,
        builder: CodeBlockBuilder,
        value: Expression,
        location: InputLocation | None,
    ) -> None:
        """Emits store nodes for storing a value into the referenced bit string."""
        self._bits.emitStore(builder, truncate(value, self.width), location)


class FixedValueReference(Reference):
    """Reference to a value defined by an expression."""

    __slots__ = ()

    if TYPE_CHECKING:

        @property
        def bits(self) -> FixedValue:
            return cast(FixedValue, super().bits)

    @property
    def expr(self) -> Expression:
        return self.bits.expr

    def __init__(self, expr: Expression, typ: IntType):
        super().__init__(FixedValue(expr, typ.width), typ)

    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self.expr!r}, {self._type!r})"


def badReference(decl: ReferenceType | IntType) -> Reference:
    """
    Return a dummy reference to the given declared reference/value type,
    with a BadValue instance as the underlying expression.
    """
    typ = decl.type if isinstance(decl, ReferenceType) else decl
    return FixedValueReference(BadValue(typ.width), typ)


def intReference(value: int, typ: IntType) -> FixedValueReference:
    """
    Return a reference to a fixed integer with the given value and type.

    Raises ValueError if the value does not fit within the type.
    """
    typ.check_range(value)
    return FixedValueReference(IntLiteral(value), typ)


def symbolReference(name: str, typ: IntType) -> FixedValueReference:
    """
    Return a reference to a symbol value.
    """
    return FixedValueReference(SymbolValue(name, typ.width), typ)
