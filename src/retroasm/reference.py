from __future__ import annotations

from abc import ABC, abstractmethod
from collections.abc import Callable, Iterator, Sequence
from typing import TYPE_CHECKING, cast, override

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
    opt_slice,
    truncate,
)
from .expression_simplifier import simplify_expression
from .input import InputLocation
from .storage import IOChannel, IOStorage, Storage
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


class BitString(ABC):
    """Abstract base class for bit strings."""

    __slots__ = ("_width",)

    @property
    def width(self) -> Width:
        return self._width

    def __init__(self, width: Width):
        self._width = width

    @abstractmethod
    def iter_expressions(self) -> Iterator[Expression]:
        """Iterates through the expressions contained in this bit string."""

    @abstractmethod
    def iter_storages(self) -> Iterator[Storage]:
        """Iterates through the storages accessed through this bit string."""

    def simplify(self) -> BitString:
        """
        Return an equivalent new bit string with all contained expressions simplified,
        or this bit string itself if nothing could be simplified.
        """
        return self.substitute(expression_func=simplify_expression)

    @abstractmethod
    def substitute(
        self,
        *,
        storage_func: Callable[[Storage], BitString | None] | None = None,
        expression_func: Callable[[Expression], Expression | None] | None = None,
        variable_func: Callable[[Variable], BitString | None] | None = None,
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

    @abstractmethod
    def emit_load(
        self, builder: CodeBlockBuilder, location: InputLocation | None
    ) -> Expression:
        """
        Emits load nodes for loading a bit string from the underlying
        storage(s).
        Returns the value of the bit string as an Expression.
        """

    @abstractmethod
    def emit_store(
        self,
        builder: CodeBlockBuilder,
        value: Expression,
        location: InputLocation | None,
    ) -> None:
        """
        Emits store nodes for storing a bit string into the underlying
        storage(s).
        """

    @abstractmethod
    def decompose(self) -> Iterator[tuple[AtomicBitString, Segment]]:
        """
        Decomposes the given bit string into its leaf nodes.
        Yields a series of pairs of base string and segment in the base string.
        When concatenated, with the first yielded as the least significant bits,
        these slices produce the given bit string.
        Raises ValueError if a bit string cannot be decomposed because it
        contains a slice with an unknown offset.
        """

    @property
    def int_value(self) -> int:
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

    @override
    def __repr__(self) -> str:
        return f"FixedValue({self._expr!r}, {self._width})"

    @override
    def __str__(self) -> str:
        return str(self._expr)

    @override
    def iter_expressions(self) -> Iterator[Expression]:
        yield self._expr

    @override
    def iter_storages(self) -> Iterator[Storage]:
        return iter(())

    @override
    def substitute(
        self,
        *,
        storage_func: Callable[[Storage], BitString | None] | None = None,
        expression_func: Callable[[Expression], Expression | None] | None = None,
        variable_func: Callable[[Variable], BitString | None] | None = None,
    ) -> FixedValue:
        if expression_func is None:
            return self
        expr = self._expr
        new_expr = expr.substitute(expression_func)
        if new_expr is expr:
            return self
        else:
            return FixedValue(simplify_expression(new_expr), self._width)

    @override
    def emit_load(
        self, builder: CodeBlockBuilder, location: InputLocation | None
    ) -> Expression:
        return self._expr

    @override
    def emit_store(
        self,
        builder: CodeBlockBuilder,
        value: Expression,
        location: InputLocation | None,
    ) -> None:
        pass

    @override
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

    @override
    def __repr__(self) -> str:
        return f"SingleStorage({self._storage!r})"

    @override
    def __str__(self) -> str:
        return str(self._storage)

    @override
    def iter_expressions(self) -> Iterator[Expression]:
        return self._storage.iter_expressions()

    @override
    def iter_storages(self) -> Iterator[Storage]:
        yield self._storage

    @override
    def substitute(
        self,
        *,
        storage_func: Callable[[Storage], BitString | None] | None = None,
        expression_func: Callable[[Expression], Expression | None] | None = None,
        variable_func: Callable[[Variable], BitString | None] | None = None,
    ) -> BitString:
        storage = self._storage
        if storage_func is not None:
            match storage_func(storage):
                case None:
                    pass
                case SingleStorage(_storage=new_storage) if new_storage is storage:
                    return self
                case new_bits:
                    return new_bits

        if expression_func is None:
            return self
        new_storage = storage.substitute_expressions(expression_func)
        if new_storage is storage:
            return self
        else:
            return SingleStorage(new_storage)

    @override
    def emit_load(
        self, builder: CodeBlockBuilder, location: InputLocation | None
    ) -> Expression:
        return builder.emit_load_bits(self._storage, location)

    @override
    def emit_store(
        self,
        builder: CodeBlockBuilder,
        value: Expression,
        location: InputLocation | None,
    ) -> None:
        builder.emit_store_bits(self._storage, value, location)

    @override
    def decompose(self) -> Iterator[tuple[SingleStorage, Segment]]:
        yield self, Segment(0, self.width)


class Variable(BitString):
    """A local variable."""

    __slots__ = ("_name",)

    @property
    def name(self) -> str:
        return self._name

    def __init__(self, name: str, width: Width):
        self._name = name
        super().__init__(width)

    @override
    def __repr__(self) -> str:
        return f"Variable({self._name}, {self._width})"

    @override
    def __str__(self) -> str:
        return f"var{self._width} {self._name}"

    @override
    def iter_expressions(self) -> Iterator[Expression]:
        return iter(())

    @override
    def iter_storages(self) -> Iterator[Storage]:
        return iter(())

    @override
    def substitute(
        self,
        *,
        storage_func: Callable[[Storage], BitString | None] | None = None,
        expression_func: Callable[[Expression], Expression | None] | None = None,
        variable_func: Callable[[Variable], BitString | None] | None = None,
    ) -> BitString:
        if variable_func is not None:
            new_bits = variable_func(self)
            if new_bits is not None:
                return new_bits
        return self

    @override
    def emit_load(
        self, builder: CodeBlockBuilder, location: InputLocation | None
    ) -> Expression:
        return builder.read_variable(self, location)

    @override
    def emit_store(
        self,
        builder: CodeBlockBuilder,
        value: Expression,
        location: InputLocation | None,
    ) -> None:
        return builder.write_variable(self, value, location)

    @override
    def decompose(self) -> Iterator[tuple[Variable, Segment]]:
        yield self, Segment(0, self.width)


class ConcatenatedBits(BitString):
    """A concatenation of a bit strings."""

    __slots__ = ("_subs",)

    def __init__(self, *subs: BitString):
        """
        Creates a concatenation of the given bit strings, in order from least
        to most significant.
        """
        width: Width = 0
        for sub in subs:
            if width is unlimited:
                raise ValueError(
                    "unlimited width is only allowed on most significant bit string"
                )
            width += cast(int, sub.width)
        BitString.__init__(self, width)
        self._subs: Sequence[BitString] = subs

    @override
    def __repr__(self) -> str:
        return f"ConcatenatedBits({', '.join(repr(sub) for sub in self._subs)})"

    @override
    def __str__(self) -> str:
        return f"({' ; '.join(str(sub) for sub in reversed(self._subs))})"

    def __iter__(self) -> Iterator[BitString]:
        return iter(self._subs)

    @override
    def iter_expressions(self) -> Iterator[Expression]:
        for sub in self._subs:
            yield from sub.iter_expressions()

    @override
    def iter_storages(self) -> Iterator[Storage]:
        for sub in self._subs:
            yield from sub.iter_storages()

    @override
    def substitute(
        self,
        *,
        storage_func: Callable[[Storage], BitString | None] | None = None,
        expression_func: Callable[[Expression], Expression | None] | None = None,
        variable_func: Callable[[Variable], BitString | None] | None = None,
    ) -> ConcatenatedBits:
        changed = False
        subs = []
        for sub in self._subs:
            new_bits = sub.substitute(
                storage_func=storage_func,
                expression_func=expression_func,
                variable_func=variable_func,
            )
            subs.append(new_bits)
            changed |= new_bits is not sub
        return ConcatenatedBits(*subs) if changed else self

    @override
    def emit_load(
        self, builder: CodeBlockBuilder, location: InputLocation | None
    ) -> Expression:
        terms = []
        offset = 0
        for sub in self._subs:
            value = sub.emit_load(builder, location)
            terms.append(value if offset == 0 else LShift(value, offset))
            offset += cast(int, sub.width)
        return OrOperator(*terms)

    @override
    def emit_store(
        self,
        builder: CodeBlockBuilder,
        value: Expression,
        location: InputLocation | None,
    ) -> None:
        offset = 0
        for sub in self._subs:
            width = cast(int, sub.width)
            value_slice = opt_slice(value, offset, width)
            sub.emit_store(builder, value_slice, location)
            offset += width

    @override
    def decompose(self) -> Iterator[tuple[AtomicBitString, Segment]]:
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

        # Some invalid offsets can only be detected upon use, but others we
        # can detect on definition and rejecting them early is likely helpful
        # towards the user.
        match simplify_expression(offset):
            case IntLiteral(value=value) if value < 0:
                raise ValueError("slice offset must not be negative")
            case offset:
                self._offset = offset

        BitString.__init__(self, width)

    @override
    def __repr__(self) -> str:
        return f"SlicedBits({self._bits!r}, {self._offset!r}, {self._width})"

    @override
    def __str__(self) -> str:
        width = self._width
        match self._offset:
            case IntLiteral(value=offset_val):
                start = "" if offset_val == 0 else offset_val
                end = "" if width is unlimited else offset_val + width
            case offset:
                start = str(offset)
                end = (
                    ""
                    if width is unlimited
                    else str(AddOperator(offset, IntLiteral(width)))
                )
        return f"{self._bits}[{start}:{end}]"

    @override
    def iter_expressions(self) -> Iterator[Expression]:
        return self._bits.iter_expressions()

    @override
    def iter_storages(self) -> Iterator[Storage]:
        return self._bits.iter_storages()

    @override
    def substitute(
        self,
        *,
        storage_func: Callable[[Storage], BitString | None] | None = None,
        expression_func: Callable[[Expression], Expression | None] | None = None,
        variable_func: Callable[[Variable], BitString | None] | None = None,
    ) -> SlicedBits:
        bits = self._bits
        new_bits = bits.substitute(
            storage_func=storage_func,
            expression_func=expression_func,
            variable_func=variable_func,
        )
        if new_bits is bits:
            return self
        else:
            return SlicedBits(new_bits, self._offset, self._width)

    @override
    def emit_load(
        self, builder: CodeBlockBuilder, location: InputLocation | None
    ) -> Expression:
        # Load value from our bit string.
        value = self._bits.emit_load(builder, location)

        # Slice the loaded value.
        return truncate(RVShift(value, self._offset), self.width)

    @override
    def emit_store(
        self,
        builder: CodeBlockBuilder,
        value: Expression,
        location: InputLocation | None,
    ) -> None:
        offset = self._offset
        width = self.width
        value_mask = LVShift(IntLiteral(mask_for_width(width)), offset)

        # Get mask and previous value of our bit string.
        bits = self._bits
        full_mask = IntLiteral(mask_for_width(bits.width))
        prev_value = bits.emit_load(builder, location)

        # Combine previous value with new value.
        mask_lit = AndOperator(full_mask, XorOperator(IntLiteral(-1), value_mask))
        combined = OrOperator(AndOperator(prev_value, mask_lit), LVShift(value, offset))

        bits.emit_store(builder, simplify_expression(combined), location)

    @override
    def decompose(self) -> Iterator[tuple[AtomicBitString, Segment]]:
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


type AtomicBitString = FixedValue | SingleStorage | Variable
"""An atomic bit string is one that decomposes into itself."""


def decode_int(encoded: Expression, typ: IntType) -> Expression:
    """
    Decodes the given encoded representation as an integer of the given type.
    Returns the decoded value.
    """
    if typ.signed:
        width = typ.width
        if width is not unlimited:
            return SignExtension(encoded, width)
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

    @override
    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self._bits!r}, {self._type!r})"

    @override
    def __str__(self) -> str:
        return f"{self._type}& {self._bits}"

    def simplify(self) -> Reference:
        """
        Return an equivalent new reference with all contained expressions simplified,
        or this reference itself if nothing could be simplified.
        """
        bits = self._bits
        new_bits = bits.simplify()
        return self if new_bits is bits else Reference(new_bits, self._type)

    def emit_load(
        self, builder: CodeBlockBuilder, location: InputLocation | None
    ) -> Expression:
        """
        Emits load nodes for loading a typed value from the referenced
        bit string.
        Returns the loaded value as an Expression.
        """
        encoded = self._bits.emit_load(builder, location)
        return decode_int(encoded, self._type)

    def emit_store(
        self,
        builder: CodeBlockBuilder,
        value: Expression,
        location: InputLocation | None,
    ) -> None:
        """Emits store nodes for storing a value into the referenced bit string."""
        self._bits.emit_store(builder, truncate(value, self.width), location)


class FixedValueReference(Reference):
    """Reference to a value defined by an expression."""

    __slots__ = ()

    if TYPE_CHECKING:

        @property
        @override
        def bits(self) -> FixedValue:
            return cast(FixedValue, super().bits)

    @property
    def expr(self) -> Expression:
        return self.bits.expr

    def __init__(self, expr: Expression, typ: IntType):
        super().__init__(FixedValue(expr, typ.width), typ)

    @override
    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self.expr!r}, {self._type!r})"


def bad_reference(decl: ReferenceType | IntType, message: str) -> Reference:
    """
    Return a dummy reference to the given declared reference/value type,
    with a BadValue instance as the underlying expression.
    """
    typ = decl.type if isinstance(decl, ReferenceType) else decl
    return FixedValueReference(BadValue(message, typ.mask), typ)


def int_reference(value: int, typ: IntType) -> FixedValueReference:
    """
    Return a reference to a fixed integer with the given value and type.

    Raises ValueError if the value does not fit within the type.
    """
    typ.check_range(value)
    return FixedValueReference(IntLiteral(value), typ)


def symbol_reference(name: str, typ: IntType) -> FixedValueReference:
    """
    Return a reference to a symbol value.
    """
    return FixedValueReference(SymbolValue(name, typ.width), typ)


def io_reference(channel: IOChannel, index: Expression) -> Reference:
    """
    Return a reference to a specific index in an I/O channel.
    """
    addr_width = channel.addr_type.width
    truncated_index = opt_slice(index, 0, addr_width)
    storage = IOStorage(channel, truncated_index)
    bits = SingleStorage(storage)
    return Reference(bits, channel.elem_type)
