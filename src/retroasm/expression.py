from __future__ import annotations

from abc import ABC, abstractmethod
from collections.abc import Callable, Iterable, Iterator, Sequence
from functools import reduce
from operator import mul
from typing import ClassVar, Self, TypeVar, cast, override

from .types import (
    CarryMask,
    Width,
    mask_for_width,
    trailing_zeroes,
    unlimited,
    width_for_mask,
)
from .utils import const_property

ExprT = TypeVar("ExprT", bound="Expression")


class Expression(ABC):
    """
    Abstract base class for integer expressions.

    Expressions are considered equal if they have the same tree form.
    This means that for example (A + (B + C)) and ((A + B) + C) are considered
    unequal: they represent the same computation, but not the same tree.
    """

    __slots__ = ()

    @property
    @abstractmethod
    def mask(self) -> int:
        """
        A bit mask for the potential values of this expression: the mask
        is 1 for bits that might be 1 in the values and is 0 for bits that
        are certainly 0 in all possible values.
        """

    @abstractmethod
    def _ctorargs(self) -> tuple[object, ...]:
        """
        Returns a tuple containing the constructor arguments that can be
        used to re-create this expression.
        """

    @override
    def __repr__(self) -> str:
        args = ", ".join(repr(arg) for arg in self._ctorargs())
        return f"{self.__class__.__name__}({args})"

    @override
    def __eq__(self, other: object) -> bool:
        if isinstance(other, Expression):
            if self.__class__ is other.__class__:
                return self._equals(other)
            else:
                return False
        else:
            return NotImplemented

    @override
    def __hash__(self) -> int:
        return hash(self._ctorargs() + (self.__class__,))

    @abstractmethod
    def _equals(self, other: Self) -> bool:
        """
        Returns True if this expression is equal to the other expression,
        False otherwise.
        The other expression is of the same Python class as this one.
        """

    @property
    @abstractmethod
    def complexity(self) -> int:
        """
        Returns a postive number that reflects the complexity of this
        expression: the higher the number, the more complex the expression.
        This is used to compare simplification candidates.
        """

    def iter_instances(self, cls: type[ExprT]) -> Iterator[ExprT]:
        """
        Yields the subexpressions of this expression that are instances of
        the given Python type.
        """
        if isinstance(self, cls):
            yield self
        for value in self._ctorargs():
            if isinstance(value, Expression):
                yield from value.iter_instances(cls)

    def substitute(self, func: Callable[[Expression], Expression | None]) -> Expression:
        """
        Applies the given substitution function to this expression and
        returns the resulting expression.
        The function is called for each node in the expression with that
        node as the argument. If it returns None, the node is kept and the
        substitution applied to its subnodes. If it returns an expression,
        that expression is the substituted for the node.
        """
        subst = func(self)
        if subst is not None:
            return subst

        changed = False
        args = []
        for value in self._ctorargs():
            if isinstance(value, Expression):
                subst = value.substitute(func)
                if subst is not value:
                    value = subst
                    changed = True
            args.append(value)
        if changed:
            return self.__class__(*args)
        else:
            return self


class BadValue(Expression):
    """
    A dummy expression that represents an impossible value.

    This can be used when an error has been discovered in the input but
    we don't want to abort parsing immediately.
    This can also be used when it is discovered during simplification that
    an operator is applied to a value that is out of range.
    """

    __slots__ = ("_message", "_mask")

    @property
    def message(self) -> str:
        return self._message

    @property
    @override
    def mask(self) -> int:
        return self._mask

    def __init__(self, message: str, mask: int = -1):
        self._message = message
        self._mask = mask
        Expression.__init__(self)

    @override
    def _ctorargs(self) -> tuple[str, int]:
        return (self._message, self._mask)

    @override
    def __str__(self) -> str:
        return f"(bad value: {self._message})"

    @override
    def _equals(self, other: BadValue) -> bool:
        return self is other

    @property
    @override
    def complexity(self) -> int:
        return 1


class IntLiteral(Expression):
    """An integer literal."""

    __slots__ = ("_value",)

    @property
    def value(self) -> int:
        return self._value

    @property
    @override
    def mask(self) -> int:
        return self._value

    def __init__(self, value: int):
        self._value = value
        Expression.__init__(self)

    @override
    def _ctorargs(self) -> tuple[int]:
        return (self._value,)

    @override
    def __str__(self) -> str:
        value = self._value
        if value < 10:  # small, zero or negative -> print as decimal
            return str(self._value)
        else:  # print as hexadecimal
            return f"${value:X}"

    @override
    def _equals(self, other: IntLiteral) -> bool:
        return self._value == other._value

    @property
    @override
    def complexity(self) -> int:
        return 1


class MultiExpression(Expression):
    """
    Base class for expressions that combine zero or more subexpressions.
    All subclasses of this class must represent operations that are both
    associative and commutative; other algebraic properties differ per subclass.
    """

    __slots__ = ("_exprs", "_mask")

    # https://github.com/pylint-dev/pylint/issues/9950
    # pylint: disable=E0245
    operator: ClassVar[str]
    idempotent: ClassVar[bool]
    identity: ClassVar[int]
    absorber: ClassVar[int | None]

    node_complexity = 1
    """Contribution of the expression node itself to expression complexity."""

    @property
    def exprs(self) -> Sequence[Expression]:
        return self._exprs

    def __init__(self, *exprs: Expression):
        if not exprs:
            raise TypeError("one or more subexpressions must be provided")
        Expression.__init__(self)
        self._exprs = exprs

    @override
    def _ctorargs(self) -> tuple[Expression, ...]:
        return self._exprs

    @const_property
    @override
    def mask(self) -> int:
        return self.compute_mask(self._exprs)

    @classmethod
    @abstractmethod
    def compute_mask(cls, exprs: Iterable[Expression]) -> int:
        """Returns the bit mask for the composition of the given expressions."""

    @property
    @override
    def complexity(self) -> int:
        return self.node_complexity + sum(expr.complexity for expr in self._exprs)

    @classmethod
    @abstractmethod
    def combine_literals(cls, *values: int) -> int:
        """Combine the given literal values into a single value."""

    @override
    def __str__(self) -> str:
        sep = f" {self.operator} "
        return f"({sep.join(str(expr) for expr in self._exprs)})"

    @override
    def _equals(self, other: MultiExpression) -> bool:
        return len(self._exprs) == len(other._exprs) and all(
            myExpr == otherExpr
            for (myExpr, otherExpr) in zip(self._exprs, other._exprs)
        )


class AndOperator(MultiExpression):
    __slots__ = ()
    operator = "&"
    idempotent = True
    identity = -1
    absorber = 0

    @override
    def __str__(self) -> str:
        match self.exprs:
            case [*terms, IntLiteral(value=value)]:
                width = width_for_mask(value)
                if width is not unlimited and mask_for_width(width) == value:
                    assert isinstance(width, int)
                    # Special formatting for truncation and slicing.
                    match terms:
                        case [RShift(expr=expr, offset=offset)]:
                            if width == 1:
                                return f"{expr}[{offset:d}]"
                            else:
                                return f"{expr}[{offset:d}:{offset + width:d}]"
                        case [expr]:
                            if width == 1:
                                return f"{expr}[0]"
                            else:
                                return f"{expr}[:{width:d}]"
                        case terms:
                            conjuction = " & ".join(str(expr) for expr in terms)
                            return f"({conjuction})[:{width:d}]"
        return super().__str__()

    @classmethod
    @override
    def compute_mask(cls, exprs: Iterable[Expression]) -> int:
        return reduce(int.__and__, (expr.mask for expr in exprs), -1)

    @classmethod
    @override
    def combine_literals(cls, *values: int) -> int:
        return reduce(int.__and__, values, -1)


class OrOperator(MultiExpression):
    __slots__ = ()
    operator = "|"
    idempotent = True
    identity = 0
    absorber = -1

    @classmethod
    @override
    def compute_mask(cls, exprs: Iterable[Expression]) -> int:
        return reduce(int.__or__, (expr.mask for expr in exprs), 0)

    @classmethod
    @override
    def combine_literals(cls, *values: int) -> int:
        return reduce(int.__or__, values, 0)


class XorOperator(MultiExpression):
    __slots__ = ()
    operator = "^"
    idempotent = False
    identity = 0
    absorber = None

    @classmethod
    @override
    def compute_mask(cls, exprs: Iterable[Expression]) -> int:
        # Note: OR not XOR, since we don't know whether a bit is set in an
        #       even or an odd number of subexpressions.
        return reduce(int.__or__, (expr.mask for expr in exprs), 0)

    @classmethod
    @override
    def combine_literals(cls, *values: int) -> int:
        return reduce(int.__xor__, values, 0)


class AddOperator(MultiExpression):
    __slots__ = ()
    operator = "+"
    idempotent = False
    identity = 0
    absorber = None

    @classmethod
    @override
    def compute_mask(cls, exprs: Iterable[Expression]) -> int:
        return reduce(
            CarryMask.__add__, (CarryMask.from_pattern(expr.mask) for expr in exprs)
        ).pattern

    @classmethod
    @override
    def combine_literals(cls, *values: int) -> int:
        return sum(values)

    @override
    def __str__(self) -> str:
        exprs = self._exprs
        fragments = [str(exprs[0])]
        for expr in exprs[1:]:
            match expr:
                case Complement(expr=expr):
                    fragments += ("-", str(expr))
                case expr:
                    fragments += ("+", str(expr))
        return f"({' '.join(fragments)})"


class MultiplyOperator(MultiExpression):
    __slots__ = ()
    operator = "*"
    idempotent = False
    identity = 1
    absorber = 0

    @classmethod
    @override
    def compute_mask(cls, exprs: Iterable[Expression]) -> int:
        # TODO: Produce a more accurate mask using CarryMask.
        return -1

    @classmethod
    @override
    def combine_literals(cls, *values: int) -> int:
        return reduce(mul, values, 1)

    @override
    def __str__(self) -> str:
        return f"({' * '.join(str(expr) for expr in self._exprs)})"


class SingleExpression(Expression):
    """Base class for expressions that have a single subexpression."""

    __slots__ = ("_expr",)

    @property
    def expr(self) -> Expression:
        return self._expr

    def __init__(self, expr: Expression):
        Expression.__init__(self)
        self._expr = expr

    @override
    def _ctorargs(self) -> tuple[object, ...]:
        return (self._expr,)

    @override
    def _equals(self, other: Self) -> bool:
        return self._expr == other._expr

    @property
    @override
    def complexity(self) -> int:
        return 1 + self._expr.complexity


class Complement(SingleExpression):
    __slots__ = ("_mask",)

    @override
    def __str__(self) -> str:
        return f"-{self._expr}"

    @const_property
    @override
    def mask(self) -> int:
        expr_mask = self._expr.mask
        return 0 if expr_mask == 0 else -1 << cast(int, trailing_zeroes(expr_mask))


class Negation(SingleExpression):
    __slots__ = ()

    @property
    @override
    def mask(self) -> int:
        return 1

    @override
    def __str__(self) -> str:
        return f"!{self._expr}"


class SignTest(SingleExpression):
    """Tests the sign of the given expression."""

    __slots__ = ()

    @property
    @override
    def mask(self) -> int:
        return 1

    @override
    def __str__(self) -> str:
        return f"sign({self._expr})"


class SignExtension(SingleExpression):
    """Extends the sign bit at the front of a given expression."""

    __slots__ = ("_width",)

    @property
    def width(self) -> int:
        return self._width

    @property
    @override
    def mask(self) -> int:
        return 0 if self._width == 0 else -1

    def __init__(self, expr: Expression, width: int):
        SingleExpression.__init__(self, expr)
        self._width = width

    @override
    def _ctorargs(self) -> tuple[Expression, int]:
        return self._expr, self._width

    @override
    def __str__(self) -> str:
        return f"s{self._width:d}({self._expr})"

    @override
    def _equals(self, other: SignExtension) -> bool:
        return self._width == other._width and super()._equals(other)


_SHIFT_LIMIT_BITS = 256
"""
When shifting left, do not create masks longer than this number of bits.
While Python integers can grow arbitrarily large, they will take up a lot of
memory and take long to compute with. So we want to avoid creating masks
that are overly large.
Note that we can still produce a correct mask even when applying this limit,
it just won't be as strict as possible. Since bits this far into the expression
will most likely be truncated at a later stage, getting an exact mask is not
important.
"""


class LShift(SingleExpression):
    """Shifts a bit string to the left, appending zero bits at the end."""

    __slots__ = ("_offset", "_mask")

    @property
    def offset(self) -> int:
        return self._offset

    def __init__(self, expr: Expression, offset: int):
        SingleExpression.__init__(self, expr)
        self._offset = offset
        if offset < 0:
            raise ValueError("negative shift count")

    @override
    def _ctorargs(self) -> tuple[Expression, int]:
        return self._expr, self._offset

    @override
    def __str__(self) -> str:
        return f"({self._expr} << {self._offset:d})"

    @override
    def _equals(self, other: LShift) -> bool:
        return self._offset == other._offset and super()._equals(other)

    @const_property
    @override
    def mask(self) -> int:
        expr_mask = self._expr.mask
        if expr_mask == 0:
            return 0
        offset = self._offset
        if offset < _SHIFT_LIMIT_BITS:
            return expr_mask << offset
        else:
            return -1 << _SHIFT_LIMIT_BITS


class RShift(SingleExpression):
    """Drops the lower N bits from a bit string."""

    __slots__ = ("_offset", "_mask")

    @property
    def offset(self) -> int:
        return self._offset

    def __init__(self, expr: Expression, offset: int):
        SingleExpression.__init__(self, expr)
        self._offset = offset
        if offset < 0:
            raise ValueError("negative shift count")

    @override
    def _ctorargs(self) -> tuple[Expression, int]:
        return self._expr, self._offset

    @override
    def __str__(self) -> str:
        if self.mask == 1:
            return f"{self._expr}[{self._offset:d}]"
        else:
            return f"{self._expr}[{self._offset:d}:]"

    @override
    def _equals(self, other: RShift) -> bool:
        return self._offset == other._offset and super()._equals(other)

    @property
    @override
    def complexity(self) -> int:
        return 1 + self._expr.complexity

    @const_property
    @override
    def mask(self) -> int:
        return self._expr.mask >> self._offset


class LVShift(Expression):
    """
    Shifts a bit string to the left, appending zero bits at the end.
    Unlike LShift, our offset is an expression.
    """

    __slots__ = ("_expr", "_offset", "_mask")

    @property
    def expr(self) -> Expression:
        return self._expr

    @property
    def offset(self) -> Expression:
        return self._offset

    def __init__(self, expr: Expression, offset: Expression):
        Expression.__init__(self)
        self._expr = expr
        self._offset = offset

    @property
    @override
    def complexity(self) -> int:
        return 1 + self._expr.complexity + self._offset.complexity

    @const_property
    @override
    def mask(self) -> int:
        expr_mask = self._expr.mask
        if expr_mask == 0:
            return 0
        offset_mask = self._offset.mask
        width = cast(
            int, width_for_mask(offset_mask if offset_mask >= 0 else ~offset_mask)
        )
        mask = expr_mask
        for i in range(width):
            if 1 << i >= _SHIFT_LIMIT_BITS:
                mask |= -1 << (1 << i)
                break
            if (offset_mask >> i) & 1:
                mask |= mask << (1 << i)
        return (
            mask
            if offset_mask >= 0
            else mask | (-1 << ((1 << width) + cast(int, trailing_zeroes(expr_mask))))
        )

    @override
    def _ctorargs(self) -> tuple[Expression, Expression]:
        return self._expr, self._offset

    @override
    def __str__(self) -> str:
        return f"({self._expr} << {self._offset})"

    @override
    def _equals(self, other: LVShift) -> bool:
        return self._offset == other._offset and self._expr == other._expr


class RVShift(Expression):
    """
    Drops the lower N bits from a bit string.
    Unlike RShift, our offset (N) is an expression.
    """

    __slots__ = ("_expr", "_offset", "_mask")

    @property
    def expr(self) -> Expression:
        return self._expr

    @property
    def offset(self) -> Expression:
        return self._offset

    def __init__(self, expr: Expression, offset: Expression):
        Expression.__init__(self)
        self._expr = expr
        self._offset = offset

    @property
    @override
    def complexity(self) -> int:
        return 1 + self._expr.complexity + self._offset.complexity

    @const_property
    @override
    def mask(self) -> int:
        expr_mask = self._expr.mask
        offset_mask = self._offset.mask
        if offset_mask >= 0:
            # There is a limited number of possible offsets.
            offset_width = cast(int, width_for_mask(offset_mask))
        elif expr_mask >= 0:
            # There is an unlimited number of possible offsets, but only
            # offsets below the width of the expression mask contribute.
            expr_width = cast(int, width_for_mask(expr_mask))
            offset_width = cast(int, width_for_mask(expr_width))
        else:
            # There is no value we can rule out.
            assert expr_mask < 0 and offset_mask < 0, self
            return -1
        mask = expr_mask
        for i in range(offset_width):
            if (offset_mask >> i) & 1:
                mask |= mask >> (1 << i)
        return mask

    @override
    def _ctorargs(self) -> tuple[Expression, Expression]:
        return self._expr, self._offset

    @override
    def __str__(self) -> str:
        return f"({self._expr} >> {self._offset})"

    @override
    def _equals(self, other: RVShift) -> bool:
        return self._offset == other._offset and self._expr == other._expr


def truncate(expr: Expression, width: Width) -> Expression:
    return AndOperator(expr, IntLiteral(mask_for_width(width)))


def opt_slice(expr: Expression, index: int, width: Width) -> Expression:
    """
    Return a slice of the given expression, at the given index with the given width,
    without adding any unnecessary operations.
    """
    if index != 0:
        expr = RShift(expr, index)
    mask = expr.mask
    if mask & mask_for_width(width) != mask:
        expr = truncate(expr, width)
    return expr
