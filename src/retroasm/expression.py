from functools import reduce
from itertools import chain
from typing import Optional

from .types import (
    maskForWidth, maskToSegments, trailingZeroes, unlimited, widthForMask
)
from .utils import checkType, const_property

# pylint: disable=protected-access

class Expression:
    '''Abstract base class for integer expressions.

    Expressions are considered equal if they have the same tree form.
    This means that for example (A + (B + C)) and ((A + B) + C) are considered
    unequal: they represent the same computation, but not the same tree.
    '''

    __slots__ = ()

    mask = property()
    '''A bit mask for the potential values of this expression: the mask is 1
    for bits that might be 1 in the values and is 0 for bits that are certainly
    0 in all possible values.
    '''

    def _ctorargs(self):
        '''Returns a tuple containing the constructor arguments that can be
        used to re-create this expression.
        '''
        raise NotImplementedError

    def __repr__(self):
        return '%s(%s)' % (
            self.__class__.__name__,
            ', '.join(repr(arg) for arg in self._ctorargs())
            )

    def __eq__(self, other):
        if isinstance(other, Expression):
            if self.__class__ is other.__class__:
                return self._equals(other)
            else:
                return False
        else:
            return NotImplemented

    def __ne__(self, other):
        if isinstance(other, Expression):
            if self.__class__ is other.__class__:
                return not self._equals(other)
            else:
                return True
        else:
            return NotImplemented

    def __hash__(self):
        return hash(tuple(self._ctorargs()) + (self.__class__,))

    def _equals(self, other):
        '''Returns True if this expression is equal to the other expression,
        False otherwise.
        The other expression is of the same Python class as this one.
        '''
        raise NotImplementedError

    @property
    def complexity(self):
        '''Returns a postive number that reflects the complexity of this
        expression: the higher the number, the more complex the expression.
        This is used to compare simplification candidates.
        '''
        raise NotImplementedError

    def iterInstances(self, cls):
        '''Yields the subexpressions of this expression that are instances of
        the given Python type.
        '''
        if isinstance(self, cls):
            yield self
        for value in self._ctorargs():
            if isinstance(value, Expression):
                yield from value.iterInstances(cls)

    def substitute(self, func):
        '''Applies the given substitution function to this expression and
        returns the resulting expression.
        The function is called for each node in the expression with that
        node as the argument. If it returns None, the node is kept and the
        substitution applied to its subnodes. If it returns an expression,
        that expression is the substituted for the node.
        '''
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
    '''A dummy expression that can be used when an error has been discovered
    in the input but we don't want to abort parsing immediately.
    '''
    __slots__ = ('_width',)

    mask = property(lambda self: maskForWidth(self._width))

    def __init__(self, width):
        self._width = checkType(width, (int, unlimited), 'width')
        Expression.__init__(self)

    def _ctorargs(self):
        return self._width,

    def __str__(self):
        return '(%s-bit bad value)' % self._width

    def _equals(self, other):
        return self is other

    @property
    def complexity(self):
        return 1

class IntLiteral(Expression):
    '''An integer literal.
    '''
    __slots__ = ('_value',)

    value = property(lambda self: self._value)
    mask = property(lambda self: self._value)

    def __init__(self, value):
        self._value = checkType(value, int, 'value')
        Expression.__init__(self)

    def _ctorargs(self):
        return self._value,

    def __str__(self):
        value = self._value
        if value < 10: # small, zero or negative -> print as decimal
            return str(self._value)
        else: # print as hexadecimal
            return '$%X' % value

    def _equals(self, other):
        return self._value == other._value

    @property
    def complexity(self):
        return 1

class MultiExpression(Expression):
    '''Base class for expressions that combine zero or more subexpressions.
    All subclasses of this class must represent operations that are both
    associative and commutative; other algebraic properties differ per subclass.
    '''
    __slots__ = ('_exprs', '_mask')
    operator: str
    idempotent: bool
    identity: int
    absorber: Optional[int]

    nodeComplexity = 1
    '''Contribution of the expression node itself to expression complexity.'''

    exprs = property(lambda self: self._exprs)

    def __init__(self, *exprs):
        if not exprs:
            raise TypeError('one or more subexpressions must be provided')
        for expr in exprs:
            checkType(expr, Expression, 'subexpression')
        Expression.__init__(self)
        self._exprs = exprs

    def _ctorargs(self):
        return self._exprs

    @const_property
    def mask(self):
        return self.computeMask(self._exprs)

    @classmethod
    def computeMask(cls, exprs):
        '''Returns the bit mask for the composition of the given expressions.
        '''
        raise NotImplementedError

    @property
    def complexity(self):
        return self.nodeComplexity + sum(
            expr.complexity for expr in self._exprs
            )

    @classmethod
    def combineLiterals(cls, *values):
        '''Combine the given literal values into a single value.
        '''
        raise NotImplementedError

    def __str__(self):
        sep = ' %s ' % self.operator
        return '(%s)' % sep.join(str(expr) for expr in self._exprs)

    def _equals(self, other):
        return len(self._exprs) == len(other._exprs) and all(
            myExpr == otherExpr
            for (myExpr, otherExpr) in zip(self._exprs, other._exprs)
            )

class AndOperator(MultiExpression):
    __slots__ = ('_tryDistributeAndOverOr',)
    operator = '&'
    idempotent = True
    identity = -1
    absorber = 0

    def __init__(self, *exprs):
        MultiExpression.__init__(self, *exprs)

        # Set this to False to block the simplification attempt.
        self._tryDistributeAndOverOr = True

    def __str__(self):
        exprs = self.exprs
        last = exprs[-1]
        if isinstance(last, IntLiteral):
            value = last.value
            width = widthForMask(value)
            if width is not unlimited and maskForWidth(width) == value:
                # Special formatting for truncation and slicing.
                if len(exprs) == 2:
                    first = exprs[0]
                    if isinstance(first, RShift):
                        offset = first.offset
                        if width == 1:
                            return '%s[%d]' % (first.expr, offset)
                        else:
                            return '%s[%d:%d]' % (
                                first.expr, offset, offset + width
                                )
                    else:
                        if width == 1:
                            return '%s[0]' % first
                        else:
                            return '%s[:%d]' % (first, width)
                else:
                    return '(%s)[:%d]' % (
                        ' & '.join(str(expr) for expr in exprs[:-1]), width
                        )
        return super().__str__()

    @classmethod
    def computeMask(cls, exprs):
        return reduce(int.__and__, (expr.mask for expr in exprs), -1)

    @classmethod
    def combineLiterals(cls, *values):
        return reduce(int.__and__, values, -1)

class OrOperator(MultiExpression):
    __slots__ = ('_tryDistributeOrOverAnd', )
    operator = '|'
    idempotent = True
    identity = 0
    absorber = -1

    def __init__(self, *exprs):
        MultiExpression.__init__(self, *exprs)

        # Set this to False to block the simplification attempt.
        self._tryDistributeOrOverAnd = True

    @classmethod
    def computeMask(cls, exprs):
        return reduce(int.__or__, (expr.mask for expr in exprs), 0)

    @classmethod
    def combineLiterals(cls, *values):
        return reduce(int.__or__, values, 0)

class XorOperator(MultiExpression):
    __slots__ = ()
    operator = '^'
    idempotent = False
    identity = 0
    absorber = None

    @classmethod
    def computeMask(cls, exprs):
        # Note: OR not XOR, since we don't know whether a bit is set in an
        #       even or an odd number of subexpressions.
        return reduce(int.__or__, (expr.mask for expr in exprs), 0)

    @classmethod
    def combineLiterals(cls, *values):
        return reduce(int.__xor__, values, 0)

class AddOperator(MultiExpression):
    __slots__ = ()
    operator = '+'
    idempotent = False
    identity = 0
    absorber = None

    @classmethod
    def computeMask(cls, exprs):
        result = 0
        cmbValue = 0
        cmbMask = 0
        for start, end in sorted(chain(*(
                maskToSegments(expr.mask) for expr in exprs))):
            # Compute bit mask for this segment.
            segMask = maskForWidth(end - start) << start
            # If masks don't overlap, restart adding.
            if (segMask & cmbMask) == 0:
                cmbStart = start
                cmbValue = 0
            # Maximum value is when the value is equal to the mask.
            cmbValue += segMask if segMask >= 0 else unlimited
            # Compute bit mask for maximum combined value.
            cmbMask = -1 << cmbStart
            if cmbValue is not unlimited:
                cmbMask &= (1 << cmbValue.bit_length()) - 1
            result |= cmbMask
        return result

    @classmethod
    def combineLiterals(cls, *values):
        return sum(values)

    def __str__(self):
        exprs = self._exprs
        fragments = [str(exprs[0])]
        for expr in exprs[1:]:
            if isinstance(expr, Complement):
                fragments += ('-', str(expr.expr))
            else:
                fragments += ('+', str(expr))
        return '(%s)' % ' '.join(fragments)

class SingleExpression(Expression):
    '''Base class for expressions that have a single subexpression.
    '''
    __slots__ = ('_expr',)

    expr = property(lambda self: self._expr)

    def __init__(self, expr):
        Expression.__init__(self)
        self._expr = checkType(expr, Expression, 'subexpression')

    def _ctorargs(self):
        return self._expr,

    def _equals(self, other):
        return self._expr == other._expr

    @property
    def complexity(self):
        return 1 + self._expr.complexity

class Complement(SingleExpression):
    __slots__ = ('_mask',)

    def __str__(self):
        return '-%s' % self._expr

    @const_property
    def mask(self):
        exprMask = self._expr.mask
        return 0 if exprMask == 0 else -1 << trailingZeroes(exprMask)

class Negation(SingleExpression):
    __slots__ = ()

    mask = property(lambda self: 1)

    def __str__(self):
        return '!%s' % self._expr

class SignTest(SingleExpression):
    '''Tests the sign of the given expression.
    '''
    __slots__ = ()

    mask = property(lambda self: 1)

    def __str__(self):
        return 'sign(%s)' % self._expr

class SignExtension(SingleExpression):
    '''Extends the sign bit at the front of a given expression.
    '''
    __slots__ = ('_width',)

    width = property(lambda self: self._width)
    mask = property(lambda self: 0 if self._width == 0 else -1)

    def __init__(self, expr, width):
        SingleExpression.__init__(self, expr)
        self._width = checkType(width, int, 'width')

    def _ctorargs(self):
        return self._expr, self._width

    def __str__(self):
        return 's%d(%s)' % (self._width, self._expr)

    def _equals(self, other):
        return self._width == other._width and super()._equals(other)

_SHIFT_LIMIT_BITS = 256
'''When shifting left, do not create masks longer than this number of bits.
While Python integers can grow arbitrarily large, they will take up a lot of
memory and take long to compute with. So we want to avoid creating masks
that are overly large.
Note that we can still produce a correct mask even when applying this limit,
it just won't be as strict as possible. Since bits this far into the expression
will most likely be truncated at a later stage, getting an exact mask is not
important.
'''

class LShift(SingleExpression):
    '''Shifts a bit string to the left, appending zero bits at the end.
    '''
    __slots__ = ('_offset', '_mask')

    offset = property(lambda self: self._offset)

    def __init__(self, expr, offset):
        SingleExpression.__init__(self, expr)
        self._offset = checkType(offset, int, 'shift offset')
        if offset < 0:
            raise ValueError('negative shift count')

    def _ctorargs(self):
        return self._expr, self._offset

    def __str__(self):
        return '(%s << %d)' % (self._expr, self._offset)

    def _equals(self, other):
        return self._offset == other._offset and super()._equals(other)

    @const_property
    def mask(self):
        exprMask = self._expr.mask
        if exprMask == 0:
            return 0
        offset = self._offset
        if offset < _SHIFT_LIMIT_BITS:
            return exprMask << offset
        else:
            return -1 << _SHIFT_LIMIT_BITS

class RShift(SingleExpression):
    '''Drops the lower N bits from a bit string.
    '''
    __slots__ = ('_offset', '_mask')

    offset = property(lambda self: self._offset)

    def __init__(self, expr, offset):
        SingleExpression.__init__(self, expr)
        self._offset = checkType(offset, int, 'shift offset')
        if offset < 0:
            raise ValueError('negative shift count')

    def _ctorargs(self):
        return self._expr, self._offset

    def __str__(self):
        if self.mask == 1:
            return '%s[%d]' % (self._expr, self._offset)
        else:
            return '%s[%d:]' % (self._expr, self._offset)

    def _equals(self, other):
        return self._offset == other._offset and super()._equals(other)

    @property
    def complexity(self):
        return 1 + self._expr.complexity

    @const_property
    def mask(self):
        return self._expr.mask >> self._offset

class LVShift(Expression):
    '''Shifts a bit string to the left, appending zero bits at the end.
    Unlike LShift, our offset is an expression.
    '''
    __slots__ = ('_expr', '_offset', '_mask')

    expr = property(lambda self: self._expr)
    offset = property(lambda self: self._offset)

    def __init__(self, expr, offset):
        Expression.__init__(self)
        self._expr = checkType(expr, Expression, 'subexpression')
        self._offset = checkType(offset, Expression, 'offset')

    @property
    def complexity(self):
        return 1 + self._expr.complexity + self._offset.complexity

    @const_property
    def mask(self):
        exprMask = self._expr.mask
        if exprMask == 0:
            return 0
        offsetMask = self._offset.mask
        width = widthForMask(offsetMask if offsetMask >= 0 else ~offsetMask)
        mask = exprMask
        for i in range(width):
            if 1 << i >= _SHIFT_LIMIT_BITS:
                mask |= -1 << (1 << i)
                break
            if (offsetMask >> i) & 1:
                mask |= mask << (1 << i)
        return mask if offsetMask >= 0 else \
                mask | (-1 << ((1 << width) + trailingZeroes(exprMask)))

    def _ctorargs(self):
        return self._expr, self._offset

    def __str__(self):
        return '(%s << %s)' % (self._expr, self._offset)

    def _equals(self, other):
        return self._offset == other._offset and self._expr == other._expr

class RVShift(Expression):
    '''Drops the lower N bits from a bit string.
    Unlike RShift, our offset (N) is an expression.
    '''
    __slots__ = ('_expr', '_offset', '_mask')

    expr = property(lambda self: self._expr)
    offset = property(lambda self: self._offset)

    def __init__(self, expr, offset):
        Expression.__init__(self)
        self._expr = checkType(expr, Expression, 'subexpression')
        self._offset = checkType(offset, Expression, 'offset')

    @property
    def complexity(self):
        return 1 + self._expr.complexity + self._offset.complexity

    @const_property
    def mask(self):
        exprMask = self._expr.mask
        offsetMask = self._offset.mask
        if offsetMask >= 0:
            # There is a limited number of possible offsets.
            offsetWidth = widthForMask(offsetMask)
        elif exprMask >= 0:
            # There is an unlimited number of possible offsets, but only
            # offsets below the width of the expression mask contribute.
            exprWidth = widthForMask(exprMask)
            offsetWidth = widthForMask(exprWidth)
        else:
            # There is no value we can rule out.
            assert exprMask < 0 and offsetMask < 0, self
            return -1
        mask = exprMask
        for i in range(offsetWidth):
            if (offsetMask >> i) & 1:
                mask |= mask >> (1 << i)
        return mask

    def _ctorargs(self):
        return self._expr, self._offset

    def __str__(self):
        return '(%s >> %s)' % (self._expr, self._offset)

    def _equals(self, other):
        return self._offset == other._offset and self._expr == other._expr

def truncate(expr, width):
    return AndOperator(expr, IntLiteral(maskForWidth(width)))

def optSlice(expr, index, width):
    '''Return a slice of the given expression, at the given index with the given
    width, without adding any unnecessary operations.
    '''
    if index != 0:
        expr = RShift(expr, index)
    mask = expr.mask
    if mask & maskForWidth(width) != mask:
        expr = truncate(expr, width)
    return expr
