from .types import (
    maskForWidth, maskToSegments, trailingZeroes, unlimited, widthForMask
    )
from .utils import checkType

from functools import reduce
from inspect import signature
from itertools import chain

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

    @staticmethod
    def checkInstance(expr):
        if not isinstance(expr, Expression):
            raise TypeError(
                'expected Expression subclass, got %s' % type(expr).__name__
                )
        return expr

    @staticmethod
    def checkScalar(expr):
        Expression.checkInstance(expr)
        expr._checkScalar()
        return expr

    def _ctorargs(self, *exprs, **kwargs):
        '''Returns the constructor arguments that can be used to re-create
        this expression. Any arguments passed to this method will acts as
        overrides. The returned value must be a BoundArguments instance.
        '''
        raise NotImplementedError

    def __repr__(self):
        cls = self.__class__
        def formatArgs():
            ctorSignature = signature(cls)
            binding = self._ctorargs()
            for arg in binding.args:
                yield repr(arg)
            for name, value in binding.kwargs.items():
                param = ctorSignature.parameters.get(name)
                if param is None or param.default != value:
                    yield '%s=%s' % (name, repr(value))
        return '%s(%s)' % (cls.__name__, ', '.join(formatArgs()))

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

    def _equals(self, other):
        '''Returns True if this expression is equal to the other expression,
        False otherwise.
        The other expression is of the same Python class as this one.
        '''
        raise NotImplementedError

    def _checkScalar(self):
        '''Does nothing if this expression returns a single value.
        Otherwise, ValueError is raised.
        '''
        pass

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

        binding = self._ctorargs()
        changed = False
        for name, value in binding.arguments.items():
            if isinstance(value, tuple):
                substs = []
                seqChanged = False
                for expr in value:
                    subst = expr.substitute(func)
                    seqChanged |= subst is not expr
                    substs.append(subst)
                if seqChanged:
                    binding.arguments[name] = tuple(substs)
                    changed = True
            elif isinstance(value, Expression):
                subst = value.substitute(func)
                if subst is not value:
                    binding.arguments[name] = subst
                    changed = True
        if changed:
            return self.__class__(*binding.args, **binding.kwargs)
        else:
            return self

class IntLiteral(Expression):
    '''An integer literal.
    '''
    __slots__ = ('_value',)

    value = property(lambda self: self._value)
    mask = property(lambda self: self._value)

    def __init__(self, value):
        self._value = checkType(value, int, 'value')
        Expression.__init__(self)

    def _ctorargs(self, *exprs, **kwargs):
        cls = self.__class__
        if exprs:
            raise ValueError('%s does not take expression args' % cls.__name__)
        kwargs.setdefault('value', self._value)
        return signature(cls).bind(**kwargs)

    def __str__(self):
        value = self._value
        if value < 10: # small, zero or negative -> print as decimal
            return str(self._value)
        else: # print as hexadecimal
            return '$%X' % value

    def _equals(self, other):
        return self._value == other._value

class ComposedExpression(Expression):
    '''Base class for expressions that combine multiple subexpressions.
    '''
    __slots__ = ('_exprs', '_mask')
    operator = property()

    exprs = property(lambda self: self._exprs)
    mask = property(lambda self: self._mask)

    def __init__(self, *exprs):
        if not exprs:
            raise TypeError('one or more subexpressions must be provided')
        for expr in exprs:
            Expression.checkScalar(expr)
        Expression.__init__(self)
        self._exprs = exprs
        self._mask = self.computeMask(exprs)

    def _ctorargs(self, *exprs, **kwargs):
        if not exprs:
            exprs = self._exprs
        return signature(self.__class__).bind(*exprs, **kwargs)

    @classmethod
    def computeMask(cls, exprs):
        '''Returns the bit mask for the composition of the given expressions.
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

class SimplifiableComposedExpression(ComposedExpression):
    '''Base class for composed expressions that can be simplified using
    their algebraic properties.
    '''
    __slots__ = ()
    associative = property()
    commutative = property()
    idempotent = property()
    identity = property()
    absorber = property()
    emptySubstitute = property(lambda self: self.identity)

    nodeComplexity = 1
    '''Contribution of the expression node itself to expression complexity.'''

    @classmethod
    def computeMask(cls, exprs):
        raise NotImplementedError

    # pylint: disable=unused-argument

    @classmethod
    def combineLiterals(cls, literal1, literal2):
        '''Attempt to combine the two given literals into a single expression.
        Returns the new expression if successful, None otherwise.
        The default implementation returns None, subclasses are encouraged
        to override this method.
        '''
        return None

class AndOperator(SimplifiableComposedExpression):
    __slots__ = ('_tryDistributeAndOverOr',)
    operator = '&'
    associative = True
    commutative = True
    idempotent = True
    identity = IntLiteral(-1)
    absorber = IntLiteral(0)

    def __init__(self, *exprs):
        SimplifiableComposedExpression.__init__(self, *exprs)

        # Set this to False to block the simplification attempt.
        self._tryDistributeAndOverOr = True

    def __str__(self):
        exprs = self.exprs
        last = exprs[-1]
        if isinstance(last, IntLiteral):
            value = last.value
            width = widthForMask(value)
            if maskForWidth(width) == value:
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
    def combineLiterals(cls, literal1, literal2):
        return IntLiteral(literal1.value & literal2.value)

class OrOperator(SimplifiableComposedExpression):
    __slots__ = ('_tryDistributeOrOverAnd', )
    operator = '|'
    associative = True
    commutative = True
    idempotent = True
    identity = IntLiteral(0)
    absorber = IntLiteral(-1)

    def __init__(self, *exprs):
        SimplifiableComposedExpression.__init__(self, *exprs)

        # Set this to False to block the simplification attempt.
        self._tryDistributeOrOverAnd = True

    @classmethod
    def computeMask(cls, exprs):
        return reduce(int.__or__, (expr.mask for expr in exprs), 0)

    @classmethod
    def combineLiterals(cls, literal1, literal2):
        return IntLiteral(literal1.value | literal2.value)

class XorOperator(SimplifiableComposedExpression):
    __slots__ = ()
    operator = '^'
    associative = True
    commutative = True
    idempotent = False
    identity = IntLiteral(0)
    absorber = None

    @classmethod
    def computeMask(cls, exprs):
        # Note: OR not XOR, since we don't know whether a bit is set in an
        #       even or an odd number of subexpressions.
        return reduce(int.__or__, (expr.mask for expr in exprs), 0)

    @classmethod
    def combineLiterals(cls, literal1, literal2):
        return IntLiteral(literal1.value ^ literal2.value)

class AddOperator(SimplifiableComposedExpression):
    __slots__ = ()
    operator = '+'
    associative = True
    commutative = True
    idempotent = False
    identity = IntLiteral(0)
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
    def combineLiterals(cls, literal1, literal2):
        return IntLiteral(literal1.value + literal2.value)

    def __str__(self):
        exprs = self._exprs
        fragments = [str(exprs[0])]
        for expr in exprs[1:]:
            if isinstance(expr, Complement):
                fragments += ('-', str(expr.expr))
            else:
                fragments += ('+', str(expr))
        return '(%s)' % ' '.join(fragments)

class Complement(Expression):
    __slots__ = ('_expr', '_mask')

    expr = property(lambda self: self._expr)
    mask = property(lambda self: self._mask)

    def __init__(self, expr):
        Expression.__init__(self)
        self._expr = Expression.checkScalar(expr)
        exprMask = expr.mask
        self._mask = 0 if exprMask == 0 else -1 << trailingZeroes(exprMask)

    def _ctorargs(self, *exprs, **kwargs):
        if not exprs:
            exprs = (self._expr,)
        return signature(self.__class__).bind(*exprs, **kwargs)

    def __str__(self):
        return '-%s' % self._expr

    def _equals(self, other):
        return self._expr == other._expr

class Negation(Expression):
    __slots__ = ('_expr',)

    expr = property(lambda self: self._expr)
    mask = 1

    def __init__(self, expr):
        Expression.__init__(self)
        self._expr = Expression.checkScalar(expr)

    def _ctorargs(self, *exprs, **kwargs):
        if not exprs:
            exprs = (self._expr,)
        return signature(self.__class__).bind(*exprs, **kwargs)

    def __str__(self):
        return '!%s' % self._expr

    def _equals(self, other):
        return self._expr == other._expr

class SignTest(Expression):
    '''Tests the sign of the given expression.
    '''
    __slots__ = ('_expr')

    expr = property(lambda self: self._expr)
    mask = 1

    def __init__(self, expr):
        Expression.__init__(self)
        self._expr = Expression.checkScalar(expr)

    def _ctorargs(self, *exprs, **kwargs):
        if not exprs:
            exprs = (self._expr,)
        return signature(self.__class__).bind(*exprs, **kwargs)

    def __str__(self):
        return 'sign(%s)' % self._expr

    def _equals(self, other):
        return self._expr == other._expr

class SignExtension(Expression):
    '''Extends the sign bit at the front of a given expression.
    '''
    __slots__ = ('_expr', '_width')

    expr = property(lambda self: self._expr)
    width = property(lambda self: self._width)
    mask = property(lambda self: 0 if self._width == 0 else -1)

    def __init__(self, expr, width):
        Expression.__init__(self)
        self._expr = Expression.checkScalar(expr)
        self._width = checkType(width, int, 'width')

    def _ctorargs(self, *exprs, **kwargs):
        if not exprs:
            exprs = (self._expr,)
        kwargs.setdefault('width', self._width)
        return signature(self.__class__).bind(*exprs, **kwargs)

    def __str__(self):
        return 's%d(%s)' % (self._width, self._expr)

    def _equals(self, other):
        return self._expr == other._expr and self._width == other._width

class LShift(Expression):
    '''Shifts a bit string to the left, appending zero bits at the end.
    '''
    __slots__ = ('_expr', '_offset')

    expr = property(lambda self: self._expr)
    offset = property(lambda self: self._offset)
    mask = property(lambda self: self._expr.mask << self._offset)

    def __init__(self, expr, offset):
        Expression.__init__(self)
        self._expr = Expression.checkScalar(expr)
        self._offset = checkType(offset, int, 'shift offset')

    def _ctorargs(self, *exprs, **kwargs):
        if not exprs:
            exprs = (self._expr,)
        kwargs.setdefault('offset', self._offset)
        return signature(self.__class__).bind(*exprs, **kwargs)

    def __str__(self):
        return '(%s << %d)' % (self._expr, self._offset)

    def _equals(self, other):
        return self._offset == other._offset and self._expr == other._expr

class RShift(Expression):
    '''Drops the lower N bits from a bit string.
    '''
    __slots__ = ('_expr', '_offset')

    expr = property(lambda self: self._expr)
    offset = property(lambda self: self._offset)
    mask = property(lambda self: self._expr.mask >> self._offset)

    def __init__(self, expr, offset):
        Expression.__init__(self)
        self._expr = Expression.checkScalar(expr)
        self._offset = checkType(offset, int, 'shift offset')

    def _ctorargs(self, *exprs, **kwargs):
        if not exprs:
            exprs = (self._expr,)
        kwargs.setdefault('offset', self._offset)
        return signature(self.__class__).bind(*exprs, **kwargs)

    def __str__(self):
        if self.mask == 1:
            return '%s[%d]' % (self._expr, self._offset)
        else:
            return '%s[%d:]' % (self._expr, self._offset)

    def _equals(self, other):
        return self._offset == other._offset and self._expr == other._expr

class LVShift(Expression):
    '''Shifts a bit string to the left, appending zero bits at the end.
    Unlike LShift, our offset is an expression.
    '''
    __slots__ = ('_expr', '_offset')

    expr = property(lambda self: self._expr)
    offset = property(lambda self: self._offset)

    def __init__(self, expr, offset):
        Expression.__init__(self)
        self._expr = Expression.checkScalar(expr)
        self._offset = Expression.checkScalar(offset)

    @property
    def mask(self):
        exprMask = self._expr.mask
        if exprMask == 0:
            return 0
        offsetMask = self._offset.mask
        width = widthForMask(offsetMask if offsetMask >= 0 else ~offsetMask)
        mask = exprMask
        for i in range(width):
            if (offsetMask >> i) & 1:
                mask |= mask << (1 << i)
        return mask if offsetMask >= 0 else \
                mask | (-1 << ((1 << width) + trailingZeroes(exprMask)))

    def _ctorargs(self, *exprs, **kwargs):
        if not exprs:
            exprs = (self._expr,)
        kwargs.setdefault('offset', self._offset)
        return signature(self.__class__).bind(*exprs, **kwargs)

    def __str__(self):
        return '(%s << %s)' % (self._expr, self._offset)

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
