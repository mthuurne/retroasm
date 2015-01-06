from .types import IntType, unlimited
from .utils import Singleton

from inspect import signature

# pylint: disable=protected-access

class Expression:
    '''Abstract base class for typed expressions.

    Expressions are considered equal if they have the same tree form.
    This means that for example (A + (B + C)) and ((A + B) + C) are considered
    unequal: they represent the same computation, but not the same tree.

    Since most operators convert their arguments to unlimited width integers,
    the width of an expression is not ignored when determining equality.
    In cases where the width matters, such as concatenation, the operand
    class must check the widths of subexpressions when determining equality
    between two expressions.

    When the parser creates expressions, it follows the typing rules from the
    instruction set description language, which apply most operations on
    unlimited width integers. However, for analysis and code generation it is
    useful to have width reduced as much as possible, so constructors will
    accept narrower types and width reduction is a goal of simplification.
    '''
    __slots__ = ('_type',)

    type = property(lambda self: self._type)

    @staticmethod
    def checkInstance(expr):
        if not isinstance(expr, Expression):
            raise TypeError('expected Expression subclass, got %s' % type(expr))
        return expr

    @staticmethod
    def checkScalar(expr):
        Expression.checkInstance(expr)
        expr._checkScalar()
        return expr

    def __init__(self, typ):
        if typ is not None and not isinstance(typ, IntType):
            raise TypeError('type must be None or IntType, got %s' % type(typ))
        self._type = typ

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

    def _getWidth(self):
        '''Returns the width of this expression in bits (possibly unlimited),
        or None if the expression is not a scalar.
        '''
        typ = self._type
        return None if typ is None else typ._width
    width = property(_getWidth)

    def _complexity(self):
        '''Returns a postive number that reflects the complexity of this
        expression: the higher the number, the more complex the expression.
        This is used to compare simplification candidates.
        '''
        raise NotImplementedError

    def simplify(self):
        '''Returns an equivalent expression that is simpler (fewer nodes),
        or this expression object itself if no simplification was found.
        Simplified expressions can have reduced width.
        '''
        return self

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

class Unit(Expression, metaclass=Singleton):
    '''Expression that represents the absense of a value.
    '''
    __slots__ = ()

    def __init__(self):
        Expression.__init__(self, None)

    def _ctorargs(self, *exprs, **kwargs):
        return signature(self.__class__).bind()

    def __str__(self):
        return 'unit'

    def _equals(self, other):
        return self is other

    def _checkScalar(self):
        '''Does nothing if this expression returns a single value.
        Otherwise, ValueError is raised.
        '''
        raise ValueError('attempt to use unit value')

    def _complexity(self):
        return 1

unit = Unit()

class IntLiteral(Expression):
    '''An integer literal.
    '''
    __slots__ = ('_value',)

    value = property(lambda self: self._value)

    @classmethod
    def create(cls, value):
        '''Returns an unlimited-width integer literal with the given value.
        '''
        return cls(value, IntType(unlimited))

    def __init__(self, value, intType):
        if not isinstance(value, int):
            raise TypeError('value must be int, got %s' % type(value))
        Expression.__init__(self, intType)
        if intType.width is not unlimited:
            if value < 0:
                raise ValueError(
                    'unsigned integer literal value must not be negative: '
                    '%d' % value
                    )
            if value >= 1 << intType.width:
                raise ValueError(
                    'integer literal value %d does not fit in type %s'
                    % (value, intType)
                    )
        self._value = value

    def _ctorargs(self, *exprs, **kwargs):
        cls = self.__class__
        if exprs:
            raise ValueError('%s does not take expression args' % cls.__name__)
        kwargs.setdefault('value', self._value)
        kwargs.setdefault('intType', self._type)
        return signature(cls).bind(**kwargs)

    def __str__(self):
        width = self.width
        if width is unlimited:
            return str(self._value)
        elif width == 0:
            return '0[0:0]'
        elif width % 4 == 0:
            return ('${:0%dX}' % (width // 4)).format(self._value)
        else:
            return ('%%{:0%db}' % width).format(self._value)

    def _equals(self, other):
        return self._value == other._value

    def _complexity(self):
        return 2 if self.width is unlimited else 1

    def simplify(self):
        value = self._value
        if value >= 0:
            valueWidth = value.bit_length()
            if valueWidth < self.width:
                return IntLiteral(value, IntType(valueWidth))
        return self

class ComposedExpression(Expression):
    '''Base class for expressions that combine multiple subexpressions.
    '''
    __slots__ = ('_exprs',)
    operator = property()

    exprs = property(lambda self: self._exprs)

    def __init__(self, *exprs, intType=IntType(unlimited)):
        if not exprs:
            raise TypeError('one or more subexpressions must be provided')
        for expr in exprs:
            Expression.checkScalar(expr)
        Expression.__init__(self, intType)
        self._exprs = exprs

    def _ctorargs(self, *exprs, **kwargs):
        if not exprs:
            exprs = self._exprs
        kwargs.setdefault('intType', self._type)
        return signature(self.__class__).bind(*exprs, **kwargs)

    def __str__(self):
        sep = ' %s ' % self.operator
        return '(%s)' % sep.join(str(expr) for expr in self._exprs)

    def _equals(self, other):
        return len(self._exprs) == len(other._exprs) and all(
            myExpr == otherExpr
            for (myExpr, otherExpr) in zip(self._exprs, other._exprs)
            )

    def _complexity(self):
        raise NotImplementedError

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

    def _complexity(self):
        return self.nodeComplexity + sum(
            expr._complexity() for expr in self._exprs
            )

    def simplify(self):
        # Simplify the subexpressions individually.
        exprs = [expr.simplify() for expr in self._exprs]

        if self.associative:
            # Merge subexpressions of the same type into this expression.
            i = 0
            while i < len(exprs):
                expr = exprs[i]
                if expr.__class__ is self.__class__:
                    exprs[i:i+1] = expr._exprs
                    i += len(expr._exprs)
                else:
                    i += 1

        if self.associative and self.commutative:
            # Move all literals to the end.
            # This makes the later merge step more effective.
            numExprs = len(exprs)
            i = 0
            while i < numExprs:
                expr = exprs[i]
                if isinstance(expr, IntLiteral):
                    del exprs[i]
                    exprs.append(expr)
                    numExprs -= 1
                else:
                    i += 1

        if self.associative or len(exprs) == 2:
            # Merge literals.
            i = 1
            while i < len(exprs):
                expr1 = exprs[i - 1]
                if not isinstance(expr1, IntLiteral):
                    i += 1
                    continue
                expr2 = exprs[i]
                if not isinstance(expr2, IntLiteral):
                    i += 2
                    continue
                expr = self._combineLiterals(expr1, expr2)
                if expr is None:
                    i += 1
                else:
                    exprs[i-1:i+1] = [expr.simplify()]

        absorber = self.absorber
        if absorber is not None:
            # If any absorber is present, the result is the absorber.
            if any(expr == absorber for expr in exprs):
                return absorber.simplify()

        identity = self.identity
        if identity is not None:
            # Remove identity values.
            exprs = [expr for expr in exprs if expr != identity]

        if self.idempotent:
            # Remove duplicate values.
            numExprs = len(exprs)
            i = 0
            while i + 1 < numExprs:
                expr = exprs[i]
                i += 1
                j = i
                while j < numExprs:
                    if exprs[j] == expr:
                        del exprs[j]
                        numExprs -= 1
                    else:
                        j += 1

        self._customSimplify(exprs)

        if len(exprs) == 0:
            substitute = self.emptySubstitute
            assert substitute is not None
            return substitute.simplify()
        elif len(exprs) == 1:
            return exprs[0]
        elif len(exprs) == len(self._exprs) \
                and all(new is old for new, old in zip(exprs, self._exprs)):
            return self
        else:
            return self.__class__(*exprs)

    # pylint: disable=unused-argument

    def _combineLiterals(self, literal1, literal2):
        '''Attempt to combine the two given literals into a single expression.
        Returns the new expression if successful, None otherwise.
        The default implementation returns None, subclasses are encouraged
        to override this method.
        '''
        return None

    def _customSimplify(self, exprs):
        '''Applies operator-specific simplifications on the given list of
        expressions. This method is called by simplify() after it has performed
        all generic simplifications.
        Nothing is returned, the list is modified instead.
        '''
        pass

class AndOperator(SimplifiableComposedExpression):
    __slots__ = ('_tryDistributeAndOverOr', '_tryMaskToShift')
    operator = '&'
    associative = True
    commutative = True
    idempotent = True
    identity = IntLiteral.create(-1)
    absorber = IntLiteral.create(0)

    def __init__(self, *exprs, intType=IntType(unlimited)):
        SimplifiableComposedExpression.__init__(self, *exprs, intType=intType)

        # Set this to False to block the simplification attempt.
        self._tryDistributeAndOverOr = True
        # Set this to False to block the simplification attempt.
        self._tryMaskToShift = True

    def _combineLiterals(self, literal1, literal2):
        return IntLiteral.create(literal1.value & literal2.value)

    def _customSimplify(self, exprs):
        if not exprs:
            return

        myComplexity = self.nodeComplexity + sum(
            expr._complexity() for expr in exprs
            )

        width = min(expr.width for expr in exprs)
        if width is not unlimited:
            # Try truncating each subexpression to the minimum width.
            changed = width < self.width
            for i, expr in enumerate(exprs):
                trunc = Truncation(expr, width).simplify()
                if trunc._complexity() < expr._complexity():
                    exprs[i] = trunc
                    changed = True
            if changed:
                # Force earlier simplification steps to run again.
                alt = AndOperator(*exprs, intType=IntType(width))
                if not self._tryDistributeAndOverOr:
                    alt._tryDistributeAndOverOr = False
                if not self._tryMaskToShift:
                    alt._tryMaskToShift = False
                exprs[:] = [alt.simplify()]
                return

            last = exprs[-1]
            if isinstance(last, IntLiteral):
                value = last.value
                mask = (1 << width) - 1
                if value & mask == mask:
                    # This bit mask application is essentially truncating;
                    # convert it to an actual Truncation expression.
                    expr = Truncation(AndOperator(*exprs[:-1]), width)
                    exprs[:] = [expr.simplify()]
                    return

                assert value != 0, self
                trailingZeroes = 0
                while (value >> trailingZeroes) & 1 == 0:
                    trailingZeroes += 1
                if trailingZeroes != 0:
                    # Check whether there are any expressions that are fully
                    # consumed by the trailing zeroes.
                    for expr in exprs:
                        width = expr.width
                        if width <= trailingZeroes:
                            exprs[:] = [self.absorber]
                            return
                    if self._tryMaskToShift:
                        clone = AndOperator(*exprs)
                        clone._tryMaskToShift = False
                        alt = LShift(
                            RShift(clone, trailingZeroes),
                            trailingZeroes
                            ).simplify()
                        if alt._complexity() < myComplexity:
                            exprs[:] = [alt]
                            return

        for i, expr in enumerate(exprs):
            if isinstance(expr, OrOperator) and self._tryDistributeAndOverOr:
                # Distribute AND over OR.
                andExprs = exprs[:i] + exprs[i+1:]
                alt = OrOperator(*(
                    AndOperator(term, *andExprs)
                    for term in expr.exprs
                    ))
                alt._tryDistributeOrOverAnd = False
                alt = alt.simplify()
                if alt._complexity() < myComplexity:
                    exprs[:] = [alt]
                    return

class OrOperator(SimplifiableComposedExpression):
    __slots__ = ('_tryDistributeOrOverAnd', )
    operator = '|'
    associative = True
    commutative = True
    idempotent = True
    identity = IntLiteral.create(0)
    absorber = IntLiteral.create(-1)

    def __init__(self, *exprs, intType=IntType(unlimited)):
        SimplifiableComposedExpression.__init__(self, *exprs, intType=intType)

        # Set this to False to block the simplification attempt.
        self._tryDistributeOrOverAnd = True

    def _combineLiterals(self, literal1, literal2):
        return IntLiteral.create(literal1.value | literal2.value)

    def _customSimplify(self, exprs):
        if not exprs:
            return

        # Reduce expression width if possible.
        curWidth = self.width
        width = max(expr.width for expr in exprs)
        if width < curWidth:
            alt = OrOperator(*exprs, intType=IntType(width))
            if not self._tryDistributeOrOverAnd:
                alt._tryDistributeOrOverAnd = False
            exprs[:] = [alt.simplify()]
            return
        else:
            assert width == curWidth, self

        myComplexity = self.nodeComplexity + sum(
            expr._complexity() for expr in exprs
            )
        for i, expr in enumerate(exprs):
            if isinstance(expr, AndOperator) and self._tryDistributeOrOverAnd:
                # Distribute OR over AND.
                orExprs = exprs[:i] + exprs[i+1:]
                alt = AndOperator(*(
                    OrOperator(term, *orExprs)
                    for term in expr.exprs
                    ))
                alt._tryDistributeAndOverOr = False
                alt = alt.simplify()
                if alt._complexity() < myComplexity:
                    exprs[:] = [alt]
                    return

class XorOperator(SimplifiableComposedExpression):
    __slots__ = ()
    operator = '^'
    associative = True
    commutative = True
    idempotent = False
    identity = IntLiteral.create(0)
    absorber = None

    def _combineLiterals(self, literal1, literal2):
        return IntLiteral.create(literal1.value ^ literal2.value)

    def _customSimplify(self, exprs):
        # Remove duplicate expression pairs: A ^ A == 0.
        i = 0
        while i < len(exprs):
            expr = exprs[i]
            try:
                j = exprs.index(expr, i + 1)
            except ValueError:
                i += 1
            else:
                del exprs[j]
                del exprs[i]

        if not exprs:
            return

        # Reduce expression width if possible.
        curWidth = self.width
        width = max(expr.width for expr in exprs)
        if width < curWidth:
            alt = XorOperator(*exprs, intType=IntType(width))
            exprs[:] = [alt.simplify()]
            return
        else:
            assert width == curWidth, self

        # TODO: Distribution over AND and OR.

class AddOperator(SimplifiableComposedExpression):
    __slots__ = ()
    operator = '+'
    associative = True
    commutative = True
    idempotent = False
    identity = IntLiteral.create(0)
    absorber = None

    def __str__(self):
        exprs = self._exprs
        fragments = [str(exprs[0])]
        for expr in exprs[1:]:
            if isinstance(expr, Complement):
                fragments += ('-', str(expr.expr))
            else:
                fragments += ('+', str(expr))
        return '(%s)' % ' '.join(fragments)

    def _combineLiterals(self, literal1, literal2):
        return IntLiteral.create(literal1.value + literal2.value)

    def _customSimplify(self, exprs):
        # Remove pairs of A and -A.
        complIdx = 0
        while complIdx < len(exprs):
            compl = exprs[complIdx]
            if not isinstance(compl, Complement):
                complIdx += 1
                continue
            try:
                idx = exprs.index(compl.expr)
            except ValueError:
                complIdx += 1
            else:
                del exprs[idx]
                if idx < complIdx:
                    complIdx -= 1
                del exprs[complIdx]

class Complement(Expression):
    __slots__ = ('_expr',)

    expr = property(lambda self: self._expr)

    def __init__(self, expr):
        self._expr = Expression.checkScalar(expr)
        Expression.__init__(self, IntType(unlimited))

    def _ctorargs(self, *exprs, **kwargs):
        if not exprs:
            exprs = (self._expr,)
        return signature(self.__class__).bind(*exprs, **kwargs)

    def __str__(self):
        return '-%s' % self._expr

    def _equals(self, other):
        return self._expr == other._expr

    def _complexity(self):
        return 1 + self._expr._complexity()

    def simplify(self):
        expr = self._expr.simplify()
        if isinstance(expr, IntLiteral):
            return IntLiteral.create(-expr.value).simplify()
        elif isinstance(expr, Complement):
            return expr._expr
        elif isinstance(expr, AddOperator):
            # Distribute complement over addition terms:
            #   -(x + y + z) = -x + -y + -z
            return AddOperator(
                *(Complement(term) for term in expr._exprs)
                ).simplify()
        elif expr is self._expr:
            return self
        else:
            return Complement(expr)

class LShift(Expression):
    '''Shifts a bit string to the left, appending zero bits at the end.
    '''
    __slots__ = ('_expr', '_offset')

    expr = property(lambda self: self._expr)
    offset = property(lambda self: self._offset)

    def __init__(self, expr, offset):
        self._expr = Expression.checkScalar(expr)
        if not isinstance(offset, int):
            raise TypeError('shift offset must be int, got %s' % type(offset))
        self._offset = offset
        Expression.__init__(self, IntType(expr.width + offset))

    def _ctorargs(self, *exprs, **kwargs):
        if not exprs:
            exprs = (self._expr,)
        kwargs.setdefault('offset', self._offset)
        return signature(self.__class__).bind(*exprs, **kwargs)

    def __str__(self):
        return '(%s << %d)' % (self._expr, self._offset)

    def _equals(self, other):
        return self._offset == other._offset and self._expr == other._expr

    def _complexity(self):
        return 1 + self._expr._complexity()

    def simplify(self):
        expr = self._expr.simplify()

        offset = self._offset
        if offset == 0:
            # No actual shift occurs.
            return expr

        width = expr.width + offset

        if isinstance(expr, IntLiteral):
            return IntLiteral(expr.value << offset, IntType(width))
        elif isinstance(expr, LShift):
            # Combine both shifts into one.
            return LShift(expr._expr, offset + expr._offset).simplify()
        elif isinstance(expr, RShift):
            roffset = expr.offset
            mask = (0 if expr.width is unlimited else 1 << expr.width) - 1
            masked = AndOperator(expr.expr, IntLiteral.create(mask << roffset))
            masked._tryMaskToShift = False
            if roffset < offset:
                # Left shift wins.
                return LShift(masked, offset - roffset).simplify()
            elif roffset == offset:
                # Left and right shift cancel each other out.
                return masked.simplify()
            else:
                # Right shift wins.
                return RShift(masked, roffset - offset).simplify()
        elif isinstance(expr, (AndOperator, OrOperator)):
            alt = type(expr)(
                *(LShift(term, offset) for term in expr.exprs)
                )
            if not getattr(expr, '_tryMaskToShift', True):
                alt._tryMaskToShift = False
            alt = alt.simplify()
            if alt._complexity() <= self._complexity():
                return alt

        if expr is self._expr:
            return self
        else:
            return LShift(expr, offset)

class RShift(Expression):
    '''Drops the lower N bits from a bit string.
    '''
    __slots__ = ('_expr', '_offset')

    expr = property(lambda self: self._expr)
    offset = property(lambda self: self._offset)

    def __init__(self, expr, offset):
        self._expr = Expression.checkScalar(expr)
        if not isinstance(offset, int):
            raise TypeError('shift offset must be int, got %s' % type(offset))
        self._offset = offset
        Expression.__init__(self, IntType(max(expr.width - offset, 0)))

    def _ctorargs(self, *exprs, **kwargs):
        if not exprs:
            exprs = (self._expr,)
        kwargs.setdefault('offset', self._offset)
        return signature(self.__class__).bind(*exprs, **kwargs)

    def __str__(self):
        return '%s[%d:]' % (self._expr, self._offset)

    def _equals(self, other):
        return self._offset == other._offset and self._expr == other._expr

    def _complexity(self):
        return 1 + self._expr._complexity()

    def simplify(self):
        expr = self._expr.simplify()

        offset = self._offset
        if offset == 0:
            # No actual shift occurs.
            return expr

        width = expr.width - offset
        if width <= 0:
            # Entire subexpression is discarded by the shift.
            return IntLiteral.create(0)

        if isinstance(expr, IntLiteral):
            return IntLiteral(expr.value >> offset, IntType(width))
        elif isinstance(expr, LShift):
            loffset = expr.offset
            if loffset < offset:
                # Right shift wins.
                return RShift(expr.expr, offset - loffset).simplify()
            elif loffset == offset:
                # Left and right shift cancel each other out.
                return expr.expr
            else:
                # Left shift wins.
                return LShift(expr.expr, loffset - offset).simplify()
        elif isinstance(expr, RShift):
            # Combine both shifts into one.
            return RShift(expr._expr, offset + expr._offset).simplify()
        elif isinstance(expr, Truncation):
            # Truncate after shifting: this maps better to the slice semantics.
            return Truncation(RShift(expr.expr, offset), width).simplify()
        elif isinstance(expr, (AndOperator, OrOperator)):
            alt = type(expr)(
                *(RShift(term, offset) for term in expr.exprs)
                )
            if not getattr(expr, '_tryMaskToShift', True):
                alt._tryMaskToShift = False
            alt = alt.simplify()
            if alt._complexity() < self._complexity():
                return alt

        if expr is self._expr:
            return self
        else:
            return RShift(expr, offset)

class Truncation(Expression):
    '''Extracts the lower N bits from a bit string.
    '''
    __slots__ = ('_expr', )

    expr = property(lambda self: self._expr)

    def __init__(self, expr, width):
        self._expr = Expression.checkScalar(expr)
        Expression.__init__(self, IntType(width))

    def _ctorargs(self, *exprs, **kwargs):
        if not exprs:
            exprs = (self._expr,)
        kwargs.setdefault('width', self.width)
        return signature(self.__class__).bind(*exprs, **kwargs)

    def __str__(self):
        expr = self._expr
        if isinstance(expr, RShift):
            offset = expr.offset
            return '%s[%d:%d]' % (expr.expr, offset, offset + self.width)
        else:
            return '%s[:%d]' % (self._expr, self.width)

    def _equals(self, other):
        return self.width == other.width and self._expr == other._expr

    def _complexity(self):
        return 1 + self._expr._complexity()

    def simplify(self):
        width = self.width
        assert width is not unlimited, self
        if width == 0:
            # Every zero-width expression is equivalent to an empty bitstring.
            return IntLiteral(0, IntType(0))

        # Note that simplification can reduce the width of the subexpression,
        # so do subexpression simplification before checking the width.
        expr = self._expr.simplify()

        # If we're truncating beyond the subexpression's width, reduce the
        # truncation width.
        if expr.width <= width:
            # The subexpression already fits: no truncation needed.
            return expr

        if isinstance(expr, IntLiteral):
            return IntLiteral.create(expr.value & ((1 << width) - 1)).simplify()
        elif isinstance(expr, LShift):
            offset = expr.offset
            if offset >= width:
                # Result contains nothing but trailing zeroes.
                return IntLiteral(0, IntType(0))
            else:
                # Truncate before left-shifting.
                trunc = Truncation(expr.expr, width - offset)
                return LShift(trunc, offset).simplify()
        elif isinstance(expr, RShift):
            subExpr = expr.expr
            offset = expr.offset
            alt = Truncation(subExpr, width + offset).simplify()
            if alt._complexity() < subExpr._complexity():
                return Truncation(RShift(alt, offset), width).simplify()
        elif isinstance(expr, Truncation):
            # Combine both truncations into one.
            return Truncation(expr._expr, width).simplify()
        elif isinstance(expr, (AndOperator, OrOperator)):
            alt = type(expr)(
                *(Truncation(term, width) for term in expr.exprs)
                ).simplify()
            if alt._complexity() < expr._complexity():
                return Truncation(alt, width).simplify()
        elif isinstance(expr, AddOperator):
            # Eliminate inner truncations that are not narrower than the outer
            # trunctation.
            terms = []
            changed = False
            for term in expr.exprs:
                if isinstance(term, Truncation) and term.width >= width:
                    terms.append(term._expr)
                    changed = True
                else:
                    terms.append(term)
            if changed:
                return Truncation(AddOperator(*terms), width).simplify()
            # Distribute truncation over terms.
            alt = AddOperator(
                *(Truncation(term, width) for term in expr.exprs)
                ).simplify()
            if alt._complexity() < expr._complexity():
                return Truncation(alt, width)
        elif isinstance(expr, Complement):
            # Apply truncation to subexpr.
            alt = Complement(Truncation(expr.expr, width)).simplify()
            if alt._complexity() < expr._complexity():
                return Truncation(alt, width)

        if expr is self._expr:
            return self
        else:
            return Truncation(expr, width)

class PlaceholderExpression:
    '''Mixin for expressions that contain parse results in an accessible form,
    but will be replaced by instances of a different class on simplification.
    '''
    __slots__ = ()

    def _complexity(self):
        # Encourage replacement of this expression in conditional
        # simplification.
        return 1 << 50

    def _convert(self):
        '''Returns an equivalent expression that can be simplified.
        '''
        raise NotImplementedError

    def simplify(self):
        return self._convert().simplify()

class Concatenation(PlaceholderExpression, ComposedExpression):
    '''Expression that concatenates bit strings.
    '''
    __slots__ = ()
    operator = ';'

    def __init__(self, expr1, *exprs):
        Expression.checkScalar(expr1)
        for i, expr in enumerate(exprs, 2):
            if Expression.checkScalar(expr).width is unlimited:
                raise ValueError(
                    'all concatenation operands except the first must have '
                    'a fixed width; operand %d has unlimited width' % i
                    )
        width = expr1.width + sum(expr.width for expr in exprs)
        ComposedExpression.__init__(self, expr1, *exprs, intType=IntType(width))

    def _ctorargs(self, *exprs, **kwargs):
        if not exprs:
            exprs = self._exprs
        return signature(self.__class__).bind(*exprs, **kwargs)

    def _equals(self, other):
        return super()._equals(other) and all(
            myExpr.width == otherExpr.width
            for (myExpr, otherExpr) in zip(self._exprs[1:], other._exprs[1:])
            )

    def iterWithOffset(self):
        '''Iterates through the concatenated expressions, where each element
        is a pair of the expression and its offset in the concatenation.
        '''
        exprs = self._exprs
        offset = 0
        for expr in reversed(exprs[1:]):
            yield expr, offset
            offset += expr.width
        yield exprs[0], offset

    def _convert(self):
        return OrOperator(
            *(LShift(*eo) for eo in self.iterWithOffset()),
            intType=self._type
            )

class Slice(PlaceholderExpression, Expression):
    '''Creates an expression that extracts a region from a bit string.
    '''
    __slots__ = ('_expr', '_index')

    expr = property(lambda self: self._expr)
    index = property(lambda self: self._index)

    def __init__(self, expr, index, width):
        self._expr = Expression.checkScalar(expr)
        if not isinstance(index, int):
            raise TypeError('slice index must be int, got %s' % type(index))
        if index < 0:
            raise ValueError('slice index must not be negative: %d' % index)
        self._index = index
        Expression.__init__(self, IntType(width))

    def _ctorargs(self, *exprs, **kwargs):
        if not exprs:
            exprs = (self._expr,)
        kwargs.setdefault('index', self._index)
        kwargs.setdefault('width', self.width)
        return signature(self.__class__).bind(*exprs, **kwargs)

    def __str__(self):
        if self.width == 1:
            return '%s[%d]' % (self._expr, self._index)
        else:
            return '%s[%d:%d]' % (
                self._expr, self._index, self._index + self.width
                )

    def _equals(self, other):
        return (self.width == other.width and
                self._index == other._index and
                self._expr == other._expr)

    def _convert(self):
        expr = self._expr
        index = self._index
        if index != 0:
            expr = RShift(expr, index)
        width = self.width
        if width != expr.width:
            expr = Truncation(expr, width)
        return expr
