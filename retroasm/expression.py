from weakref import WeakValueDictionary
import re

class Unique(type):
    '''Metaclass that enforces that for each combination of arguments there
    is only one instance.
    Keyword constructor arguments are not supported.
    Weak references are used to keep track of instances, so if you define
    __slots__ in your class, make sure you include '__weakref__' in __slots__.
    '''

    def __init__(cls, name, bases, dict):
        type.__init__(cls, name, bases, dict)
        cls.__cache = WeakValueDictionary()

    def __call__(cls, *args):
        cache = cls.__cache
        value = cache.get(args)
        if value is None:
            value = super().__call__(*args)
            cache[args] = value
        return value

class IntType(metaclass=Unique):
    '''An integer value type of "width" bits.
    Width can be None, which indicates an unlimited width integer type.
    There is at most one instance of IntType for each width, so instances can
    be compared using the "is" operator.
    '''
    __slots__ = ('_width', '__weakref__')

    width = property(lambda self: self._width)

    def __init__(self, width):
        if width is not None:
            if not isinstance(width, int):
                raise TypeError(
                    'width must be integer or None, got %s' % type(width)
                    )
            if width < 0:
                raise ValueError('width must not be negative: %d' % width)
        self._width = width

    def __repr__(self):
        return 'IntType(%s)' % self._width

    def __str__(self):
        return 'int' if self._width is None else 'u%d' % self._width

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
                'expected IOChannel subclass, got %s' % type(channel)
                )
        return channel

    def __init__(self, name, elemType, addrType):
        if not isinstance(name, str):
            raise TypeError('name must be string, got %s' % type(name))
        if not isinstance(elemType, IntType):
            raise TypeError(
                'element type must be IntType, got %s' % type(elemType))
        if not isinstance(addrType, IntType):
            raise TypeError(
                'address type must be IntType, got %s' % type(addrType))
        self._name = name
        self._elemType = elemType
        self._addrType = addrType

    def __repr__(self):
        return 'IOChannel(%s, %s, %s)' % (
            repr(self._name), repr(self._elemType), repr(self._addrType)
            )

    def __str__(self):
        return '%s %s[%s]' % (self._name, self._elemType, self._addrType)

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
    '''
    __slots__ = ('_type',)

    type = property(lambda self: self._type)
    width = property(lambda self: self._type._width)

    @staticmethod
    def checkInstance(expr):
        if not isinstance(expr, Expression):
            raise TypeError('expected Expression subclass, got %s' % type(expr))
        return expr

    def __init__(self, intType):
        if not isinstance(intType, IntType):
            raise TypeError('type must be IntType, got %s' % type(intType))
        self._type = intType

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

    def _complexity(self):
        '''Returns a postive number that reflects the complexity of this
        expression: the higher the number, the more complex the expression.
        This is used to compare simplification candidates.
        '''
        raise NotImplementedError

    def simplify(self):
        '''Returns an equivalent expression that is simpler (fewer nodes),
        or this expression object itself if no simplification was found.
        '''
        return self

class IntLiteral(Expression):
    '''An integer literal.
    '''
    __slots__ = ('_value',)

    value = property(lambda self: self._value)

    @classmethod
    def create(cls, value):
        '''Returns an unlimited-width integer literal with the given value.
        '''
        return cls(value, IntType(None))

    def __init__(self, value, intType):
        if not isinstance(value, int):
            raise TypeError('value must be int, got %s' % type(value))
        Expression.__init__(self, intType)
        if intType.width is not None:
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

    def __repr__(self):
        return 'IntLiteral(%d, %s)' % (self._value, repr(self._type))

    def __str__(self):
        width = self.width
        if width is None:
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
        return 1

namePat = r"[A-Za-z_][A-Za-z0-9_]*'?"
reName = re.compile(namePat + '$')

class NamedValue(Expression):
    '''Base class for named values that exist in a global or local context.
    '''
    __slots__ = ('_name',)

    name = property(lambda self: self._name)

    def __init__(self, name, typ):
        if not isinstance(name, str):
            raise TypeError('name must be string, got %s' % type(name))
        if not reName.match(name):
            raise ValueError('invalid name: "%s"', name)
        Expression.__init__(self, typ)
        self._name = name

    def __repr__(self):
        return '%s(%s, %s)' % (
            self.__class__.__name__, repr(self._name), repr(self._type)
            )

    def __str__(self):
        return self._name

    def _equals(self, other):
        # There must be one only instance of a class for each name.
        if self._name == other._name:
            assert self is other
            return True
        else:
            return False

    def _complexity(self):
        return 2

class LocalValue(NamedValue):
    '''A variable in the local context.
    '''
    __slots__ = ()

class Reference:
    '''A reference to a global storage location.
    '''
    __slots__ = ()

class LocalReference(NamedValue, Reference):
    '''A local reference to a global storage location.
    '''
    __slots__ = ()

class Register(NamedValue, Reference):
    '''A CPU register.
    '''
    __slots__ = ()

class IOReference(Expression, Reference):
    '''Reference to a particular index on an I/O channel.
    '''
    __slots__ = ('_channel', '_index')

    def __init__(self, channel, index):
        self._channel = IOChannel.checkInstance(channel)
        self._index = Expression.checkInstance(index)
        Expression.__init__(self, self._channel.elemType)

    def __str__(self):
        return '%s[%s]' % (self._channel.name, self._index)

    def __repr__(self):
        return 'IOReference(%s, %s)' % (repr(self._channel), repr(self._index))

    def _equals(self, other):
        return self._channel is other._channel and self._index == other._index

    def _complexity(self):
        return 4 + self._index._complexity()

class ComposedExpression(Expression):
    '''Base class for expressions that combine multiple subexpressions.
    '''
    __slots__ = ('_exprs',)
    operator = property()
    associative = property()
    commutative = property()
    identity = property()
    emptySubstitute = property(lambda self: self.identity)

    exprs = property(lambda self: self._exprs)

    def __init__(self, exprs, intType=IntType(None)):
        self._exprs = tuple(Expression.checkInstance(expr) for expr in exprs)
        Expression.__init__(self, intType)

    def __repr__(self):
        return '%s(%s)' % (
            self.__class__.__name__,
            ', '.join(repr(expr) for expr in self._exprs)
            )

    def __str__(self):
        sep = ' %s ' % self.operator
        return '(%s)' % sep.join(str(expr) for expr in self._exprs)

    def _equals(self, other):
        return len(self._exprs) == len(other._exprs) and all(
            myExpr == otherExpr
            for (myExpr, otherExpr) in zip(self._exprs, other._exprs)
            )

    def _complexity(self):
        return 1 + sum(expr._complexity() for expr in self._exprs)

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
            n = len(exprs)
            i = 0
            while i < n:
                expr = exprs[i]
                if isinstance(expr, IntLiteral):
                    del exprs[i]
                    exprs.append(expr)
                    n -= 1
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
                    exprs[i-1:i+1] = [expr]

        identity = self.identity
        if identity is not None:
            # Remove identity values.
            exprs = [expr for expr in exprs if expr != identity]

        self._customSimplify(exprs)

        if len(exprs) == 0:
            substitute = self.emptySubstitute
            assert substitute is not None
            return substitute
        elif len(exprs) == 1:
            return exprs[0]
        elif len(exprs) == len(self._exprs) \
                and all(new is old for new, old in zip(exprs, self._exprs)):
            return self
        else:
            return self.__class__(*exprs)

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

class AddOperator(ComposedExpression):
    operator = '+'
    associative = True
    commutative = True
    identity = IntLiteral.create(0)

    def __init__(self, *exprs):
        ComposedExpression.__init__(self, exprs)

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

class SubOperator(ComposedExpression):
    operator = '-'
    associative = False
    commutative = False
    identity = None # 0 is a right-identity only

    def __init__(self, *exprs):
        ComposedExpression.__init__(self, exprs)

    def simplify(self):
        # Convert subtraction into addition of the complement, since the
        # associativity and commutativity of addition allow for more
        # simplifications.
        return AddOperator(
            self._exprs[0], *[Complement(expr) for expr in self._exprs[1:]]
            ).simplify()

class Complement(Expression):
    __slots__ = ('_expr',)

    expr = property(lambda self: self._expr)

    def __init__(self, expr):
        self._expr = Expression.checkInstance(expr)
        Expression.__init__(self, IntType(None))

    def __repr__(self):
        return 'Complement(%s)' % repr(self._expr)

    def __str__(self):
        return '-%s' % self._expr

    def _equals(self, other):
        return self._expr == other._expr

    def simplify(self):
        expr = self._expr.simplify()
        if isinstance(expr, IntLiteral):
            return IntLiteral.create(-expr.value)
        elif isinstance(expr, Complement):
            return expr._expr
        elif isinstance(expr, AddOperator):
            # Distribute complement over addition terms:
            #   -(x + y + z) = -x + -y + -z
            return AddOperator(
                *(Complement(term) for term in expr._exprs)
                ).simplify()
        else:
            return self

class Concatenation(ComposedExpression):
    '''Combines several expressions into one by concatenating their bit strings.
    '''
    operator = ';'
    associative = True
    commutative = False
    # Actually, the empty bitstring is the identity element for concatenation,
    # but there is no easy way to express that: a u0 typed literal is not an
    # option since it is considered equal to zero literals of other widths.
    identity = None
    emptySubstitute = IntLiteral(0, IntType(0))

    def __init__(self, *exprs):
        for n, expr in enumerate(exprs[1:], 2):
            if Expression.checkInstance(expr).width is None:
                raise ValueError(
                    'all concatenation operands except the first must have '
                    'a fixed width; operand %d has unlimited width' % n
                    )
        if Expression.checkInstance(exprs[0]).width is None:
            width = None
        else:
            width = sum(expr.width for expr in exprs)
        ComposedExpression.__init__(self, exprs, IntType(width))

    def _equals(self, other):
        return super()._equals(other) and all(
            myExpr.width == otherExpr.width
            for (myExpr, otherExpr) in zip(self._exprs[1:], other._exprs[1:])
            )

    def _combineLiterals(self, literal1, literal2):
        width1 = literal1.width
        width2 = literal2.width
        assert width2 is not None, literal2
        if width1 is None:
            return IntLiteral.create(literal1.value << width2 | literal2.value)
        else:
            return IntLiteral(
                literal1.value << width2 | literal2.value,
                IntType(width1 + width2)
                )

    def _customSimplify(self, exprs):
        # Remove zero-width expressions.
        i = len(exprs)
        while i != 0:
            i -= 1
            if exprs[i].width == 0:
                del exprs[i]

class Slice(Expression):
    '''Extracts a region from a bit string.
    '''
    __slots__ = ('_expr', '_index')

    expr = property(lambda self: self._expr)
    index = property(lambda self: self._index)

    def __init__(self, expr, index, width):
        self._expr = Expression.checkInstance(expr)
        if not isinstance(index, int):
            raise TypeError('slice index must be int, got %s' % type(index))
        if index < 0:
            raise ValueError('slice index must not be negative: %d' % index)
        self._index = index
        Expression.__init__(self, IntType(width))

    def __str__(self):
        if self.width == 1:
            return '%s[%s]' % (self._expr, self._index)
        else:
            return '%s[%s:%s]' % (
                self._expr, self._index, self._index + self.width
                )

    def __repr__(self):
        return 'Slice(%s, %d, %d)' % (repr(self._expr), self._index, self.width)

    def _equals(self, other):
        return (self.width == other.width
            and self._index == other._index
            and self._expr == other._expr)

    def _complexity(self):
        return 2 + self._expr._complexity()

    def simplify(self):
        width = self.width
        if width == 0:
            # This slice is an empty bitstring.
            return IntLiteral(0, self._type)

        # If we're slicing beyond the subexpression's width, reduce the width
        # of the slice and prepend leading zeroes.
        leadingZeroes = 0
        index = self._index
        ewidth = self._expr.width
        if ewidth is not None:
            if index + width > ewidth:
                if index >= ewidth:
                    # This slice contains nothing but leading zeroes.
                    return IntLiteral(0, self._type)
                leadingZeroes = index + width - ewidth
                width -= leadingZeroes
                assert width > 0, width
                assert index + width <= ewidth, self
            elif width == ewidth:
                # This slice spans exactly the expression being sliced.
                assert index == 0, self
                return self._expr.simplify()

        expr = self._expr.simplify()
        assert ewidth == expr.width, (self._expr, expr)

        if isinstance(expr, IntLiteral):
            return IntLiteral(
                (expr.value >> index) & ((1 << width) - 1),
                self._type
                )
        elif isinstance(expr, Slice):
            # Combine both slice operations into a single slice.
            index += expr._index
            expr = expr._expr
        elif isinstance(expr, Concatenation):
            exprs = list(expr.exprs)
            # Remove subexpressions that are entirely below slice range.
            i = len(exprs) - 1
            while i != 0:
                subWidth = exprs[i].width
                if subWidth > index:
                    break
                index -= subWidth
                i -= 1
            del exprs[i+1:]
            # Skip subexpressions that are inside slice range.
            offset = -index
            while i != 0 and offset < width:
                offset += exprs[i].width
                i -= 1
            # Remove subexpressions that are entirely above slice range.
            if offset >= width:
                del exprs[:i+1]
            # Distribute slicing over concatenation subexpressions.
            if len(exprs) == 0:
                assert False, self
            elif len(exprs) == 1:
                exprs[0] = Slice(exprs[0], index, width)
            else:
                if index != 0:
                    exprs[-1] = Slice(exprs[-1], index, exprs[-1].width - index)
                if offset < width:
                    # The original head (most significant) remains.
                    newHeadWidth = width - offset
                    if exprs[0].width is None or exprs[0].width > newHeadWidth:
                        # Head value must be sliced.
                        exprs[0] = Slice(exprs[0], 0, newHeadWidth)
                else:
                    # The original head (most significant) was cut off.
                    if offset > width:
                        exprs[0] = Slice(
                            exprs[0], 0, exprs[0].width - (offset - width)
                            )
            if leadingZeroes != 0:
                exprs.insert(0, IntLiteral(0, IntType(leadingZeroes)))
            return Concatenation(*exprs).simplify()
        elif isinstance(expr, AddOperator):
            if index == 0:
                # Distribute slicing over terms.
                assert leadingZeroes == 0, self # since ewidth is None
                alt = AddOperator(
                    *(Slice(term, 0, width) for term in expr.exprs)
                    ).simplify()
                if alt._complexity() < expr._complexity():
                    return Slice(alt, 0, width)

        if leadingZeroes != 0:
            return Concatenation(
                IntLiteral(0, IntType(leadingZeroes)),
                Slice(expr, index, width)
                ).simplify()
        elif expr is self._expr:
            return self
        else:
            return Slice(expr, index, width)
