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
        elif width % 4 == 0:
            return ('${:0%dX}' % (width // 4)).format(self._value)
        else:
            return ('%%{:0%db}' % width).format(self._value)

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

class BinaryOperator(Expression):
    '''Base class for binary operators.
    '''
    __slots__ = ('_exprs',)
    operator = property()

    def __init__(self, expr1, expr2):
        exprs = (expr1, expr2)
        self._exprs = tuple(Expression.checkInstance(expr) for expr in exprs)
        Expression.__init__(self, IntType(None))

    def __repr__(self):
        return '%s(%s)' % (
            self.__class__.__name__,
            ', '.join(repr(expr) for expr in self._exprs)
            )

    def __str__(self):
        return '(%s %s %s)' % (self._exprs[0], self.operator, self._exprs[1])

class ComposedExpression(Expression):
    '''Base class for expressions that combine multiple subexpressions.
    '''
    __slots__ = ('_exprs',)
    operator = property()
    associative = property()
    commutative = property()
    identity = property()

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
            # Remove literals with the identity value.
            if self.associative and self.commutative:
                # Earlier simplification steps ensure there can be at most
                # one literal term and it will be at the end.
                i = len(exprs) - 1
            else:
                i = 0
            while max(i, 1) < len(exprs):
                expr = exprs[i]
                if isinstance(expr, IntLiteral) and expr.value == identity:
                    del exprs[i]
                else:
                    i += 1

        self._customSimplify(exprs)

        if len(exprs) < 2:
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
    identity = 0

    def __init__(self, *exprs):
        ComposedExpression.__init__(self, exprs)

    def _combineLiterals(self, literal1, literal2):
        return IntLiteral.create(literal1.value + literal2.value)

class SubOperator(BinaryOperator):
    operator = '-'

    def simplify(self):
        expr1, expr2 = self._simplifyExprs(self._exprs)
        isInt1 = isinstance(expr1, IntLiteral)
        isInt2 = isinstance(expr2, IntLiteral)
        if isInt2 and expr2.value == 0:
            return expr1
        elif isInt1 and isInt2:
            return IntLiteral.create(expr1.value - expr2.value)
        elif expr1 is self._exprs[0] and expr2 is self._exprs[1]:
            return self
        else:
            return SubOperator(expr1, expr2)

class Concatenation(Expression):
    '''Combines several expressions into one by concatenating their bit strings.
    '''
    __slots__ = ('_exprs',)

    def __init__(self, exprs):
        self._exprs = tuple(Expression.checkInstance(expr) for expr in exprs)
        for n, expr in enumerate(self._exprs[1:], 2):
            if expr.width is None:
                raise ValueError(
                    'all concatenation operands except the first must have '
                    'a fixed width; operand %d has unlimited width' % n
                    )
        if self._exprs[0].width is None:
            width = None
        else:
            width = sum(expr.width for expr in self._exprs)
        Expression.__init__(self, IntType(width))

    def __repr__(self):
        return 'Concatenation([%s])' % ', '.join(
            repr(expr) for expr in self._exprs
            )

    def __str__(self):
        return '(%s)' % ' ; '.join(str(expr) for expr in self._exprs)

class Slice(Expression):
    '''Extracts a region from a bit string.
    '''
    __slots__ = ('_expr', '_index')

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
