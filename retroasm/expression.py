from weakref import WeakValueDictionary

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
    There is at most one instance of IntType for each width, so instances can
    be compared using the "is" operator.
    '''
    __slots__ = ('_width', '__weakref__')

    width = property(lambda self: self._width)

    def __init__(self, width):
        if not isinstance(width, int):
            raise TypeError('width should be integer, got %s' % type(width))
        self._width = width

    def __repr__(self):
        return 'IntType(%d)' % self._width

    def __str__(self):
        return 'i%d' % self._width

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
            raise TypeError('type should be IntType, got %s' % type(intType))
        self._type = intType

class IntLiteral(Expression):
    '''An integer literal.
    '''
    __slots__ = ('_value',)

    value = property(lambda self: self._value)

    def __init__(self, value, intType):
        if not isinstance(value, int):
            raise TypeError('value should be int, got %s' % type(value))
        Expression.__init__(self, intType)
        if value < 0:
            raise ValueError(
                'integer literal value must not be negative: %d' % value)
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
        if width % 4 == 0:
            return ('${:0%dX}' % (width // 4)).format(self._value)
        else:
            return ('%%{:0%db}' % width).format(self._value)

class Concatenation(Expression):
    '''Combines several expressions into one by concatenating their bit strings.
    '''
    __slots__ = ('_exprs',)

    def __init__(self, exprs):
        self._exprs = tuple(Expression.checkInstance(expr) for expr in exprs)
        width = sum(expr.width for expr in self._exprs)
        Expression.__init__(self, IntType(width))

    def __repr__(self):
        return 'Concatenation([%s])' % ', '.join(
            repr(expr) for expr in self._exprs
            )

    def __str__(self):
        return '(%s)' % ' ; '.join(str(expr) for expr in self._exprs)
