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
    __slots__ = ('width', '__weakref__')

    def __init__(self, width):
        if not isinstance(width, int):
            raise TypeError('width should be integer, got %s' % type(width))
        self.width = width

    def __repr__(self):
        return 'IntType(%d)' % self.width

    def __str__(self):
        return 'i%d' % self.width

class Concatenation:

    def __init__(self, exprs):
        self._exprs = list(exprs)
        self.type = IntType(sum(expr.type.width for expr in self._exprs))

    def __repr__(self):
        return 'Concatenation([%s])' % ', '.join(
            repr(expr) for expr in self._exprs
            )

    def __str__(self):
        return '(%s)' % ' ; '.join(str(expr) for expr in self._exprs)
