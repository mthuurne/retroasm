from weakref import WeakValueDictionary

class Unique(type):
    '''Metaclass that enforces that for each combination of arguments there
    is only one instance.
    Keyword constructor arguments are not supported.
    Weak references are used to keep track of instances, so if you define
    __slots__ in your class, make sure you include '__weakref__' in __slots__.
    '''

    def __init__(cls, name, bases, nmspc):
        type.__init__(cls, name, bases, nmspc)
        # pylint: disable=abstract-class-instantiated
        cls.__cache = WeakValueDictionary()

    def __call__(cls, *args):
        cache = cls.__cache
        value = cache.get(args)
        if value is None:
            value = super().__call__(*args)
            cache[args] = value
        return value

class Singleton(type):
    '''Metaclass that enforces that there is one shared instance of a class.
    '''

    def __init__(cls, name, bases, nmspc):
        type.__init__(cls, name, bases, nmspc)
        cls.__instance = None

    def __call__(cls):
        instance = cls.__instance
        if instance is None:
            instance = super().__call__()
            cls.__instance = instance
        return instance
