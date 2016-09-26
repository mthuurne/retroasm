from weakref import WeakValueDictionary

class Unique(type):
    '''Metaclass that enforces that for each combination of arguments there
    is only one instance.
    Arguments must be of hashable types.
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

def checkType(obj, typ, desc):
    '''Checks whether the given object is of the given type(s).
    If it is, the object is returned unchanged, otherwise TypeError is raised,
    with the "desc" argument used to describe the object in the error message.
    '''
    if isinstance(obj, typ):
        return obj
    else:
        if isinstance(typ, type):
            good = typ.__name__
        else:
            good = ' or '.join(goodType.__name__ for goodType in typ)
        actual = type(obj).__name__
        raise TypeError('%s must be %s, got %s' % (desc, good, actual))
