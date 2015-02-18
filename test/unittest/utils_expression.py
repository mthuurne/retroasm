from retroasm.expression import Expression

from inspect import signature

class TestValue(Expression):
    __slots__ = ('_name',)

    name = property(lambda self: self._name)

    def __init__(self, name, typ):
        if not isinstance(name, str):
            raise TypeError('name must be string, got %s' % type(name).__name__)
        Expression.__init__(self, typ)
        self._name = name

    def _ctorargs(self, *exprs, **kwargs):
        cls = self.__class__
        if exprs:
            raise ValueError('%s does not take expression args' % cls.__name__)
        kwargs.setdefault('name', self._name)
        kwargs.setdefault('typ', self._type)
        return signature(cls).bind(**kwargs)

    def __str__(self):
        return self._name

    def _equals(self, other):
        return self is other
