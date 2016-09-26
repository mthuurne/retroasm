from retroasm.expression import Expression, LShift, OrOperator
from retroasm.types import IntType

from inspect import signature

class TestValue(Expression):
    __slots__ = ('_name', '_type')

    name = property(lambda self: self._name)
    mask = property(lambda self: self._type.mask)

    def __init__(self, name, typ):
        if not isinstance(name, str):
            raise TypeError('name must be string, got %s' % type(name).__name__)
        if not isinstance(typ, IntType):
            raise TypeError('typ must be IntType, got %s' % type(type).__name__)
        Expression.__init__(self)
        self._name = name
        self._type = typ

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
