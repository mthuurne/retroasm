from .expression import Expression
from .types import maskForWidth

from inspect import signature

class Immediate(Expression):
    '''A constant value defined as part of an instruction.
    Note that the name of an immediate is unique only within the mode entry
    that declares, not in mode entries that include it.
    '''
    __slots__ = ('_name', '_type', '_location')

    name = property(lambda self: self._name)
    type = property(lambda self: self._type)
    mask = property(lambda self: maskForWidth(self._type.width))
    location = property(lambda self: self._location)

    def __init__(self, name, typ, location):
        Expression.__init__(self)
        self._name = name
        self._type = typ
        self._location = location

    def _ctorargs(self, *exprs, **kwargs):
        cls = self.__class__
        if exprs:
            raise ValueError('%s does not take expression args' % cls.__name__)
        kwargs.setdefault('name', self._name)
        kwargs.setdefault('typ', self._type)
        kwargs.setdefault('location', self._location)
        return signature(cls).bind(**kwargs)

    def __str__(self):
        return self._name

    def _equals(self, other):
        # pylint: disable=protected-access
        return self._name == other._name

class Mode:

    def __init__(self):
        self._entries = []

    def add(self, encoding, mnemonic, semantics, immediates, includedModes):
        self._entries.append(
            (encoding, mnemonic, semantics, immediates, includedModes)
            )
