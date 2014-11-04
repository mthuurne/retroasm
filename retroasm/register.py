from .expression import Expression

import re

reRegName = re.compile('[a-z][a-z0-9]*\'?$')

class Register(Expression):
    '''A CPU register.
    '''
    __slots__ = ('name',)

    def __init__(self, name, typ):
        if not isinstance(name, str):
            raise TypeError('name should be integer, got %s' % type(name))
        if not reRegName.match(name):
            raise ValueError('invalid register name: "%s"', name)

        Expression.__init__(self, typ)
        self.name = name

    def __str__(self):
        return self.name

    def __repr__(self):
        return 'Register(%s, %s)' % (repr(self.name), repr(self._type))

    def __eq__(self, other):
        if isinstance(other, Register):
            return self.name == other.name and self._type == other._type
        else:
            return NotImplemented

    def __ne__(self, other):
        return not self.__eq__(other)
