from .expression import IntType

import re

reRegName = re.compile('[a-z][a-z0-9]*\'?$')

class Register:
    '''A CPU register.
    '''
    __slots__ = ('name', 'type')

    def __init__(self, name, typ):
        if not isinstance(name, str):
            raise TypeError('name should be integer, got %s' % type(name))
        if not isinstance(typ, IntType):
            raise TypeError('type should be IntType, got %s' % type(typ))
        if not reRegName.match(name):
            raise ValueError('invalid register name: "%s"', name)

        self.name = name
        self.type = typ

    def __str__(self):
        return self.name

    def __repr__(self):
        return 'Register(%s, %s)' % (repr(self.name), repr(self.type))

    def __eq__(self, other):
        if isinstance(other, Register):
            return self.name == other.name and self.type == other.type
        else:
            return NotImplemented

    def __ne__(self, other):
        return not self.__eq__(other)
