from .linereader import BadInput
from .utils import checkType

class Namespace:
    '''Container in which named elements such as variables, arguments,
    functions etc. are stored.
    Fetching elements is done through a dictionary-like interface.
    Storing elements is done by calling define().
    '''
    scope = property()

    def __init__(self):
        self.elements = {}
        self.locations = {}

    def __str__(self):
        return '%s(%s)' % (
            self.__class__.__name__,
            ', '.join('%s=%s' % item for item in self.elements.items())
            )

    def __contains__(self, key):
        return key in self.elements

    def __getitem__(self, key):
        return self.elements[key]

    def get(self, key):
        return self.elements.get(key)

    def keys(self):
        return self.elements.keys()

    def values(self):
        return self.elements.values()

    def items(self):
        return self.elements.items()

    def define(self, name, value, location):
        '''Defines a named item in the this namespace.
        If the name was already taken, NameExistsError is raised.
        '''
        checkType(name, str, 'name')
        self._checkName(name, location)
        if name in self.elements:
            msg = 'name "%s" redefined' % name
            oldLocation = self.locations[name]
            if oldLocation is not None:
                msg += '; first definition was on line %d' % oldLocation.lineno
            raise NameExistsError(msg, location)
        self.locations[name] = location
        self.elements[name] = value

    def _checkName(self, name, location):
        '''Checks whether the given name can be used in this namespace.
        Raises NameExistsError if the name is rejected.
        '''
        pass

class GlobalNamespace(Namespace):
    '''Namespace for the global scope.
    '''
    scope = property(lambda self: 0)

    def _checkName(self, name, location):
        if name == 'ret':
            raise NameExistsError(
                'the name "ret" is reserved for function return values',
                location
                )

class LocalNamespace(Namespace):
    '''A namespace for local blocks, that can import entries from its parent
    namespace on demand.
    '''
    scope = property(lambda self: 1)

    def __init__(self, parent):
        Namespace.__init__(self)
        self.parent = checkType(parent, Namespace, 'parent namespace')

    def __contains__(self, key):
        return super().__contains__(key) or key in self.parent

    def __getitem__(self, key):
        try:
            return super().__getitem__(key)
        except KeyError:
            value = self.parent[key]
            self.elements[key] = value
            self.locations[key] = None
            return value

    def _checkName(self, name, location):
        if name == 'pc':
            raise NameExistsError(
                'the name "pc" is reserved for the program counter register',
                location
                )

class NameExistsError(BadInput):
    '''Raised when attempting to add an element to a namespace under a name
    which is already in use.
    '''
