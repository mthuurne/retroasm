from .linereader import BadInput
from .utils import checkType

class Context:
    '''Container in which named elements such as variables, arguments,
    functions etc. are stored.
    Fetching elements is done through a dictionary-like interface.
    Storing elements is done by calling define().
    '''

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

    def keys(self):
        return self.elements.keys()

    def items(self):
        return self.elements.items()

    def define(self, name, value, location):
        '''Defines a named item in the this context.
        If the name was already taken, NameExistsError is raised.
        '''
        checkType(name, str, 'context element name')
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
        '''Checks whether the given name can be used in this context.
        Raises NameExistsError if the name is rejected.
        '''
        pass

class GlobalContext(Context):
    '''Context for the global scope.
    '''

    def _checkName(self, name, location):
        if name == 'ret':
            raise NameExistsError(
                'the name "ret" is reserved for function return values',
                location
                )

class NameExistsError(BadInput):
    '''Raised when attempting to add an element to a context under a name
    which is already in use.
    '''
