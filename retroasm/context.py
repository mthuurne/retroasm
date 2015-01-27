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

    def __contains__(self, key):
        return key in self.elements

    def __getitem__(self, key):
        return self.elements[key]

    def items(self):
        return self.elements.items()

    def define(self, name, value, location):
        '''Defines a named item in the global context.
        If the name was already taken, NameExistsError is raised.
        '''
        checkType(name, str, 'context element name')
        if name in self.elements:
            msg = 'name "%s" redefined' % name
            oldLocation = self.locations[name]
            if oldLocation is not None:
                msg += '; first definition was on line %d' % oldLocation.lineno
            raise NameExistsError(msg, location)
        self.locations[name] = location
        self.elements[name] = value

class NameExistsError(BadInput):
    '''Raised when attempting to add an element to a context under a name
    which is already in use.
    '''
