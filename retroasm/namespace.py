from .codeblock import FixedValue, LoadedConstant, Reference, SingleReference
from .function import Function
from .linereader import BadInput
from .storage import IOChannel
from .utils import checkType

class Namespace:
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

    def _checkName(self, name, location):
        if name == 'ret':
            raise NameExistsError(
                'the name "ret" is reserved for function return values',
                location
                )

class LocalNamespace(Namespace):
    '''A namespace for local blocks, that can import entries from its parent
    namespace on demand.
    Its goal is to avoid a lot of redundant references when a block is first
    created: although the simplifier can remove those, that is a pretty
    inefficient operation which should be applied to non-trivial cases only.
    '''

    def __init__(self, localBuilder, parentBuilder):
        Namespace.__init__(self)
        self.localBuilder = localBuilder
        self.parentBuilder = parentBuilder
        self.cidImportMap = {}
        self.storageImportMap = {}

    def __contains__(self, key):
        return super().__contains__(key) or key in self.parentBuilder.namespace

    def __getitem__(self, key):
        try:
            return super().__getitem__(key)
        except KeyError:
            value = self.parentBuilder.namespace[key]
            if isinstance(value, (Function, IOChannel)):
                pass
            elif isinstance(value, Reference):
                value = value.clone(
                    self._importSingleRef, self._importFixedValue
                    )
            else:
                assert False, (key, repr(value))
            self.elements[key] = value
            self.locations[key] = None
            return value

    def _importSingleRef(self, parentRef):
        '''Imports the given SingleReference from the parent builder into the
        local namespace. Returns the local reference.
        '''
        storage = parentRef.storage
        importMap = self.storageImportMap
        try:
            return importMap[storage]
        except KeyError:
            localBuilder = self.localBuilder
            localRef = SingleReference(localBuilder, storage, parentRef.type)
            importMap[storage] = localRef
            return localRef

    def _importFixedValue(self, parentRef):
        '''Imports the given FixedValue from the parent builder into the
        local namespace. Returns the local reference.
        '''
        return FixedValue(parentRef.expr, parentRef.type)

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
