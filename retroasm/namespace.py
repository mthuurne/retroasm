from .codeblock import ArgumentValue
from .expression import optSlice
from .linereader import BadInput
from .reference import Reference, SingleStorage
from .storage import IOStorage, RefArgStorage, Variable
from .types import IntType, maskForWidth
from .utils import checkType

class Namespace:
    '''Container in which named elements such as variables, arguments,
    functions etc. are stored.
    Fetching elements is done through a dictionary-like interface.
    Storing elements is done by calling define().
    '''
    scope = property()

    def __init__(self, builder):
        self.builder = builder
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

    def dump(self):
        '''Prints the current state of this namespace and its code block
        builder on stdout.
        '''
        self.builder.dump()
        if 'ret' in self.elements:
            print('    return %s' % self.elements['ret'])

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

    def _addNamedStorage(self, name, storage, typ, location):
        bits = SingleStorage(storage)
        ref = Reference(bits, typ)
        self.define(name, ref, location)
        return ref

    def addVariable(self, name, typ, location):
        '''Adds a variable with the given name and type to this namespace.
        Returns a reference to the variable.
        '''
        checkType(typ, IntType, 'variable type')
        storage = Variable(typ.width, self.scope)
        return self._addNamedStorage(name, storage, typ, location)

    def addValueArgument(self, name, typ, location):
        '''Adds a passed-by-value argument to this namespace.
        A variable is created with the same name as the argument. The passed
        value is represented by an ArgumentValue and a store is emitted on the
        given builder to set the passed value as the initial value of the
        variable.
        Returns a reference to the corresponding variable.
        '''
        checkType(typ, IntType, 'value argument type')
        value = ArgumentValue(name, maskForWidth(typ.width))

        # Add Variable.
        ref = self.addVariable(name, typ, location)

        # Store initial value.
        ref.emitStore(self.builder, value, location)

        return ref

    def addReferenceArgument(self, name, typ, location):
        '''Adds a pass-by-reference argument with the given name and type to
        this namespace.
        Returns a reference to the argument.
        '''
        checkType(typ, IntType, 'reference argument type')
        storage = RefArgStorage(name, typ.width)
        return self._addNamedStorage(name, storage, typ, location)

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

    def __init__(self, parent, builder):
        Namespace.__init__(self, builder)
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

    def createCodeBlock(self, ret='ret', log=None):
        '''Returns a CodeBlock object containing the items emitted so far.
        The state of the builder does not change.
        If 'ret' is an existing name in this namespace, the reference with that
        name will be used for the returned bit string.
        If 'ret' is None or a non-existing name, the created code block will
        not return anything.
        Raises ValueError if our builder does not represent a valid code block.
        If a log is provided, errors are logged individually as well.
        '''
        retRef = None if ret is None else self.elements.get(ret)
        returned = () if retRef is None else (retRef.bits,)
        return self.builder.createCodeBlock(returned, log)

class NameExistsError(BadInput):
    '''Raised when attempting to add an element to a namespace under a name
    which is already in use.
    '''

def createIOReference(channel, index):
    addrWidth = channel.addrType.width
    truncatedIndex = optSlice(index, 0, addrWidth)
    storage = IOStorage(channel, truncatedIndex)
    bits = SingleStorage(storage)
    return Reference(bits, channel.elemType)
