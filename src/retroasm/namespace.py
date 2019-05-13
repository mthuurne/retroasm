from __future__ import annotations

from typing import (
    Dict, ItemsView, KeysView, Optional, Sequence, Union, ValuesView, cast
)

from .codeblock import CodeBlock
from .codeblock_builder import SemanticsCodeBlockBuilder
from .expression import Expression, optSlice
from .function import Function
from .linereader import BadInput, InputLocation, LineReader
from .reference import BitString, FixedValue, Reference, SingleStorage
from .storage import (
    IOChannel, IOStorage, RefArgStorage, Storage, ValArgStorage, Variable
)
from .types import IntType, maskForWidth
from .utils import checkType

NamespaceValue = Union[Reference, IOChannel, Function]

class Namespace:
    '''Container in which named elements such as variables, arguments,
    functions etc. are stored.
    Fetching elements is done through a dictionary-like interface.
    Storing elements is done by calling define().
    '''

    def __init__(self, parent: Optional[Namespace]):
        self.parent = checkType(
            parent, (Namespace, type(None)), 'parent namespace'
            )
        self.elements: Dict[str, NamespaceValue] = {}
        self.locations: Dict[str, Optional[InputLocation]] = {}

    def __str__(self) -> str:
        return '%s(%s)' % (
            self.__class__.__name__,
            ', '.join('%s=%s' % item for item in self.elements.items())
            )

    def __contains__(self, key: str) -> bool:
        if key in self.elements:
            return True
        else:
            parent = self.parent
            return parent is not None and key in parent

    def __getitem__(self, key: str) -> NamespaceValue:
        try:
            return self.elements[key]
        except KeyError:
            parent = self.parent
            if parent is None:
                raise
            value = parent[key]
            self.elements[key] = value
            self.locations[key] = None
            return value

    def get(self, key: str) -> Optional[NamespaceValue]:
        return self.elements.get(key)

    def keys(self) -> KeysView[str]:
        return self.elements.keys()

    def values(self) -> ValuesView[NamespaceValue]:
        return self.elements.values()

    def items(self) -> ItemsView[str, NamespaceValue]:
        return self.elements.items()

    def define(self,
               name: str,
               value: NamespaceValue,
               location: InputLocation
               ) -> None:
        '''Defines a named item in the this namespace.
        If the name was already taken, NameExistsError is raised.
        '''
        checkType(name, str, 'name')
        self._checkName(name, location)
        if name in self.elements:
            msg = 'name "%s" redefined' % name
            if location is not None:
                oldLocation = self.locations[name]
                if oldLocation is not None:
                    raise NameExistsError(msg, (location, oldLocation))
            raise NameExistsError(msg, location)
        self.locations[name] = location
        self.elements[name] = value

    def _checkName(self, name: str, location: InputLocation) -> None:
        '''Checks whether the given name can be used in this namespace.
        Raises NameExistsError if the name is rejected.
        '''
        pass

    def _addNamedStorage(self,
                         name: str,
                         storage: Storage,
                         typ: IntType,
                         location: InputLocation
                         ) -> Reference:
        bits = SingleStorage(storage)
        ref = Reference(bits, typ)
        self.define(name, ref, location)
        return ref

    def addValueArgument(self,
                         name: str,
                         typ: IntType,
                         location: InputLocation
                         ) -> Reference:
        '''Adds a passed-by-value argument to this namespace.
        Returns a reference to the argument constant.
        '''
        checkType(typ, IntType, 'value argument type')
        storage = ValArgStorage(name, typ.width)
        return self._addNamedStorage(name, storage, typ, location)

    def addReferenceArgument(self,
                             name: str,
                             typ: IntType,
                             location: InputLocation
                             ) -> Reference:
        '''Adds a pass-by-reference argument with the given name and type to
        this namespace.
        Returns a reference to the argument.
        '''
        checkType(typ, IntType, 'reference argument type')
        storage = RefArgStorage(name, typ.width)
        return self._addNamedStorage(name, storage, typ, location)

class ContextNamespace(Namespace):
    '''A namespace for a mode entry context.
    '''

    def _checkName(self, name: str, location: InputLocation) -> None:
        _rejectPC(name, location)
        _rejectRet(name, location)

class BuilderNamespace(Namespace):
    '''A namespace with an associated code block builder.
    '''

    @property
    def scope(self) -> int:
        raise NotImplementedError

    def __init__(self,
                 parent: Optional[Namespace],
                 builder: SemanticsCodeBlockBuilder
                 ):
        Namespace.__init__(self, parent)
        self.builder = builder

    def dump(self) -> None:
        '''Prints the current state of this namespace and its code block
        builder on stdout.
        '''
        self.builder.dump()
        if 'ret' in self.elements:
            print('    return %s' % self.elements['ret'])

    def addVariable(self,
                    name: str,
                    typ: IntType,
                    location: InputLocation
                    ) -> Reference:
        '''Adds a variable with the given name and type to this namespace.
        Returns a reference to the variable.
        '''
        checkType(typ, IntType, 'variable type')
        storage = Variable(typ.width, self.scope)
        return self._addNamedStorage(name, storage, typ, location)

    def addValueArgument(self,
                         name: str,
                         typ: IntType,
                         location: InputLocation
                         ) -> Reference:
        '''Adds a passed-by-value argument to this namespace.
        A variable is created with the same name as the argument. The passed
        value is loaded from an ValArgStorage and then stored as the initial
        value of the variable.
        Returns a reference to the corresponding variable.
        '''
        checkType(typ, IntType, 'value argument type')
        storage = ValArgStorage(name, typ.width)
        argRef = Reference(SingleStorage(storage), typ)

        # Add Variable.
        varRef = self.addVariable(name, typ, location)

        # Store initial value.
        builder = self.builder
        value = argRef.emitLoad(builder, location)
        varRef.emitStore(builder, value, location)

        return varRef

class GlobalNamespace(BuilderNamespace):
    '''Namespace for the global scope.
    '''

    @property
    def scope(self) -> int:
        return 0

    def __init__(self, builder: SemanticsCodeBlockBuilder):
        BuilderNamespace.__init__(self, None, builder)

    def _checkName(self, name: str, location: InputLocation) -> None:
        _rejectRet(name, location)

class LocalNamespace(BuilderNamespace):
    '''A namespace for local blocks, that can import entries from its parent
    namespace on demand.
    '''

    @property
    def scope(self) -> int:
        return 1

    def _checkName(self, name: str, location: InputLocation) -> None:
        _rejectPC(name, location)

    def createCodeBlock(self,
                        ret: Optional[str] = 'ret',
                        log: Optional[LineReader] = None,
                        location: Optional[InputLocation] = None
                        ) -> CodeBlock:
        '''Returns a CodeBlock object containing the items emitted so far.
        The state of the builder does not change.
        If 'ret' is an existing name in this namespace, the reference with that
        name will be used for the returned bit string.
        If 'ret' is None or a non-existing name, the created code block will
        not return anything.
        Raises ValueError if our builder does not represent a valid code block.
        If a log is provided, errors are logged individually as well, using
        the given location if no specific location is known.
        '''
        retRef = None if ret is None else self.elements.get(ret)
        if retRef is None:
            returned: Sequence[BitString] = ()
        else:
            returned = (cast(Reference, retRef).bits,)
        return self.builder.createCodeBlock(returned, log, location)

class NameExistsError(BadInput):
    '''Raised when attempting to add an element to a namespace under a name
    which is already in use.
    '''

def _rejectRet(name: str, location: InputLocation) -> None:
    if name == 'ret':
        raise NameExistsError(
            'the name "ret" is reserved for function return values',
            location
            )

def _rejectPC(name: str, location: InputLocation) -> None:
    if name == 'pc':
        raise NameExistsError(
            'the name "pc" is reserved for the program counter register',
            location
            )

def createIOReference(channel: IOChannel, index: Expression) -> Reference:
    addrWidth = channel.addrType.width
    truncatedIndex = optSlice(index, 0, addrWidth)
    storage = IOStorage(channel, truncatedIndex)
    bits = SingleStorage(storage)
    return Reference(bits, channel.elemType)
