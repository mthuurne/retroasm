from __future__ import annotations

from typing import ItemsView, KeysView, Sequence, Union, ValuesView

from .codeblock import CodeBlock
from .codeblock_builder import CodeBlockBuilder, SemanticsCodeBlockBuilder
from .expression import Expression, optSlice
from .function import Function
from .linereader import BadInput, InputLocation, LineReader
from .reference import BitString, Reference, SingleStorage
from .storage import ArgStorage, IOChannel, IOStorage, Storage, Variable
from .types import IntType

NamespaceValue = Union[Reference, IOChannel, Function]

class Namespace:
    """Container in which named elements such as variables, arguments,
    functions etc. are stored.
    Fetching elements is done through a dictionary-like interface.
    Storing elements is done by calling define().
    """

    def __init__(self, parent: Namespace | None):
        self.parent = parent
        self.elements: dict[str, NamespaceValue] = {}
        self.locations: dict[str, InputLocation] = {}

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
            return value

    def get(self, key: str) -> NamespaceValue | None:
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
        """Defines a named item in the this namespace.
        If the name was already taken, NameExistsError is raised.
        """
        self._checkName(name, value, location)
        if name in self.elements:
            oldLocation = self.locations.get(name)
            if oldLocation is None:
                # No stored location implies this is an imported name.
                # TODO: Showing the import location would be nice,
                #       but we'd have to change the interface to pass
                #       a location for lookups.
                raise NameExistsError(f'imported name "{name}" redefined',
                                      location)
            else:
                raise NameExistsError(f'name "{name}" redefined',
                                      location, oldLocation)
        self.locations[name] = location
        self.elements[name] = value

    def _checkName(self,
                   name: str,
                   value: NamespaceValue,
                   location: InputLocation
                   ) -> None:
        """Checks whether the given name can be used in this namespace
        for the given value.
        Raises NameExistsError if the name is rejected.
        """
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

    def addArgument(self,
                    name: str,
                    typ: IntType,
                    location: InputLocation
                    ) -> Reference:
        """Add an pass-by-reference argument to this namespace.
        Returns a reference to the argument's storage.
        """
        storage = ArgStorage(name, typ.width)
        return self._addNamedStorage(name, storage, typ, location)

class ContextNamespace(Namespace):
    """A namespace for a mode entry context.
    """

    def _checkName(self,
                   name: str,
                   value: NamespaceValue,
                   location: InputLocation
                   ) -> None:
        _rejectPC(name, location)
        _rejectRet(name, location)

class BuilderNamespace(Namespace):
    """A namespace with an associated code block builder.
    """

    @property
    def scope(self) -> int:
        raise NotImplementedError

    def __init__(self,
                 parent: Namespace | None,
                 builder: CodeBlockBuilder
                 ):
        Namespace.__init__(self, parent)
        self.builder = builder

    def dump(self) -> None:
        """Prints the current state of this namespace and its code block
        builder on stdout.
        """
        self.builder.dump()
        if 'ret' in self.elements:
            print(f"    return {self.elements['ret']}")

    def addVariable(self,
                    name: str,
                    typ: IntType,
                    location: InputLocation
                    ) -> Reference:
        """Adds a variable with the given name and type to this namespace.
        Returns a reference to the variable.
        """
        storage = Variable(typ.width, self.scope)
        return self._addNamedStorage(name, storage, typ, location)

class GlobalNamespace(BuilderNamespace):
    """Namespace for the global scope.
    """

    @property
    def scope(self) -> int:
        return 0

    def __init__(self, builder: CodeBlockBuilder):
        BuilderNamespace.__init__(self, None, builder)

    def _checkName(self,
                   name: str,
                   value: NamespaceValue,
                   location: InputLocation
                   ) -> None:
        if not isinstance(value, Reference):
            _rejectPC(name, location)
        _rejectRet(name, location)

class LocalNamespace(BuilderNamespace):
    """A namespace for local blocks, that can import entries from its parent
    namespace on demand.
    """

    @property
    def scope(self) -> int:
        return 1

    def __init__(self,
                 parent: Namespace | None,
                 builder: SemanticsCodeBlockBuilder
                 ):
        super().__init__(parent, builder)
        self.builder: SemanticsCodeBlockBuilder

    def _checkName(self,
                   name: str,
                   value: NamespaceValue,
                   location: InputLocation
                   ) -> None:
        _rejectPC(name, location)

    def createCodeBlock(self,
                        retRef: Reference | None,
                        log: LineReader | None = None,
                        location: InputLocation | None = None
                        ) -> CodeBlock:
        """Returns a CodeBlock object containing the items emitted so far.
        The state of the builder does not change.
        If `retRef` is None, the created code block will not return anything,
        otherwise it returns that reference.
        Raises ValueError if our builder does not represent a valid code block.
        If a log is provided, errors are logged individually as well, using
        the given location if no specific location is known.
        """
        if retRef is None:
            returned: Sequence[BitString] = ()
        else:
            returned = (retRef.bits,)
        return self.builder.createCodeBlock(returned, log, location)

class NameExistsError(BadInput):
    """Raised when attempting to add an element to a namespace under a name
    which is already in use.
    """

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
