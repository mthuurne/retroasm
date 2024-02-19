from __future__ import annotations

from collections.abc import ItemsView, KeysView, Sequence, ValuesView
from typing import TypeAlias

from .codeblock import BasicBlock
from .codeblock_builder import CodeBlockBuilder, SemanticsCodeBlockBuilder
from .expression import Expression, opt_slice
from .function import Function
from .parser.linereader import BadInput, InputLocation, LineReader
from .reference import BitString, Reference, SingleStorage, Variable
from .storage import (
    ArgStorage,
    IOChannel,
    IOStorage,
    Register,
    Storage,
)
from .types import IntType

NamespaceValue: TypeAlias = Reference | IOChannel | Function


class Namespace:
    """
    Container in which named elements such as variables, arguments,
    functions etc. are stored.
    Fetching elements is done through a dictionary-like interface.
    Storing elements is done by calling define().
    """

    def __init__(self, parent: Namespace | None):
        self.parent = parent
        self.elements: dict[str, NamespaceValue] = {}
        self.locations: dict[str, InputLocation | None] = {}

    def __str__(self) -> str:
        args = ", ".join(f"{name}={value}" for name, value in self.elements.items())
        return f"{self.__class__.__name__}({args})"

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

    def define(
        self, name: str, value: NamespaceValue, location: InputLocation | None = None
    ) -> None:
        """
        Defines a named item in the this namespace.
        If the name was already taken, NameExistsError is raised.
        """
        self._check_name(name, value, location)
        if name in self.elements:
            try:
                old_location = self.locations[name]
            except KeyError:
                # No stored location implies this is an imported name.
                # TODO: Showing the import location would be nice,
                #       but we'd have to change the interface to pass
                #       a location for lookups.
                raise NameExistsError(
                    f'imported name "{name}" redefined', location
                ) from None
            else:
                raise NameExistsError(
                    f'name "{name}" redefined', location, old_location
                )
        self.locations[name] = location
        self.elements[name] = value

    def _check_name(
        self, name: str, value: NamespaceValue, location: InputLocation | None
    ) -> None:
        """
        Check whether the given name can be used in this namespace for the given value.

        Raises NameExistsError if the name is rejected.
        The default implementation accepts all names.
        """

    def _add_named_storage(
        self, name: str, storage: Storage, typ: IntType, location: InputLocation | None
    ) -> Reference:
        bits = SingleStorage(storage)
        ref = Reference(bits, typ)
        self.define(name, ref, location)
        return ref

    def add_argument(
        self, name: str, typ: IntType, location: InputLocation | None = None
    ) -> Reference:
        """
        Add an pass-by-reference argument to this namespace.
        Returns a reference to the argument's storage.
        """
        storage = ArgStorage(name, typ.width)
        return self._add_named_storage(name, storage, typ, location)


class ContextNamespace(Namespace):
    """A namespace for a mode entry context."""

    def _check_name(
        self, name: str, value: NamespaceValue, location: InputLocation | None
    ) -> None:
        _reject_pc(name, location)
        _reject_ret(name, location)


class BuilderNamespace(Namespace):
    """A namespace with an associated code block builder."""

    def __init__(self, parent: Namespace | None, builder: CodeBlockBuilder):
        Namespace.__init__(self, parent)
        self.builder = builder

    def dump(self) -> None:
        """
        Prints the current state of this namespace and its code block
        builder on stdout.
        """
        self.builder.dump()
        if "ret" in self.elements:
            print(f"    return {self.elements['ret']}")

    def add_variable(
        self, name: str, typ: IntType, location: InputLocation | None = None
    ) -> Reference:
        """
        Adds a variable with the given name and type to this namespace.
        Returns a reference to the variable.
        """
        raise NotImplementedError


class GlobalNamespace(BuilderNamespace):
    """Namespace for the global scope."""

    def __init__(self, builder: CodeBlockBuilder):
        BuilderNamespace.__init__(self, None, builder)

    def _check_name(
        self, name: str, value: NamespaceValue, location: InputLocation | None
    ) -> None:
        if not isinstance(value, Reference):
            _reject_pc(name, location)
        _reject_ret(name, location)

    def add_variable(
        self, name: str, typ: IntType, location: InputLocation | None = None
    ) -> Reference:
        storage = Register(name, typ.width)
        return self._add_named_storage(name, storage, typ, location)


class LocalNamespace(BuilderNamespace):
    """
    A namespace for local blocks, that can import entries from its parent
    namespace on demand.
    """

    def __init__(self, parent: Namespace | None, builder: SemanticsCodeBlockBuilder):
        super().__init__(parent, builder)
        self.builder: SemanticsCodeBlockBuilder

    def _check_name(
        self, name: str, value: NamespaceValue, location: InputLocation | None
    ) -> None:
        _reject_pc(name, location)

    def add_variable(
        self, name: str, typ: IntType, location: InputLocation | None = None
    ) -> Reference:
        bits = Variable(name, typ.width)
        ref = Reference(bits, typ)
        self.define(name, ref, location)
        return ref

    def create_code_block(
        self,
        ret_ref: Reference | None,
        log: LineReader | None = None,
        location: InputLocation | None = None,
    ) -> BasicBlock:
        """
        Returns a CodeBlock object containing the items emitted so far.
        The state of the builder does not change.
        If `ret_ref` is None, the created code block will not return anything,
        otherwise it returns that reference.
        Raises ValueError if our builder does not represent a valid code block.
        If a log is provided, errors are logged individually as well, using
        the given location if no specific location is known.
        """
        if ret_ref is None:
            returned: Sequence[BitString] = ()
        else:
            returned = (ret_ref.bits,)
        return self.builder.create_code_block(returned, log, location)


class NameExistsError(BadInput):
    """
    Raised when attempting to add an element to a namespace under a name
    which is already in use.
    """

    def __init__(
        self,
        msg: str,
        location: InputLocation | None,
        old_location: InputLocation | None = None,
    ):
        locations = tuple(loc for loc in (location, old_location) if loc is not None)
        super().__init__(msg, *locations)


def _reject_ret(name: str, location: InputLocation | None) -> None:
    if name == "ret":
        raise NameExistsError(
            'the name "ret" is reserved for function return values', location
        )


def _reject_pc(name: str, location: InputLocation | None) -> None:
    if name == "pc":
        raise NameExistsError(
            'the name "pc" is reserved for the program counter register', location
        )


def create_io_reference(channel: IOChannel, index: Expression) -> Reference:
    addr_width = channel.addr_type.width
    truncated_index = opt_slice(index, 0, addr_width)
    storage = IOStorage(channel, truncated_index)
    bits = SingleStorage(storage)
    return Reference(bits, channel.elem_type)
