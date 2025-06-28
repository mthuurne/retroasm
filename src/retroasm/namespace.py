from __future__ import annotations

from collections.abc import ItemsView, Iterator, KeysView, Mapping, ValuesView
from typing import TypeVar, cast, overload, override

from .function import Function
from .input import BadInput, InputLocation
from .reference import Reference, SingleStorage, Variable
from .storage import ArgStorage, IOChannel, Register, Storage
from .types import IntType

type NamespaceValue = Reference | IOChannel | Function

_T = TypeVar("_T")


type ReadOnlyNamespace = Mapping[str, NamespaceValue]


class Namespace(Mapping[str, NamespaceValue]):
    """
    Container in which named elements such as variables, arguments,
    functions etc. are stored.
    Fetching elements is done through a dictionary-like interface.
    Storing elements is done by calling define().
    """

    def __init__(self, parent: ReadOnlyNamespace | None):
        self.parent = parent
        self.elements: dict[str, NamespaceValue] = {}
        self.locations: dict[str, InputLocation | None] = {}

    @override
    def __str__(self) -> str:
        args = ", ".join(f"{name}={value}" for name, value in self.elements.items())
        return f"{self.__class__.__name__}({args})"

    @override
    def __len__(self) -> int:
        return len(self.elements)

    @override
    def __iter__(self) -> Iterator[str]:
        return iter(self.elements)

    @override
    def __contains__(self, key: object) -> bool:
        if key in self.elements:
            return True
        else:
            parent = self.parent
            return parent is not None and key in parent

    @override
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

    @overload
    def get(self, key: str, /) -> NamespaceValue | None: ...

    @overload
    def get(self, key: str, /, default: NamespaceValue | _T) -> NamespaceValue | _T: ...

    @override
    def get(self, key: str, /, default: _T | None = None) -> NamespaceValue | _T | None:
        return self.elements.get(key)

    @override
    def keys(self) -> KeysView[str]:
        return self.elements.keys()

    @override
    def values(self) -> ValuesView[NamespaceValue]:
        return self.elements.values()

    @override
    def items(self) -> ItemsView[str, NamespaceValue]:
        return self.elements.items()

    def define(
        self, name: str, value: NamespaceValue, location: InputLocation | None = None
    ) -> None:
        """
        Define a named item in this namespace.
        Raises NameExistsError if the name was already taken.
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

    @override
    def _check_name(
        self, name: str, value: NamespaceValue, location: InputLocation | None
    ) -> None:
        _reject_pc(name, location)
        _reject_ret(name, location)


class GlobalNamespace(Namespace):
    """Namespace for the global scope."""

    def __init__(self) -> None:
        Namespace.__init__(self, None)

    @property
    def program_counter(self) -> Reference:
        return cast(Reference, self.elements["pc"])

    @override
    def _check_name(
        self, name: str, value: NamespaceValue, location: InputLocation | None
    ) -> None:
        if not isinstance(value, Reference):
            _reject_pc(name, location)
        _reject_ret(name, location)

    def add_register(
        self, name: str, typ: IntType, location: InputLocation | None = None
    ) -> Reference:
        storage = Register(name, typ.width)
        return self._add_named_storage(name, storage, typ, location)


class LocalNamespace(Namespace):
    """
    A namespace for local blocks, that can import entries from its parent
    namespace on demand.
    """

    @override
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
