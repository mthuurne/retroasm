from __future__ import annotations

from collections.abc import Iterator, Mapping, Set
from typing import Never, TypeVar, cast, override

from .function import Function
from .input import BadInput, InputLocation
from .reference import FixedValueReference, Reference, SingleStorage, Variable
from .storage import ArgStorage, IOChannel, Register
from .types import IntType

type NamespaceValue = Reference | IOChannel | Function
type ReadOnlyNamespace = Mapping[str, NamespaceValue]

ET = TypeVar("ET")
PT = TypeVar("PT")


class Namespace(Mapping[str, ET | PT]):
    """
    Mapping in which named elements such as variables, arguments,
    functions etc. are stored.
    Storing elements is done by calling define().
    When an element is looked up that was not stored in this namespace,
    it is looked up in the parent namespace (if any).
    """

    def __init__(self, parent: Mapping[str, PT] | None):
        self.parent = parent
        self.elements: dict[str, ET] = {}
        self.locations: dict[str, InputLocation | None] = {}

    @override
    def __str__(self) -> str:
        args = ", ".join(f"{name}={value}" for name, value in self.elements.items())
        return f"{self.__class__.__name__}({args})"

    def _combine_keys(self) -> Set[str]:
        """Return the combined keys of this namespace and its parent (if any)."""
        keys = self.elements.keys()
        parent = self.parent
        return keys if parent is None else (keys | parent.keys())

    @override
    def __len__(self) -> int:
        return len(self._combine_keys())

    @override
    def __iter__(self) -> Iterator[str]:
        return iter(self._combine_keys())

    @override
    def __contains__(self, key: object) -> bool:
        return key in self.elements or ((parent := self.parent) is not None and key in parent)

    @override
    def __getitem__(self, key: str) -> ET | PT:
        try:
            return self.elements[key]
        except KeyError:
            parent = self.parent
            if parent is None:
                raise
            return parent[key]

    def define(self, name: str, value: ET, location: InputLocation | None = None) -> None:
        """
        Define a named item in this namespace.
        Raises NameExistsError if the name was already taken.
        """
        self._check_name(name, value, location)
        if name in self.elements:
            old_location = self.locations[name]
            raise NameExistsError(f'name "{name}" redefined', location, old_location)
        if (parent := self.parent) is not None and name in parent:
            old_location = parent.locations[name] if isinstance(parent, Namespace) else None
            raise NameExistsError(f'name "{name}" redefined', location, old_location)
        self.locations[name] = location
        self.elements[name] = value

    def _check_name(self, name: str, value: ET, location: InputLocation | None) -> None:
        """
        Check whether the given name can be used in this namespace for the given value.

        Raises NameExistsError if the name is rejected.
        The default implementation accepts all names.
        """


class ContextNamespace(Namespace[FixedValueReference, NamespaceValue]):
    """A namespace for a mode row's context."""

    @override
    def _check_name(
        self, name: str, value: NamespaceValue, location: InputLocation | None
    ) -> None:
        _reject_pc(name, location)
        _reject_ret(name, location)


class GlobalNamespace(Namespace[NamespaceValue, Never]):
    """Namespace for the global scope."""

    def __init__(self) -> None:
        Namespace.__init__(self, None)

    @property
    def program_counter(self) -> Reference | None:
        return cast(Reference | None, self.elements.get("pc"))

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
        ref = Reference(SingleStorage(Register(name, typ.width)), typ)
        self.define(name, ref, location)
        return ref


class LocalNamespace(Namespace[Reference, NamespaceValue]):
    """
    A namespace for local blocks, that can import entries from its parent
    namespace on demand.
    """

    @override
    def _check_name(
        self, name: str, value: NamespaceValue, location: InputLocation | None
    ) -> None:
        _reject_pc(name, location)

    def add_argument(
        self, name: str, typ: IntType, location: InputLocation | None = None
    ) -> Reference:
        """
        Add an pass-by-reference argument to this namespace.
        Returns a reference to the argument's storage.
        """
        ref = Reference(SingleStorage(ArgStorage(name, typ.width)), typ)
        self.define(name, ref, location)
        return ref

    def add_variable(
        self, name: str, typ: IntType, location: InputLocation | None = None
    ) -> Reference:
        ref = Reference(Variable(name, typ.width), typ)
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
        raise NameExistsError('the name "ret" is reserved for function return values', location)


def _reject_pc(name: str, location: InputLocation | None) -> None:
    if name == "pc":
        raise NameExistsError(
            'the name "pc" is reserved for the program counter register', location
        )
