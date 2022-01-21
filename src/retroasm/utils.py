from __future__ import annotations

from collections.abc import (
    Callable,
    Iterator,
    MutableMapping,
    MutableSequence,
    MutableSet,
)
from functools import update_wrapper
from types import MappingProxyType
from typing import TYPE_CHECKING, NoReturn
from weakref import WeakValueDictionary


class _Impossible:
    pass


def bad_type(value: _Impossible) -> NoReturn:
    """
    Report a type error.

    If this function is called with a known type, the type checker will complain.
    In code that handles the different types in a type union separately, call
    this function in the default case to verify that no types were left out.
    """
    raise TypeError(type(value).__name__)


class Unique(type):
    """
    Metaclass that enforces that for each combination of arguments there
    is only one instance.
    Arguments must be of hashable types.
    Keyword constructor arguments are not supported.
    Weak references are used to keep track of instances, so if you define
    __slots__ in your class, make sure you include '__weakref__' in __slots__.
    """

    def __init__(cls, name: str, bases: tuple[type, ...], nmspc: dict[str, object]):
        type.__init__(cls, name, bases, nmspc)
        # pylint: disable=abstract-class-instantiated
        cls.__cache: MutableMapping[object, object] = WeakValueDictionary()

    def __call__(cls, *args: object, **kwargs: object) -> object:
        if kwargs:
            raise TypeError("Metaclass Unique does not support keyword arguments")
        cache = cls.__cache
        value = cache.get(args)
        if value is None:
            value = super().__call__(*args)
            cache[args] = value
        return value


class Singleton(type):
    """Metaclass that enforces that there is one shared instance of a class."""

    def __init__(cls, name: str, bases: tuple[type, ...], nmspc: dict[str, object]):
        type.__init__(cls, name, bases, nmspc)
        cls.__instance = None

    def __call__(cls, *args: object, **kwargs: object) -> object:
        if args or kwargs:
            raise TypeError("Metaclass Singleton does not support arguments")
        instance = cls.__instance
        if instance is None:
            instance = super().__call__()
            cls.__instance = instance
        return instance


if TYPE_CHECKING:
    # TODO: This correctly expresses the caching part, but not the const part.
    const_property = property
else:

    class const_property:
        """
        Decorator for properties that don't change in value.
        The getter function is called at most once: the first time the property
        is read.
        If the getter returns an iterator or a mutable sequence, set or mapping,
        the returned value is converted to a read-only value.
        The value is stored in an attribute that is named equal to the wrapped
        getter function, with an underscore prepended. In classes that define
        __slots__, make sure that you include this attribute as well.
        """

        def __init__(self, getter):
            self._getter = getter
            self._name = "_" + getter.__name__
            update_wrapper(self, getter)

        def __get__(self, obj, objtype):
            if obj is None:
                return self
            else:
                name = self._name
                value = getattr(obj, name, None)
                if value is None:
                    value = self._getter(obj)

                    # Convert to read-only type.
                    if isinstance(value, (Iterator, MutableSequence)):
                        value = tuple(value)
                    elif isinstance(value, MutableSet):
                        value = frozenset(value)
                    elif isinstance(value, MutableMapping):
                        value = MappingProxyType(value)

                    setattr(obj, name, value)
                return value

        def __set__(self, obj, value):
            raise AttributeError("const_property cannot be set")

        def __delete__(self, obj):
            raise AttributeError("const_property cannot be deleted")


def search(low: int, high: int, test: Callable[[int], bool]) -> int:
    """Binary search: [low..high) is the range to search; function "test"
    takes a single value from that interval and returns a truth value.
    The function must be ascending: (test(x) and y >= x) => test(y).
    Returns smallest argument in the interval for which the function is true,
    or "high" if the function is false for the entire interval.
    """
    while low < high:
        mid = (low + high - 1) // 2
        if test(mid):
            if mid == low:
                return low  # found
            high = mid + 1
        else:
            low = mid + 1
    return high  # not found
