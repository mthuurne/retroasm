from __future__ import annotations

from retroasm.expression import IntLiteral
from retroasm.storage import (
    ArgStorage,
    IOChannel,
    IOStorage,
    Register,
    Storage,
    Variable,
)
from retroasm.types import IntType


def assert_alias(storage1: Storage, storage2: Storage) -> None:
    assert storage1.might_be_same(storage2), f"{storage1} does not alias {storage2}"
    assert storage2.might_be_same(
        storage1
    ), f"{storage1} does alias {storage2}, but not vice versa"


def assert_no_alias(storage1: Storage, storage2: Storage) -> None:
    assert not storage1.might_be_same(storage2), f"{storage1} does alias {storage2}"
    assert not storage2.might_be_same(
        storage1
    ), f"{storage1} does not alias {storage2}, but vice versa it does"


def test_register_aliasing() -> None:
    """Test when registers might be aliased."""
    a = Register(8)
    b = Register(8)
    l = Variable(8)
    r = ArgStorage("R", 8)
    mem = IOChannel("mem", IntType.u(8), IntType.u(16))
    m = IOStorage(mem, IntLiteral(0xC000))
    assert_alias(a, a)
    assert_no_alias(a, b)
    assert_no_alias(a, l)
    assert_alias(a, r)
    assert_no_alias(a, m)


def test_variable_aliasing() -> None:
    """Test when variables might be aliased."""
    l = Variable(8)
    l2 = Variable(8)
    a = Register(8)
    r = ArgStorage("R", 8)
    mem = IOChannel("mem", IntType.u(8), IntType.u(16))
    m = IOStorage(mem, IntLiteral(0xC000))
    assert_alias(l, l)
    assert_no_alias(l, l2)
    assert_no_alias(l, a)
    assert_no_alias(l, r)
    assert_no_alias(l, m)


def test_unknown_storage_aliasing() -> None:
    """Test when unknown storages might be aliased."""
    r = ArgStorage("R", 8)
    r2 = ArgStorage("R2", 8)
    a = Register(8)
    l = Variable(8)
    mem = IOChannel("mem", IntType.u(8), IntType.u(16))
    m = IOStorage(mem, IntLiteral(0xC000))
    assert_alias(r, r)
    assert_alias(r, r2)
    assert_alias(r, a)
    assert_no_alias(r, l)
    assert_alias(r, m)


def test_io_aliasing() -> None:
    """Test when I/O storages might be aliased."""
    a = Register(8)
    l = Variable(8)
    r = ArgStorage("R", 8)
    mem = IOChannel("mem", IntType.u(8), IntType.u(16))
    io = IOChannel("io", IntType.u(8), IntType.u(16))
    m = IOStorage(mem, IntLiteral(0xC000))
    i = IOStorage(io, IntLiteral(0xC000))
    m2 = IOStorage(mem, IntLiteral(0xE000))
    assert_alias(m, m)
    assert_alias(m, m2)
    assert_no_alias(m, i)
    assert_no_alias(m, a)
    assert_no_alias(m, l)
    assert_alias(m, r)
