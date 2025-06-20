from __future__ import annotations

from retroasm.expression import IntLiteral
from retroasm.storage import ArgStorage, IOChannel, IOStorage, Register, Storage
from retroasm.types import IntType


def assert_alias(storage1: Storage, storage2: Storage) -> None:
    assert storage1.might_be_same(storage2), f"{storage1} does not alias {storage2}"
    assert storage2.might_be_same(storage1), (
        f"{storage1} does alias {storage2}, but not vice versa"
    )


def assert_no_alias(storage1: Storage, storage2: Storage) -> None:
    assert not storage1.might_be_same(storage2), f"{storage1} does alias {storage2}"
    assert not storage2.might_be_same(storage1), (
        f"{storage1} does not alias {storage2}, but vice versa it does"
    )


def test_register_aliasing() -> None:
    """Test when registers might be aliased."""
    a = Register("A", 8)
    b = Register("B", 8)
    r = ArgStorage("R", 8)
    mem = IOChannel("mem", IntType.u(8), IntType.u(16))
    m = IOStorage(mem, IntLiteral(0xC000))
    assert_alias(a, a)
    assert_no_alias(a, b)
    assert_alias(a, r)
    assert_no_alias(a, m)


def test_unknown_storage_aliasing() -> None:
    """Test when unknown storages might be aliased."""
    r = ArgStorage("R", 8)
    r2 = ArgStorage("R2", 8)
    a = Register("A", 8)
    mem = IOChannel("mem", IntType.u(8), IntType.u(16))
    m = IOStorage(mem, IntLiteral(0xC000))
    assert_alias(r, r)
    assert_alias(r, r2)
    assert_alias(r, a)
    assert_alias(r, m)


def test_io_aliasing() -> None:
    """Test when I/O storages might be aliased."""
    a = Register("A", 8)
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
    assert_alias(m, r)
