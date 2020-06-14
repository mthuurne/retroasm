from retroasm.expression import IntLiteral
from retroasm.storage import ArgStorage, IOChannel, IOStorage, Variable
from retroasm.types import IntType


def assertAlias(storage1, storage2):
    assert storage1.mightBeSame(storage2), \
        f'{storage1} does not alias {storage2}'
    assert storage2.mightBeSame(storage1), \
        f'{storage1} does alias {storage2}, but not vice versa'

def assertNoAlias(storage1, storage2):
    assert not storage1.mightBeSame(storage2), \
        f'{storage1} does alias {storage2}'
    assert not storage2.mightBeSame(storage1), \
        f'{storage1} does not alias {storage2}, but vice versa it does'

def test_register_aliasing():
    '''Test when registers might be aliased.'''
    a = Variable(8, 0)
    b = Variable(8, 0)
    l = Variable(8, 1)
    r = ArgStorage('R', 8)
    mem = IOChannel('mem', IntType.u(8), IntType.u(16))
    m = IOStorage(mem, IntLiteral(0xC000))
    assertAlias(a, a)
    assertNoAlias(a, b)
    assertNoAlias(a, l)
    assertAlias(a, r)
    assertNoAlias(a, m)

def test_variable_aliasing():
    '''Test when variables might be aliased.'''
    l = Variable(8, 1)
    l2 = Variable(8, 1)
    a = Variable(8, 0)
    r = ArgStorage('R', 8)
    mem = IOChannel('mem', IntType.u(8), IntType.u(16))
    m = IOStorage(mem, IntLiteral(0xC000))
    assertAlias(l, l)
    assertNoAlias(l, l2)
    assertNoAlias(l, a)
    assertNoAlias(l, r)
    assertNoAlias(l, m)

def test_unknown_storage_aliasing():
    '''Test when unknown storages might be aliased.'''
    r = ArgStorage('R', 8)
    r2 = ArgStorage('R2', 8)
    a = Variable(8, 0)
    l = Variable(8, 1)
    mem = IOChannel('mem', IntType.u(8), IntType.u(16))
    m = IOStorage(mem, IntLiteral(0xC000))
    assertAlias(r, r)
    assertAlias(r, r2)
    assertAlias(r, a)
    assertNoAlias(r, l)
    assertAlias(r, m)

def test_io_aliasing():
    '''Test when I/O storages might be aliased.'''
    a = Variable(8, 0)
    l = Variable(8, 1)
    r = ArgStorage('R', 8)
    mem = IOChannel('mem', IntType.u(8), IntType.u(16))
    io = IOChannel('io', IntType.u(8), IntType.u(16))
    m = IOStorage(mem, IntLiteral(0xC000))
    i = IOStorage(io, IntLiteral(0xC000))
    m2 = IOStorage(mem, IntLiteral(0xE000))
    assertAlias(m, m)
    assertAlias(m, m2)
    assertNoAlias(m, i)
    assertNoAlias(m, a)
    assertNoAlias(m, l)
    assertAlias(m, r)
