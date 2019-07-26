from retroasm.expression import IntLiteral
from retroasm.storage import ArgStorage, IOChannel, IOStorage, Variable
from retroasm.types import IntType

import unittest

class AliasTests(unittest.TestCase):

    def assertAlias(self, storage1, storage2):
        if not storage1.mightBeSame(storage2):
            self.fail('%s does not alias %s' % (storage1, storage2))
        if not storage2.mightBeSame(storage1):
            self.fail('%s does alias %s, but not vice versa'
                    % (storage1, storage2))

    def assertNoAlias(self, storage1, storage2):
        if storage1.mightBeSame(storage2):
            self.fail('%s does alias %s' % (storage1, storage2))
        if storage2.mightBeSame(storage1):
            self.fail('%s does not alias %s, but vice versa it does'
                    % (storage1, storage2))

    def test_register_aliasing(self):
        '''Test when registers might be aliased.'''
        a = Variable(8, 0)
        b = Variable(8, 0)
        l = Variable(8, 1)
        r = ArgStorage('R', 8)
        mem = IOChannel('mem', IntType.u(8), IntType.u(16))
        m = IOStorage(mem, IntLiteral(0xC000))
        self.assertAlias(a, a)
        self.assertNoAlias(a, b)
        self.assertNoAlias(a, l)
        self.assertAlias(a, r)
        self.assertNoAlias(a, m)

    def test_variable_aliasing(self):
        '''Test when variables might be aliased.'''
        l = Variable(8, 1)
        l2 = Variable(8, 1)
        a = Variable(8, 0)
        r = ArgStorage('R', 8)
        mem = IOChannel('mem', IntType.u(8), IntType.u(16))
        m = IOStorage(mem, IntLiteral(0xC000))
        self.assertAlias(l, l)
        self.assertNoAlias(l, l2)
        self.assertNoAlias(l, a)
        self.assertNoAlias(l, r)
        self.assertNoAlias(l, m)

    def test_unknown_storage_aliasing(self):
        '''Test when unknown storages might be aliased.'''
        r = ArgStorage('R', 8)
        r2 = ArgStorage('R2', 8)
        a = Variable(8, 0)
        l = Variable(8, 1)
        mem = IOChannel('mem', IntType.u(8), IntType.u(16))
        m = IOStorage(mem, IntLiteral(0xC000))
        self.assertAlias(r, r)
        self.assertAlias(r, r2)
        self.assertAlias(r, a)
        self.assertNoAlias(r, l)
        self.assertAlias(r, m)

    def test_io_aliasing(self):
        '''Test when I/O storages might be aliased.'''
        a = Variable(8, 0)
        l = Variable(8, 1)
        r = ArgStorage('R', 8)
        mem = IOChannel('mem', IntType.u(8), IntType.u(16))
        io = IOChannel('io', IntType.u(8), IntType.u(16))
        m = IOStorage(mem, IntLiteral(0xC000))
        i = IOStorage(io, IntLiteral(0xC000))
        m2 = IOStorage(mem, IntLiteral(0xE000))
        self.assertAlias(m, m)
        self.assertAlias(m, m2)
        self.assertNoAlias(m, i)
        self.assertNoAlias(m, a)
        self.assertNoAlias(m, l)
        self.assertAlias(m, r)

if __name__ == '__main__':
    unittest.main()
