from retroasm.expression import (
    IOChannel, IOReference, IntLiteral, IntType, LocalValue, LocalReference,
    Register
    )

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
        a = Register('a', IntType(8))
        b = Register('b', IntType(8))
        l = LocalValue('L', IntType(8))
        r = LocalReference('R', IntType(8))
        mem = IOChannel('mem', IntType(8), IntType(16))
        m = IOReference(mem, IntLiteral.create(0xC000))
        self.assertAlias(a, a)
        self.assertNoAlias(a, b)
        self.assertNoAlias(a, l)
        self.assertAlias(a, r)
        self.assertNoAlias(a, m)

    def test_local_value_aliasing(self):
        '''Test when local values might be aliased.'''
        l = LocalValue('L', IntType(8))
        l2 = LocalValue('L2', IntType(8))
        a = Register('a', IntType(8))
        r = LocalReference('R', IntType(8))
        mem = IOChannel('mem', IntType(8), IntType(16))
        m = IOReference(mem, IntLiteral.create(0xC000))
        self.assertAlias(l, l)
        self.assertNoAlias(l, l2)
        self.assertNoAlias(l, a)
        self.assertNoAlias(l, r)
        self.assertNoAlias(l, m)

    def test_local_reference_aliasing(self):
        '''Test when local references might be aliased.'''
        r = LocalReference('R', IntType(8))
        r2 = LocalReference('R2', IntType(8))
        a = Register('a', IntType(8))
        l = LocalValue('L', IntType(8))
        mem = IOChannel('mem', IntType(8), IntType(16))
        m = IOReference(mem, IntLiteral.create(0xC000))
        self.assertAlias(r, r)
        self.assertAlias(r, r2)
        self.assertAlias(r, a)
        self.assertNoAlias(r, l)
        self.assertAlias(r, m)

    def test_io_aliasing(self):
        '''Test when I/O references might be aliased.'''
        a = Register('a', IntType(8))
        l = LocalValue('L', IntType(8))
        r = LocalReference('R', IntType(8))
        mem = IOChannel('mem', IntType(8), IntType(16))
        io = IOChannel('io', IntType(8), IntType(16))
        m = IOReference(mem, IntLiteral.create(0xC000))
        i = IOReference(io, IntLiteral.create(0xC000))
        m2 = IOReference(mem, IntLiteral.create(0xE000))
        self.assertAlias(m, m)
        self.assertAlias(m, m2)
        self.assertNoAlias(m, i)
        self.assertNoAlias(m, a)
        self.assertNoAlias(m, l)
        self.assertAlias(m, r)

if __name__ == '__main__':
    unittest.main()
