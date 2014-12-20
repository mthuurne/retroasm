from retroasm.expression import (
    AddOperator, AndOperator, IOChannel, IOReference, IntLiteral, IntType,
    LocalReference, LocalValue, Register
    )
from retroasm.func_parser import CodeBlockBuilder, Load, Store

import unittest

verbose = False

class TestCodeBlockBuilder(CodeBlockBuilder):

    def __init__(self):
        CodeBlockBuilder.__init__(self)
        self.channels = {}

    def addRegister(self, name, width=8):
        reg = Register(name, IntType(width))
        return self.emitReference(reg)

    def addLocalReference(self, name, width=8):
        ref = LocalReference(name, IntType(width))
        return self.emitReference(ref)

    def addLocalValue(self, name, width=8):
        ref = LocalValue(name, IntType(width))
        return self.emitReference(ref)

    def addIOReference(self, channelName, index, elemWidth=8, addrWidth=16):
        channel = self.channels.get(channelName)
        if channel is None:
            channel = IOChannel(
                channelName, IntType(elemWidth), IntType(addrWidth)
                )
            self.channels[channelName] = channel
        else:
            assert channel.elemType.width == elemWidth, channel
            assert channel.addrType.width == addrWidth, channel
        indexConst = self.emitCompute(index)
        ioref = IOReference(channel, indexConst)
        return self.emitReference(ioref)

class CodeBlockTests(unittest.TestCase):

    def assertNode(self, actual, correct):
        self.assertEqual(type(actual), type(correct))
        self.assertEqual(actual.cid, correct.cid)
        self.assertEqual(actual.rid, correct.rid)

    def assertNodes(self, actual, correct):
        self.assertEqual(len(actual), len(correct))
        for a, c in zip(actual, correct):
            self.assertNode(a, c)

    def setUp(self):
        self.builder = TestCodeBlockBuilder()

    def createSimplifiedCode(self):
        code = self.builder.createCodeBlock()
        if verbose:
            print('=' * 40)
            code.dump()
        code.verify()
        code.simplify()
        if verbose:
            print('-' * 40)
            code.dump()
        code.verify()
        return code

    def test_no_change(self):
        '''Test whether a basic sequence survives a simplification attempt.'''
        ridA = self.builder.addRegister('a')
        loadA = self.builder.emitLoad(ridA)
        incA = self.builder.emitCompute(
            AddOperator(loadA, IntLiteral.create(1))
            )
        storeA = self.builder.emitStore(ridA, incA)

        correct = (
            Load(loadA.cid, ridA),
            Store(incA.cid, ridA),
            )

        code = self.builder.createCodeBlock()
        self.assertNodes(code.nodes, correct)
        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_duplicate_const(self):
        '''Test whether duplicate constants are removed.'''
        const1 = self.builder.emitCompute(IntLiteral.create(2))
        const2 = self.builder.emitCompute(
            AddOperator(IntLiteral.create(1), IntLiteral.create(1))
            )
        ridA = self.builder.addRegister('a')
        ridB = self.builder.addRegister('b')
        storeA = self.builder.emitStore(ridA, const1)
        storeB = self.builder.emitStore(ridB, const2)

        correct = (
            Store(const1.cid, ridA),
            Store(const1.cid, ridB),
            )

        code = self.builder.createCodeBlock()
        code.verify()
        code.simplifyConstants()
        code.verify()
        self.assertNodes(code.nodes, correct)

    def test_duplicate_register(self):
        '''Test whether duplicate registers are removed.'''
        ridA1 = self.builder.addRegister('a')
        ridA2 = self.builder.addRegister('a')
        ridB = self.builder.addRegister('b')
        ridC = self.builder.addRegister('c')
        loadA1 = self.builder.emitLoad(ridA1)
        loadA2 = self.builder.emitLoad(ridA2)
        storeB = self.builder.emitStore(ridB, loadA1)
        storeC = self.builder.emitStore(ridC, loadA2)

        correct = (
            Load(loadA1.cid, ridA1),
            Load(loadA2.cid, ridA1),
            Store(loadA1.cid, ridB),
            Store(loadA2.cid, ridC),
            )

        code = self.builder.createCodeBlock()
        code.verify()
        code.removeDuplicateReferences()
        code.verify()
        self.assertNodes(code.nodes, correct)

    def test_duplicate_ioref(self):
        '''Test whether duplicate I/O references are removed.'''
        ridM1 = self.builder.addIOReference('mem', IntLiteral.create(0x8765))
        ridM2 = self.builder.addIOReference('mem', IntLiteral.create(0x8765))
        ridM3 = self.builder.addIOReference('mem', IntLiteral.create(0xABCD))
        ridM4 = self.builder.addIOReference('io', IntLiteral.create(0x8765))
        loadM1 = self.builder.emitLoad(ridM1)
        loadM2 = self.builder.emitLoad(ridM2)
        storeM2 = self.builder.emitStore(ridM2, loadM1)
        storeM3 = self.builder.emitStore(ridM3, loadM2)
        storeM4 = self.builder.emitStore(ridM4, loadM1)

        correct = (
            Load(loadM1.cid, ridM1),
            Load(loadM2.cid, ridM1),
            Store(loadM1.cid, ridM1),
            Store(loadM2.cid, ridM3),
            Store(loadM1.cid, ridM4),
            )

        code = self.builder.createCodeBlock()
        code.verify()
        # Constants must be deduplicated to detect duplicate I/O indices.
        while code.simplifyConstants():
            pass
        code.verify()
        code.removeDuplicateReferences()
        code.verify()
        self.assertNodes(code.nodes, correct)

    def test_unused_load(self):
        '''Test whether unused loads are removed.'''
        ridA = self.builder.addRegister('a')
        loadA = self.builder.emitLoad(ridA)
        andA = self.builder.emitCompute(
            AndOperator(loadA, IntLiteral.create(0))
            )
        storeA = self.builder.emitStore(ridA, andA)

        correct = (
            Store(andA.cid, ridA),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_redundant_load_after_load(self):
        '''Test whether redundant successive loads are removed.'''
        ridA = self.builder.addRegister('a')
        ridB = self.builder.addRegister('b')
        ridC = self.builder.addRegister('c')
        loadA1 = self.builder.emitLoad(ridA)
        loadA2 = self.builder.emitLoad(ridA)
        storeB = self.builder.emitStore(ridB, loadA1)
        storeC = self.builder.emitStore(ridC, loadA2)

        correct = (
            Load(loadA1.cid, ridA),
            Store(loadA1.cid, ridB),
            Store(loadA1.cid, ridC),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_redundant_load_after_store(self):
        '''Test whether a redundant load after a store is removed.'''
        ridA = self.builder.addRegister('a')
        ridB = self.builder.addRegister('b')
        loadA1 = self.builder.emitLoad(ridA)
        incA = self.builder.emitCompute(
            AddOperator(loadA1, IntLiteral.create(1))
            )
        storeB = self.builder.emitStore(ridA, incA)
        loadA2 = self.builder.emitLoad(ridA)
        storeC = self.builder.emitStore(ridB, loadA2)

        correct = (
            Load(loadA1.cid, ridA),
            Store(incA.cid, ridA),
            Store(incA.cid, ridB),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_redundant_same_value_store(self):
        '''Test removal of storing the same value in the same storage twice.'''
        ridA = self.builder.addRegister('a')
        ridB = self.builder.addRegister('b')
        loadA = self.builder.emitLoad(ridA)
        storeB1 = self.builder.emitStore(ridB, loadA)
        storeB2 = self.builder.emitStore(ridB, loadA)

        correct = (
            Load(loadA.cid, ridA),
            Store(loadA.cid, ridB),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_redundant_other_value_store(self):
        '''Test removal of storing a different value in the same storage.'''
        ridA = self.builder.addRegister('a')
        ridB = self.builder.addRegister('b')
        ridC = self.builder.addRegister('c')
        loadA = self.builder.emitLoad(ridA)
        loadB = self.builder.emitLoad(ridB)
        storeC1 = self.builder.emitStore(ridC, loadA)
        storeC2 = self.builder.emitStore(ridC, loadB)

        correct = (
            Load(loadB.cid, ridB),
            Store(loadB.cid, ridC),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_uncertain_redundant_load(self):
        '''Test whether aliasing prevents loads from being removed.'''
        const = self.builder.emitCompute(IntLiteral.create(23))
        ridA = self.builder.addRegister('a')
        ridB = self.builder.addRegister('b')
        ridC = self.builder.addRegister('c')
        ridX = self.builder.addLocalReference('X')
        loadA1 = self.builder.emitLoad(ridA)
        storeX = self.builder.emitStore(ridX, const)
        loadA2 = self.builder.emitLoad(ridA)
        storeB = self.builder.emitStore(ridB, loadA1)
        storeC = self.builder.emitStore(ridC, loadA2)

        correct = (
            Load(loadA1.cid, ridA),
            Store(const.cid, ridX),
            Load(loadA2.cid, ridA),
            Store(loadA1.cid, ridB),
            Store(loadA2.cid, ridC),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_same_value_redundant_load(self):
        '''Test handling of writing the same value to a potential alias.'''
        const = self.builder.emitCompute(IntLiteral.create(23))
        ridA = self.builder.addRegister('a')
        ridB = self.builder.addRegister('b')
        ridX = self.builder.addLocalReference('X')
        loadA1 = self.builder.emitLoad(ridA)
        storeX = self.builder.emitStore(ridX, loadA1)
        loadA2 = self.builder.emitLoad(ridA)
        storeC = self.builder.emitStore(ridB, loadA2)

        correct = (
            Load(loadA1.cid, ridA),
            Store(loadA1.cid, ridX),
            Store(loadA1.cid, ridB),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_local_value(self):
        '''Test whether load and stores of local values are removed.'''
        const = self.builder.emitCompute(IntLiteral.create(23))
        ridA = self.builder.addRegister('a')
        ridV = self.builder.addLocalValue('V')
        loadV = self.builder.emitLoad(ridV)
        incV = self.builder.emitCompute(
            AddOperator(loadV, IntLiteral.create(1))
            )
        storeV = self.builder.emitStore(ridV, incV)
        storeA = self.builder.emitStore(ridA, incV)

        correct = (
            Store(incV.cid, ridA),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_unused_reference_removal(self):
        '''Test whether unused references are removed.'''
        ridA = self.builder.addRegister('a')
        loadA = self.builder.emitLoad(ridA)
        ridM = self.builder.addIOReference('mem', loadA)

        correct = (
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

if __name__ == '__main__':
    verbose = True
    unittest.main()
