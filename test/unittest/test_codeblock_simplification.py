from utils_codeblock import NodeChecker, TestCodeBlockBuilder

from retroasm.codeblock import Load, Store
from retroasm.codeblock_simplifier import CodeBlockSimplifier
from retroasm.expression import AddOperator, AndOperator, IntLiteral

import unittest

verbose = False

class CodeBlockTests(NodeChecker, unittest.TestCase):

    def setUp(self):
        self.builder = TestCodeBlockBuilder()

    def createSimplifiedCode(self):
        if verbose:
            print('=' * 40)
            self.builder.dump()
        code = self.builder.createCodeBlock()
        if verbose:
            print('-' * 40)
            code.dump()
        return code

    def test_no_change(self):
        '''Test whether a basic sequence survives a simplification attempt.'''
        sidA = self.builder.addRegister('a')
        loadA = self.builder.emitLoad(sidA)
        incA = self.builder.emitCompute(
            AddOperator(loadA, IntLiteral(1))
            )
        storeA = self.builder.emitStore(sidA, incA)

        correct = (
            Load(loadA.cid, sidA),
            Store(incA.cid, sidA),
            )

        code = self.builder.createCodeBlock()
        self.assertNodes(code.nodes, correct)
        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_duplicate_const(self):
        '''Test whether duplicate constants are removed.'''
        const1 = self.builder.emitCompute(IntLiteral(2))
        const2 = self.builder.emitCompute(
            AddOperator(IntLiteral(1), IntLiteral(1))
            )
        sidA = self.builder.addRegister('a')
        sidB = self.builder.addRegister('b')
        storeA = self.builder.emitStore(sidA, const1)
        storeB = self.builder.emitStore(sidB, const2)

        correct = (
            Store(const1.cid, sidA),
            Store(const1.cid, sidB),
            )

        code = CodeBlockSimplifier(
            self.builder.constants, self.builder.storages, self.builder.nodes
            )
        code.verify()
        code.simplifyConstants()
        code.verify()
        self.assertNodes(code.nodes, correct)

    def test_duplicate_register(self):
        '''Test whether duplicate registers are removed.'''
        sidA1 = self.builder.addRegister('a')
        sidA2 = self.builder.addRegister('a')
        sidB = self.builder.addRegister('b')
        sidC = self.builder.addRegister('c')
        loadA1 = self.builder.emitLoad(sidA1)
        loadA2 = self.builder.emitLoad(sidA2)
        storeB = self.builder.emitStore(sidB, loadA1)
        storeC = self.builder.emitStore(sidC, loadA2)

        correct = (
            Load(loadA1.cid, sidA1),
            Load(loadA2.cid, sidA1),
            Store(loadA1.cid, sidB),
            Store(loadA2.cid, sidC),
            )

        code = CodeBlockSimplifier(
            self.builder.constants, self.builder.storages, self.builder.nodes
            )
        code.verify()
        code.removeDuplicateStorages()
        code.verify()
        self.assertNodes(code.nodes, correct)

    def test_duplicate_iostorage(self):
        '''Test whether duplicate I/O storages are removed.'''
        sidM1 = self.builder.addIOStorage('mem', IntLiteral(0x8765))
        sidM2 = self.builder.addIOStorage('mem', IntLiteral(0x8765))
        sidM3 = self.builder.addIOStorage('mem', IntLiteral(0xABCD))
        sidM4 = self.builder.addIOStorage('io', IntLiteral(0x8765))
        loadM1 = self.builder.emitLoad(sidM1)
        loadM2 = self.builder.emitLoad(sidM2)
        storeM2 = self.builder.emitStore(sidM2, loadM1)
        storeM3 = self.builder.emitStore(sidM3, loadM2)
        storeM4 = self.builder.emitStore(sidM4, loadM1)

        correct = (
            Load(loadM1.cid, sidM1),
            Load(loadM2.cid, sidM1),
            Store(loadM1.cid, sidM1),
            Store(loadM2.cid, sidM3),
            Store(loadM1.cid, sidM4),
            )

        code = CodeBlockSimplifier(
            self.builder.constants, self.builder.storages, self.builder.nodes
            )
        code.verify()
        # Constants must be deduplicated to detect duplicate I/O indices.
        while code.simplifyConstants():
            pass
        code.verify()
        code.removeDuplicateStorages()
        code.verify()
        self.assertNodes(code.nodes, correct)

    def test_unused_load(self):
        '''Test whether unused loads are removed.'''
        sidA = self.builder.addRegister('a')
        loadA = self.builder.emitLoad(sidA)
        andA = self.builder.emitCompute(
            AndOperator(loadA, IntLiteral(0))
            )
        storeA = self.builder.emitStore(sidA, andA)

        correct = (
            Store(andA.cid, sidA),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_unused_load_nonremoval(self):
        '''Test whether unused loads are kept for possible side effects.'''
        addr = self.builder.emitCompute(IntLiteral(0xD0D0))
        sidM = self.builder.addIOStorage('mem', addr)
        loadM = self.builder.emitLoad(sidM)

        correct = (
            Load(loadM.cid, sidM),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_redundant_load_after_load(self):
        '''Test whether redundant successive loads are removed.'''
        sidA = self.builder.addRegister('a')
        sidB = self.builder.addRegister('b')
        sidC = self.builder.addRegister('c')
        loadA1 = self.builder.emitLoad(sidA)
        loadA2 = self.builder.emitLoad(sidA)
        storeB = self.builder.emitStore(sidB, loadA1)
        storeC = self.builder.emitStore(sidC, loadA2)

        correct = (
            Load(loadA1.cid, sidA),
            Store(loadA1.cid, sidB),
            Store(loadA1.cid, sidC),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_redundant_load_after_store(self):
        '''Test whether a redundant load after a store is removed.'''
        sidA = self.builder.addRegister('a')
        sidB = self.builder.addRegister('b')
        loadA1 = self.builder.emitLoad(sidA)
        incA = self.builder.emitCompute(AddOperator(loadA1, IntLiteral(1)))
        storeB = self.builder.emitStore(sidA, incA)
        loadA2 = self.builder.emitLoad(sidA)
        storeC = self.builder.emitStore(sidB, loadA2)

        correct = (
            Load(loadA1.cid, sidA),
            Store(incA.cid, sidA),
            Store(incA.cid, sidB),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_redundant_same_value_store(self):
        '''Test removal of storing the same value in the same storage twice.'''
        sidA = self.builder.addRegister('a')
        sidB = self.builder.addRegister('b')
        loadA = self.builder.emitLoad(sidA)
        storeB1 = self.builder.emitStore(sidB, loadA)
        storeB2 = self.builder.emitStore(sidB, loadA)

        correct = (
            Load(loadA.cid, sidA),
            Store(loadA.cid, sidB),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_redundant_other_value_store(self):
        '''Test removal of storing a different value in the same storage.'''
        sidA = self.builder.addRegister('a')
        sidB = self.builder.addRegister('b')
        sidC = self.builder.addRegister('c')
        loadA = self.builder.emitLoad(sidA)
        loadB = self.builder.emitLoad(sidB)
        storeC1 = self.builder.emitStore(sidC, loadA)
        storeC2 = self.builder.emitStore(sidC, loadB)

        correct = (
            Load(loadB.cid, sidB),
            Store(loadB.cid, sidC),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_uncertain_redundant_load(self):
        '''Test whether aliasing prevents loads from being removed.'''
        const = self.builder.emitCompute(IntLiteral(23))
        sidA = self.builder.addRegister('a')
        sidB = self.builder.addRegister('b')
        sidC = self.builder.addRegister('c')
        sidX = self.builder.addReferenceArgument('X')
        loadA1 = self.builder.emitLoad(sidA)
        storeX = self.builder.emitStore(sidX, const)
        loadA2 = self.builder.emitLoad(sidA)
        storeB = self.builder.emitStore(sidB, loadA1)
        storeC = self.builder.emitStore(sidC, loadA2)

        correct = (
            Load(loadA1.cid, sidA),
            Store(const.cid, sidX),
            Load(loadA2.cid, sidA),
            Store(loadA1.cid, sidB),
            Store(loadA2.cid, sidC),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_same_value_redundant_load(self):
        '''Test handling of writing the same value to a potential alias.'''
        sidA = self.builder.addRegister('a')
        sidB = self.builder.addRegister('b')
        sidX = self.builder.addReferenceArgument('X')
        loadA1 = self.builder.emitLoad(sidA)
        storeX = self.builder.emitStore(sidX, loadA1)
        loadA2 = self.builder.emitLoad(sidA)
        storeC = self.builder.emitStore(sidB, loadA2)

        correct = (
            Load(loadA1.cid, sidA),
            Store(loadA1.cid, sidX),
            Store(loadA1.cid, sidB),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_local_value(self):
        '''Test whether load and stores of variables are removed.'''
        sidA = self.builder.addRegister('a')
        sidV = self.builder.addValueArgument('V')
        loadV = self.builder.emitLoad(sidV)
        incV = self.builder.emitCompute(AddOperator(loadV, IntLiteral(1)))
        storeV = self.builder.emitStore(sidV, incV)
        storeA = self.builder.emitStore(sidA, incV)

        correct = (
            Store(incV.cid, sidA),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_unused_storage_removal(self):
        '''Test whether unused storages are removed.'''
        sidA = self.builder.addRegister('a')
        loadA = self.builder.emitLoad(sidA)
        sidM = self.builder.addIOStorage('mem', loadA)

        correct = (
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_return_value(self):
        '''Test whether a return value constant is created correctly.'''
        sidA = self.builder.addRegister('a')
        sidV = self.builder.addValueArgument('V')
        sidRet = self.builder.addVariable('ret')
        loadA = self.builder.emitLoad(sidA)
        loadV = self.builder.emitLoad(sidV)
        add = self.builder.emitCompute(AddOperator(loadA, loadV))
        storeRet = self.builder.emitStore(sidRet, add)

        correct = (
            Load(loadA.cid, sidA),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)
        retCid, retWidth = self.getRetVal(code)
        self.assertEqual(retCid, add.cid)
        self.assertEqual(retWidth, 8)

    def test_return_value_renumber(self):
        '''Test a simplification that must replace the return value cid.'''
        sidA = self.builder.addRegister('a')
        const = self.builder.emitCompute(IntLiteral(23))
        storeA = self.builder.emitStore(sidA, const)
        loadA = self.builder.emitLoad(sidA)
        outerRet = self.builder.addVariable('ret')
        storeRet = self.builder.emitStore(outerRet, loadA)

        correct = (
            Store(const.cid, sidA),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)
        retCid, retWidth = self.getRetVal(code)
        self.assertEqual(retCid, const.cid)
        self.assertEqual(retWidth, 8)

    def test_repeated_increase(self):
        '''Test simplification of constants in constant expressions.'''
        sidA = self.builder.addRegister('a')
        def emitInc():
            loadA = self.builder.emitLoad(sidA)
            incA = self.builder.emitCompute(AddOperator(loadA, IntLiteral(1)))
            self.builder.emitStore(sidA, incA)

        initA = self.builder.emitCompute(IntLiteral(23))
        self.builder.emitStore(sidA, initA)
        emitInc()
        emitInc()
        emitInc()
        finalA = self.builder.emitLoad(sidA)
        ret = self.builder.addVariable('ret')
        self.builder.emitStore(ret, finalA)

        code = self.createSimplifiedCode()
        retCid, retWidth = self.getRetVal(code)
        correct = (
            Store(retCid, sidA),
            )
        self.assertNodes(code.nodes, correct)
        self.assertRetVal(code, 26)
        self.assertEqual(retWidth, 8)

if __name__ == '__main__':
    verbose = True
    unittest.main()
