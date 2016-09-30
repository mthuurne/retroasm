from utils_codeblock import NodeChecker, TestCodeBlockBuilder

from retroasm.codeblock import ComputedConstant, Load, Store
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
        refA = self.builder.addRegister('a')
        loadA = refA.emitLoad(self.builder, None)
        incA = self.builder.emitCompute(
            AddOperator(loadA, IntLiteral(1))
            )
        refA.emitStore(self.builder, incA, None)

        cidA = self.getCid(loadA)
        sidA = self.getSid(refA)
        def checkNodes(code):
            self.assertEqual(len(code.nodes), 2)
            load, store = code.nodes
            self.assertIsInstance(load, Load)
            self.assertIsInstance(store, Store)
            self.assertEqual(load.sid, sidA)
            self.assertEqual(store.sid, sidA)
            self.assertEqual(load.cid, cidA)
            self.assertIsInstance(code.constants[store.cid], ComputedConstant)

        code = self.builder.createCodeBlock()
        checkNodes(code)
        code = self.createSimplifiedCode()
        checkNodes(code)

    def test_duplicate_const(self):
        '''Test whether duplicate constants are removed.'''
        const1 = self.builder.emitCompute(IntLiteral(2))
        const2 = self.builder.emitCompute(
            AddOperator(IntLiteral(1), IntLiteral(1))
            )
        refA = self.builder.addRegister('a')
        refB = self.builder.addRegister('b')
        refA.emitStore(self.builder, const1, None)
        refB.emitStore(self.builder, const2, None)

        sidA = self.getSid(refA)
        sidB = self.getSid(refB)
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

    def test_duplicate_iostorage(self):
        '''Test whether duplicate I/O storages are removed.'''
        sidM1 = self.builder.addIOStorage('mem', IntLiteral(0x8765))
        sidM2 = self.builder.addIOStorage('mem', IntLiteral(0x8765))
        sidM3 = self.builder.addIOStorage('mem', IntLiteral(0xABCD))
        sidM4 = self.builder.addIOStorage('io', IntLiteral(0x8765))
        loadM1 = self.builder.emitLoad(sidM1)
        loadM2 = self.builder.emitLoad(sidM2)
        self.builder.emitStore(sidM2, loadM1)
        self.builder.emitStore(sidM3, loadM2)
        self.builder.emitStore(sidM4, loadM1)

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
        refA = self.builder.addRegister('a')
        loadA = refA.emitLoad(self.builder, None)
        andA = self.builder.emitCompute(
            AndOperator(loadA, IntLiteral(0))
            )
        refA.emitStore(self.builder, andA, None)

        sidA = self.getSid(refA)
        code = self.createSimplifiedCode()
        self.assertEqual(len(code.nodes), 1)
        node = code.nodes[0]
        self.assertIsInstance(node, Store)
        self.assertEqual(node.sid, sidA)
        self.assertIntLiteral(code.constants[node.cid], 0)

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
        refA = self.builder.addRegister('a')
        refB = self.builder.addRegister('b')
        refC = self.builder.addRegister('c')
        loadA1 = refA.emitLoad(self.builder, None)
        loadA2 = refA.emitLoad(self.builder, None)
        refB.emitStore(self.builder, loadA1, None)
        refC.emitStore(self.builder, loadA2, None)

        cidA1 = self.getCid(loadA1)
        sidA = self.getSid(refA)
        sidB = self.getSid(refB)
        sidC = self.getSid(refC)
        correct = (
            Load(cidA1, sidA),
            Store(cidA1, sidB),
            Store(cidA1, sidC),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_redundant_load_after_store(self):
        '''Test whether a redundant load after a store is removed.'''
        refA = self.builder.addRegister('a')
        refB = self.builder.addRegister('b')
        loadA1 = refA.emitLoad(self.builder, None)
        incA = self.builder.emitCompute(AddOperator(loadA1, IntLiteral(1)))
        refA.emitStore(self.builder, incA, None)
        loadA2 = refA.emitLoad(self.builder, None)
        refB.emitStore(self.builder, loadA2, None)

        cidA1 = self.getCid(loadA1)
        sidA = self.getSid(refA)
        sidB = self.getSid(refB)

        code = self.createSimplifiedCode()
        self.assertEqual(len(code.nodes), 3)
        load, store1, store2 = code.nodes
        self.assertIsInstance(load, Load)
        self.assertIsInstance(store1, Store)
        self.assertIsInstance(store2, Store)
        self.assertEqual(load.sid, sidA)
        self.assertEqual(store1.sid, sidA)
        self.assertEqual(store2.sid, sidB)
        self.assertEqual(load.cid, cidA1)
        self.assertEqual(store1.cid, store2.cid)
        self.assertIsInstance(code.constants[store1.cid], ComputedConstant)

    def test_redundant_same_value_store(self):
        '''Test removal of storing the same value in the same storage twice.'''
        refA = self.builder.addRegister('a')
        refB = self.builder.addRegister('b')
        loadA = refA.emitLoad(self.builder, None)
        refB.emitStore(self.builder, loadA, None)
        refB.emitStore(self.builder, loadA, None)

        cidA = self.getCid(loadA)
        sidA = self.getSid(refA)
        sidB = self.getSid(refB)
        correct = (
            Load(cidA, sidA),
            Store(cidA, sidB),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_redundant_other_value_store(self):
        '''Test removal of storing a different value in the same storage.'''
        refA = self.builder.addRegister('a')
        refB = self.builder.addRegister('b')
        refC = self.builder.addRegister('c')
        loadA = refA.emitLoad(self.builder, None)
        loadB = refB.emitLoad(self.builder, None)
        refC.emitStore(self.builder, loadA, None)
        refC.emitStore(self.builder, loadB, None)

        cidB = self.getCid(loadB)
        sidB = self.getSid(refB)
        sidC = self.getSid(refC)
        correct = (
            Load(cidB, sidB),
            Store(cidB, sidC),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_uncertain_redundant_load(self):
        '''Test whether aliasing prevents loads from being removed.'''
        const = self.builder.emitCompute(IntLiteral(23))
        refA = self.builder.addRegister('a')
        refB = self.builder.addRegister('b')
        refC = self.builder.addRegister('c')
        sidX = self.builder.addReferenceArgument('X')
        loadA1 = refA.emitLoad(self.builder, None)
        self.builder.emitStore(sidX, const)
        loadA2 = refA.emitLoad(self.builder, None)
        refB.emitStore(self.builder, loadA1, None)
        refC.emitStore(self.builder, loadA2, None)

        cidA1 = self.getCid(loadA1)
        cidA2 = self.getCid(loadA2)
        sidA = self.getSid(refA)
        sidB = self.getSid(refB)
        sidC = self.getSid(refC)
        correct = (
            Load(cidA1, sidA),
            Store(const.cid, sidX),
            Load(cidA2, sidA),
            Store(cidA1, sidB),
            Store(cidA2, sidC),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_same_value_redundant_load(self):
        '''Test handling of writing the same value to a potential alias.'''
        refA = self.builder.addRegister('a')
        refB = self.builder.addRegister('b')
        sidX = self.builder.addReferenceArgument('X')
        loadA1 = refA.emitLoad(self.builder, None)
        self.builder.emitStore(sidX, loadA1)
        loadA2 = refA.emitLoad(self.builder, None)
        refB.emitStore(self.builder, loadA2, None)

        cidA1 = self.getCid(loadA1)
        sidA = self.getSid(refA)
        sidB = self.getSid(refB)
        correct = (
            Load(cidA1, sidA),
            Store(cidA1, sidX),
            Store(cidA1, sidB),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_local_value(self):
        '''Test whether load and stores of variables are removed.'''
        refA = self.builder.addRegister('a')
        sidV = self.builder.addValueArgument('V')
        loadV = self.builder.emitLoad(sidV)
        incV = self.builder.emitCompute(AddOperator(loadV, IntLiteral(1)))
        self.builder.emitStore(sidV, incV)
        refA.emitStore(self.builder, incV, None)

        sidA = self.getSid(refA)

        code = self.createSimplifiedCode()
        self.assertEqual(len(code.nodes), 1)
        node = code.nodes[0]
        self.assertIsInstance(node, Store)
        self.assertEqual(node.sid, sidA)
        self.assertIsInstance(code.constants[node.cid], ComputedConstant)

    def test_unused_storage_removal(self):
        '''Test whether unused storages are removed.'''
        refA = self.builder.addRegister('a')
        loadA = refA.emitLoad(self.builder, None)
        sidM = self.builder.addIOStorage('mem', loadA)

        correct = (
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_return_value(self):
        '''Test whether a return value constant is created correctly.'''
        refA = self.builder.addRegister('a')
        sidV = self.builder.addValueArgument('V')
        sidRet = self.builder.addVariable('ret')
        loadA = refA.emitLoad(self.builder, None)
        loadV = self.builder.emitLoad(sidV)
        add = self.builder.emitCompute(AddOperator(loadA, loadV))
        self.builder.emitStore(sidRet, add)

        cidA = self.getCid(loadA)
        sidA = self.getSid(refA)
        correct = (
            Load(cidA, sidA),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)
        retCid, retWidth = self.getRetVal(code)
        self.assertEqual(retCid, add.cid)
        self.assertEqual(retWidth, 8)

    def test_return_value_renumber(self):
        '''Test a simplification that must replace the return value cid.'''
        refA = self.builder.addRegister('a')
        const = self.builder.emitCompute(IntLiteral(23))
        refA.emitStore(self.builder, const, None)
        loadA = refA.emitLoad(self.builder, None)
        outerRet = self.builder.addVariable('ret')
        self.builder.emitStore(outerRet, loadA)

        sidA = self.getSid(refA)

        code = self.createSimplifiedCode()
        self.assertEqual(len(code.nodes), 1)
        node = code.nodes[0]
        self.assertIsInstance(node, Store)
        self.assertEqual(node.sid, sidA)
        self.assertIntLiteral(code.constants[node.cid], 23)
        retCid, retWidth = self.getRetVal(code)
        self.assertEqual(retCid, node.cid)
        self.assertEqual(retWidth, 8)

    def test_repeated_increase(self):
        '''Test simplification of constants in constant expressions.'''
        refA = self.builder.addRegister('a')
        def emitInc():
            loadA = refA.emitLoad(self.builder, None)
            incA = self.builder.emitCompute(AddOperator(loadA, IntLiteral(1)))
            refA.emitStore(self.builder, incA, None)

        initA = self.builder.emitCompute(IntLiteral(23))
        refA.emitStore(self.builder, initA, None)
        emitInc()
        emitInc()
        emitInc()
        finalA = refA.emitLoad(self.builder, None)
        ret = self.builder.addVariable('ret')
        self.builder.emitStore(ret, finalA)

        code = self.createSimplifiedCode()
        retCid, retWidth = self.getRetVal(code)
        sidA = self.getSid(refA)
        correct = (
            Store(retCid, sidA),
            )
        self.assertNodes(code.nodes, correct)
        self.assertRetVal(code, 26)
        self.assertEqual(retWidth, 8)

if __name__ == '__main__':
    verbose = True
    unittest.main()
