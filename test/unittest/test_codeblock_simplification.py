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

        code = CodeBlockSimplifier(
            self.builder.constants, self.builder.references, self.builder.nodes
            )
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

        code = CodeBlockSimplifier(
            self.builder.constants, self.builder.references, self.builder.nodes
            )
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

        code = CodeBlockSimplifier(
            self.builder.constants, self.builder.references, self.builder.nodes
            )
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

    def test_unused_load_nonremoval(self):
        '''Test whether unused loads are kept for possible side effects.'''
        addr = self.builder.emitCompute(IntLiteral.create(0xD0D0))
        ridM = self.builder.addIOReference('mem', addr)
        loadM = self.builder.emitLoad(ridM)

        correct = (
            Load(loadM.cid, ridM),
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
        '''Test whether load and stores of variables are removed.'''
        ridA = self.builder.addRegister('a')
        ridV = self.builder.addValueArgument('V')
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

    def test_return_value(self):
        '''Test whether a return value constant is created correctly.'''
        ridA = self.builder.addRegister('a')
        ridV = self.builder.addValueArgument('V')
        ridRet = self.builder.addVariable('ret')
        loadA = self.builder.emitLoad(ridA)
        loadV = self.builder.emitLoad(ridV)
        add = self.builder.emitCompute(AddOperator(loadA, loadV))
        storeRet = self.builder.emitStore(ridRet, add)

        correct = (
            Load(loadA.cid, ridA),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)
        self.assertEqual(code.retCid, add.cid)

    def test_return_value_renumber(self):
        '''Test a simplification that must replace the return value cid.'''
        ridA = self.builder.addRegister('a')
        const = self.builder.emitCompute(IntLiteral.create(23))
        storeA = self.builder.emitStore(ridA, const)
        loadA = self.builder.emitLoad(ridA)
        outerRet = self.builder.addVariable('ret')
        storeRet = self.builder.emitStore(outerRet, loadA)

        correct = (
            Store(const.cid, ridA),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)
        self.assertEqual(code.retCid, const.cid)

    def test_repeated_increase(self):
        '''Test simplification of constants in constant expressions.'''
        ridA = self.builder.addRegister('a')
        def emitInc():
            loadA = self.builder.emitLoad(ridA)
            incA = self.builder.emitCompute(
                AddOperator(loadA, IntLiteral.create(1))
                )
            self.builder.emitStore(ridA, incA)

        initA = self.builder.emitCompute(IntLiteral.create(23))
        self.builder.emitStore(ridA, initA)
        emitInc()
        emitInc()
        emitInc()
        finalA = self.builder.emitLoad(ridA)
        ret = self.builder.addVariable('ret')
        self.builder.emitStore(ret, finalA)

        code = self.createSimplifiedCode()
        correct = (
            Store(code.retCid, ridA),
            )
        self.assertNodes(code.nodes, correct)
        self.assertIntLiteral(code.constants[code.retCid], 26)

if __name__ == '__main__':
    verbose = True
    unittest.main()
