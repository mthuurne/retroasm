from utils_codeblock import NodeChecker, TestCodeBlockBuilder
from utils_expression import TestExprMixin, makeConcat

from retroasm.codeblock import (
    ArgumentValue, ConstantValue, Load, SingleReference, Store, inlineConstants
    )
from retroasm.codeblock_simplifier import CodeBlockSimplifier
from retroasm.expression import (
    AddOperator, AndOperator, IntLiteral, OrOperator, truncate
    )
from retroasm.expression_simplifier import simplifyExpression
from retroasm.storage import IOStorage
from retroasm.types import IntType

import unittest

verbose = False

class CodeBlockTests(NodeChecker, TestExprMixin, unittest.TestCase):

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
        refB = self.builder.addRegister('b')
        loadA = self.builder.emitLoad(refA)
        self.builder.emitStore(refB, loadA)

        def checkNodes(code):
            self.assertEqual(len(code.nodes), 2)
            load, store = code.nodes
            self.assertIsInstance(load, Load)
            self.assertIsInstance(store, Store)
            self.assertEqual(load.storage, refA.storage)
            self.assertEqual(store.storage, refB.storage)
            self.assertEqual(load.expr, loadA)
            self.assertEqual(store.expr, loadA)

        code = self.builder.createCodeBlock()
        checkNodes(code)
        code = self.createSimplifiedCode()
        checkNodes(code)

    def test_stored_expression(self):
        '''Test whether stored expressions are simplified.'''
        const1 = IntLiteral(2)
        const2 = AddOperator(IntLiteral(1), IntLiteral(1))
        refA = self.builder.addRegister('a')
        refB = self.builder.addRegister('b')
        self.builder.emitStore(refA, const1)
        self.builder.emitStore(refB, const2)

        correct = (
            Store(const1, refA.storage),
            Store(const1, refB.storage),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_duplicate_iostorage(self):
        '''Test whether duplicate I/O storages are removed.'''
        refM1 = self.builder.addIOStorage('mem', IntLiteral(0x8765))
        refM2 = self.builder.addIOStorage('mem', IntLiteral(0x8765))
        refM3 = self.builder.addIOStorage('mem', IntLiteral(0xABCD))
        refM4 = self.builder.addIOStorage('io', IntLiteral(0x8765))
        loadM1 = self.builder.emitLoad(refM2)
        loadM2 = self.builder.emitLoad(refM2)
        self.builder.emitStore(refM2, loadM1)
        self.builder.emitStore(refM3, loadM2)
        self.builder.emitStore(refM4, loadM1)

        correct = (
            Load(loadM1, refM1.storage),
            Load(loadM2, refM1.storage),
            Store(loadM1, refM1.storage),
            Store(loadM2, refM3.storage),
            Store(loadM1, refM4.storage),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_unused_load(self):
        '''Test whether unused loads are removed.'''
        refA = self.builder.addRegister('a')
        loadA = self.builder.emitLoad(refA)
        andA = AndOperator(loadA, IntLiteral(0))
        self.builder.emitStore(refA, andA)

        code = self.createSimplifiedCode()
        self.assertEqual(len(code.nodes), 1)
        node = code.nodes[0]
        self.assertIsInstance(node, Store)
        self.assertIs(node.storage, refA.storage)
        self.assertIntLiteral(inlineConstants(node.expr, code.constants), 0)

    def test_unused_load_nonremoval(self):
        '''Test whether unused loads are kept for possible side effects.'''
        addr = IntLiteral(0xD0D0)
        refM = self.builder.addIOStorage('mem', addr)
        loadM = self.builder.emitLoad(refM)

        correct = (
            Load(loadM, refM.storage),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_redundant_load_after_load(self):
        '''Test whether redundant successive loads are removed.'''
        refA = self.builder.addRegister('a')
        refB = self.builder.addRegister('b')
        refC = self.builder.addRegister('c')
        loadA1 = self.builder.emitLoad(refA)
        loadA2 = self.builder.emitLoad(refA)
        self.builder.emitStore(refB, loadA1)
        self.builder.emitStore(refC, loadA2)

        correct = (
            Load(loadA1, refA.storage),
            Store(loadA1, refB.storage),
            Store(loadA1, refC.storage),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_redundant_load_after_store(self):
        '''Test whether a redundant load after a store is removed.'''
        refA = self.builder.addRegister('a')
        refB = self.builder.addRegister('b')
        loadA1 = self.builder.emitLoad(refA)
        incA = AddOperator(loadA1, IntLiteral(1))
        self.builder.emitStore(refA, incA)
        loadA2 = self.builder.emitLoad(refA)
        self.builder.emitStore(refB, loadA2)

        code = self.createSimplifiedCode()
        self.assertEqual(len(code.nodes), 3)
        load, store1, store2 = code.nodes
        self.assertIsInstance(load, Load)
        self.assertIsInstance(store1, Store)
        self.assertIsInstance(store2, Store)
        self.assertEqual(load.storage, refA.storage)
        self.assertEqual(store1.storage, refA.storage)
        self.assertEqual(store2.storage, refB.storage)
        self.assertEqual(load.expr, loadA1)
        self.assertEqual(store1.expr, store2.expr)
        self.assertTrunc(
            simplifyExpression(inlineConstants(store1.expr, code.constants)),
            simplifyExpression(inlineConstants(incA, self.builder.constants)),
            incA.mask.bit_length(),
            refA.width
            )

    def test_redundant_same_value_store(self):
        '''Test removal of storing the same value in the same storage twice.'''
        refA = self.builder.addRegister('a')
        refB = self.builder.addRegister('b')
        loadA = self.builder.emitLoad(refA)
        self.builder.emitStore(refB, loadA)
        self.builder.emitStore(refB, loadA)

        correct = (
            Load(loadA, refA.storage),
            Store(loadA, refB.storage),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_redundant_other_value_store(self):
        '''Test removal of storing a different value in the same storage.'''
        refA = self.builder.addRegister('a')
        refB = self.builder.addRegister('b')
        refC = self.builder.addRegister('c')
        loadA = self.builder.emitLoad(refA)
        loadB = self.builder.emitLoad(refB)
        self.builder.emitStore(refC, loadA)
        self.builder.emitStore(refC, loadB)

        correct = (
            Load(loadB, refB.storage),
            Store(loadB, refC.storage),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_uncertain_redundant_load(self):
        '''Test whether aliasing prevents loads from being removed.'''
        const = IntLiteral(23)
        refA = self.builder.addRegister('a')
        refB = self.builder.addRegister('b')
        refC = self.builder.addRegister('c')
        refX = self.builder.addReferenceArgument('X')
        loadA1 = self.builder.emitLoad(refA)
        self.builder.emitStore(refX, const)
        loadA2 = self.builder.emitLoad(refA)
        self.builder.emitStore(refB, loadA1)
        self.builder.emitStore(refC, loadA2)

        correct = (
            Load(loadA1, refA.storage),
            Store(const, refX.storage),
            Load(loadA2, refA.storage),
            Store(loadA1, refB.storage),
            Store(loadA2, refC.storage),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_same_value_redundant_load(self):
        '''Test handling of writing the same value to a potential alias.'''
        refA = self.builder.addRegister('a')
        refB = self.builder.addRegister('b')
        refX = self.builder.addReferenceArgument('X')
        loadA1 = self.builder.emitLoad(refA)
        self.builder.emitStore(refX, loadA1)
        loadA2 = self.builder.emitLoad(refA)
        self.builder.emitStore(refB, loadA2)

        correct = (
            Load(loadA1, refA.storage),
            Store(loadA1, refX.storage),
            Store(loadA1, refB.storage),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_local_value(self):
        '''Test whether load and stores of variables are removed.'''
        refA = self.builder.addRegister('a')
        refV = self.builder.addValueArgument('V')
        loadV = self.builder.emitLoad(refV)
        incV = AddOperator(loadV, IntLiteral(1))
        self.builder.emitStore(refV, incV)
        self.builder.emitStore(refA, incV)

        code = self.createSimplifiedCode()
        self.assertEqual(len(code.nodes), 1)
        node = code.nodes[0]
        self.assertIsInstance(node, Store)
        self.assertIs(node.storage, refA.storage)
        self.assertTrunc(
            inlineConstants(node.expr, code.constants),
            AddOperator(ArgumentValue('V', 255), IntLiteral(1)),
            incV.mask.bit_length(),
            refA.width
            )

    def test_unused_storage_removal(self):
        '''Test whether unused storages are removed.'''
        refA = self.builder.addRegister('a')
        loadA = self.builder.emitLoad(refA)
        refM = self.builder.addIOStorage('mem', loadA)

        correct = (
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_return_value(self):
        '''Test whether a return value constant is created correctly.'''
        refV = self.builder.addValueArgument('V')
        self.assertEqual(len(self.builder.nodes), 1)
        self.assertIsInstance(self.builder.nodes[0], Store)
        valueV = simplifyExpression(self.builder.nodes[0].expr)

        refA = self.builder.addRegister('a')
        refRet = self.builder.addVariable('ret')
        loadA = self.builder.emitLoad(refA)
        loadV = self.builder.emitLoad(refV)
        combined = OrOperator(loadA, loadV)
        self.builder.emitStore(refRet, combined)

        code = self.createSimplifiedCode()
        retVal, retWidth = self.getRetVal(code)
        correct = (
            Load(loadA, refA.storage),
            Store(retVal, refRet.storage),
            )
        self.assertNodes(code.nodes, correct)
        self.assertEqual(retWidth, 8)
        self.assertOr(
            inlineConstants(retVal, code.constants),
            simplifyExpression(loadA),
            valueV
            )

    def test_return_value_renumber(self):
        '''Test a simplification that must replace the return value cid.'''
        refA = self.builder.addRegister('a')
        const = IntLiteral(23)
        self.builder.emitStore(refA, const)
        loadA = self.builder.emitLoad(refA)
        outerRet = self.builder.addVariable('ret')
        self.builder.emitStore(outerRet, loadA)

        code = self.createSimplifiedCode()
        self.assertEqual(len(code.nodes), 2)
        node = code.nodes[0]
        self.assertIsInstance(node, Store)
        self.assertIs(node.storage, refA.storage)
        self.assertIntLiteral(inlineConstants(node.expr, code.constants), 23)
        retVal, retWidth = self.getRetVal(code)
        self.assertEqual(retVal, node.expr)
        self.assertEqual(retWidth, 8)

    def test_return_io_index(self):
        '''Test returning an I/O reference with a simplifiable index.'''
        addr = AddOperator(IntLiteral(1), IntLiteral(1))
        memByte = self.builder.addIOStorage('mem', addr)
        self.builder.addRetReference(memByte)

        code = self.createSimplifiedCode()
        self.assertIsInstance(code.retRef, SingleReference)
        self.assertIs(code.retRef.type, IntType.u(8))
        storage = code.retRef.storage
        self.assertIsInstance(storage, IOStorage)
        self.assertIntLiteral(storage.index, 2)

    def test_repeated_increase(self):
        '''Test simplification of constants in constant expressions.'''
        refA = self.builder.addRegister('a')
        def emitInc():
            loadA = self.builder.emitLoad(refA)
            incA = AddOperator(loadA, IntLiteral(1))
            self.builder.emitStore(refA, incA)

        initA = IntLiteral(23)
        self.builder.emitStore(refA, initA)
        emitInc()
        emitInc()
        emitInc()
        finalA = self.builder.emitLoad(refA)
        ret = self.builder.addVariable('ret')
        self.builder.emitStore(ret, finalA)

        code = self.createSimplifiedCode()
        retVal, retWidth = self.getRetVal(code)
        correct = (
            Store(retVal, refA.storage),
            Store(retVal, ret.storage),
            )
        self.assertNodes(code.nodes, correct)
        self.assertRetVal(code, 26)
        self.assertEqual(retWidth, 8)

    def run_signed_load(self, write, compare):
        '''Helper method for signed load tests.'''
        signedVar = self.builder.addVariable('s', IntType.s(8))
        self.builder.emitStore(signedVar, IntLiteral(write))
        loaded = self.builder.emitLoad(signedVar)
        ret = self.builder.addVariable('ret', IntType.int)
        self.builder.emitStore(ret, loaded)

        code = self.createSimplifiedCode()
        self.assertRetVal(code, compare)

    def test_signed_load_positive(self):
        '''Test loading of a positive signed integer.'''
        self.run_signed_load(56, 56)

    def test_signed_load_negative(self):
        '''Test loading of a negative signed integer.'''
        self.run_signed_load(-78, -78)

    def test_signed_load_wrap(self):
        '''Test loading of an unsigned integer as signed.'''
        self.run_signed_load(135, 135 - 256)

    def test_signed_load_unlimited(self):
        '''Test loading of an unlimited width integer.'''

        # No sign extension should be happening here, but the code once
        # contained a bug where it would try to apply sign extension and
        # triggered an internal consistency check.

        signedVar = self.builder.addVariable('s', IntType.int)
        self.builder.emitStore(signedVar, IntLiteral(987654321))
        loaded = self.builder.emitLoad(signedVar)
        ret = self.builder.addVariable('ret', IntType.int)
        self.builder.emitStore(ret, loaded)

        code = self.createSimplifiedCode()
        self.assertRetVal(code, 987654321)

    def test_6502_pull(self):
        '''Test simplification of the 6502 PULL instructions.'''

        refD = self.builder.addReferenceArgument('D')
        refS = self.builder.addRegister('s')
        loadS1 = self.builder.emitLoad(refS)
        incS = AddOperator(loadS1, IntLiteral(1))
        self.builder.emitStore(refS, incS)
        const1 = IntLiteral(1)
        loadS2 = self.builder.emitLoad(refS)
        refM = self.builder.addIOStorage('mem', makeConcat(const1, loadS2, 8))
        loadM = self.builder.emitLoad(refM)
        self.builder.emitStore(refD, loadM)

        code = self.createSimplifiedCode()
        ioStorage, = (
            node.storage
            for node in code.nodes
            if isinstance(node.storage, IOStorage)
            )
        correct = (
            Load(loadS1, refS.storage),
            Store(truncate(incS, 8), refS.storage),
            Load(loadM, ioStorage),
            Store(loadM, refD.storage),
            )
        self.assertNodes(code.nodes, correct)

if __name__ == '__main__':
    verbose = True
    unittest.main()
