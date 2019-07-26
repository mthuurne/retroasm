from utils_codeblock import NodeChecker, TestNamespace
from utils_expression import TestExprMixin, makeConcat

from retroasm.codeblock import Load, Store
from retroasm.codeblock_simplifier import CodeBlockSimplifier
from retroasm.expression import (
    AddOperator, AndOperator, IntLiteral, OrOperator, truncate
    )
from retroasm.expression_simplifier import simplifyExpression
from retroasm.reference import (
    ConcatenatedBits, FixedValue, Reference, SingleStorage, SlicedBits
    )
from retroasm.storage import IOStorage
from retroasm.types import IntType

import unittest

verbose = False

class CodeBlockTests(NodeChecker, TestExprMixin, unittest.TestCase):

    def setUp(self):
        self.namespace = TestNamespace()

    def createSimplifiedCode(self):
        namespace = self.namespace
        if verbose:
            print('=' * 40)
            namespace.dump()
        retRef = namespace.elements['ret'] if 'ret' in namespace else None
        code = namespace.createCodeBlock(retRef)
        if verbose:
            print('-' * 40)
            code.dump()
        return code

    def test_no_change(self):
        '''Test whether a basic sequence survives a simplification attempt.'''
        refA = self.namespace.addRegister('a')
        refB = self.namespace.addRegister('b')
        loadA = self.namespace.emitLoad(refA)
        self.namespace.emitStore(refB, loadA)

        def checkNodes(code):
            self.assertEqual(len(code.nodes), 2)
            load, store = code.nodes
            self.assertIsInstance(load, Load)
            self.assertIsInstance(store, Store)
            self.assertEqual(load.storage, refA.bits.storage)
            self.assertEqual(store.storage, refB.bits.storage)
            self.assertIs(store.expr, load.expr)

        code = self.namespace.createCodeBlock(None)
        checkNodes(code)
        code = self.createSimplifiedCode()
        checkNodes(code)

    def test_stored_expression(self):
        '''Test whether stored expressions are simplified.'''
        const1 = IntLiteral(2)
        const2 = AddOperator(IntLiteral(1), IntLiteral(1))
        refA = self.namespace.addRegister('a')
        refB = self.namespace.addRegister('b')
        self.namespace.emitStore(refA, const1)
        self.namespace.emitStore(refB, const2)

        correct = (
            Store(const1, refA.bits.storage),
            Store(const1, refB.bits.storage),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_unused_load(self):
        '''Test whether unused loads are removed.'''
        refA = self.namespace.addRegister('a')
        loadA = self.namespace.emitLoad(refA)
        andA = AndOperator(loadA, IntLiteral(0))
        self.namespace.emitStore(refA, andA)

        code = self.createSimplifiedCode()
        self.assertEqual(len(code.nodes), 1)
        node = code.nodes[0]
        self.assertIsInstance(node, Store)
        self.assertIs(node.storage, refA.bits.storage)
        self.assertIntLiteral(node.expr, 0)

    def test_unused_load_nonremoval(self):
        '''Test whether unused loads are kept for possible side effects.'''
        addr = IntLiteral(0xD0D0)
        refM = self.namespace.addIOStorage('mem', addr)
        loadM = self.namespace.emitLoad(refM)

        correct = (
            Load(refM.bits.storage),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_redundant_load_after_load(self):
        '''Test whether redundant successive loads are removed.'''
        refA = self.namespace.addRegister('a')
        refB = self.namespace.addRegister('b')
        refC = self.namespace.addRegister('c')
        loadA1 = self.namespace.emitLoad(refA)
        loadA2 = self.namespace.emitLoad(refA)
        self.namespace.emitStore(refB, loadA1)
        self.namespace.emitStore(refC, loadA2)

        def correct():
            loadA = Load(refA.bits.storage)
            yield loadA
            yield Store(loadA.expr, refB.bits.storage)
            yield Store(loadA.expr, refC.bits.storage)

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct())

    def test_redundant_load_after_store(self):
        '''Test whether a redundant load after a store is removed.'''
        refA = self.namespace.addRegister('a')
        refB = self.namespace.addRegister('b')
        loadA1 = self.namespace.emitLoad(refA)
        incA = AddOperator(loadA1, IntLiteral(1))
        self.namespace.emitStore(refA, incA)
        loadA2 = self.namespace.emitLoad(refA)
        self.namespace.emitStore(refB, loadA2)

        code = self.createSimplifiedCode()
        self.assertEqual(len(code.nodes), 3)
        load, store1, store2 = code.nodes
        self.assertIsInstance(load, Load)
        self.assertIsInstance(store1, Store)
        self.assertIsInstance(store2, Store)
        self.assertEqual(load.storage, refA.bits.storage)
        self.assertEqual(store1.storage, refA.bits.storage)
        self.assertEqual(store2.storage, refB.bits.storage)
        self.assertEqual(store1.expr, store2.expr)
        self.assertTrunc(
            simplifyExpression(store1.expr),
            simplifyExpression(incA).substitute(
                lambda expr: load.expr if expr is loadA1 else None
                ),
            incA.mask.bit_length(),
            refA.width
            )

    def test_redundant_same_value_store(self):
        '''Test removal of storing the same value in the same storage twice.'''
        refA = self.namespace.addRegister('a')
        refB = self.namespace.addRegister('b')
        loadA = self.namespace.emitLoad(refA)
        self.namespace.emitStore(refB, loadA)
        self.namespace.emitStore(refB, loadA)

        def correct():
            loadA = Load(refA.bits.storage)
            yield loadA
            yield Store(loadA.expr, refB.bits.storage)

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct())

    def test_redundant_other_value_store(self):
        '''Test removal of storing a different value in the same storage.'''
        refA = self.namespace.addRegister('a')
        refB = self.namespace.addRegister('b')
        refC = self.namespace.addRegister('c')
        loadA = self.namespace.emitLoad(refA)
        loadB = self.namespace.emitLoad(refB)
        self.namespace.emitStore(refC, loadA)
        self.namespace.emitStore(refC, loadB)

        def correct():
            loadB = Load(refB.bits.storage)
            yield loadB
            yield Store(loadB.expr, refC.bits.storage)

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct())

    def test_uncertain_redundant_load(self):
        '''Test whether aliasing prevents loads from being removed.'''
        const = IntLiteral(23)
        refA = self.namespace.addRegister('a')
        refB = self.namespace.addRegister('b')
        refC = self.namespace.addRegister('c')
        refX = self.namespace.addArgument('X')
        loadA1 = self.namespace.emitLoad(refA)
        self.namespace.emitStore(refX, const)
        loadA2 = self.namespace.emitLoad(refA)
        self.namespace.emitStore(refB, loadA1)
        self.namespace.emitStore(refC, loadA2)

        def correct():
            loadA1 = Load(refA.bits.storage)
            yield loadA1
            yield Store(const, refX.bits.storage)
            loadA2 = Load(refA.bits.storage)
            yield loadA2
            yield Store(loadA1.expr, refB.bits.storage)
            yield Store(loadA2.expr, refC.bits.storage)

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct())

    def test_same_value_redundant_load(self):
        '''Test handling of writing the same value to a potential alias.'''
        refA = self.namespace.addRegister('a')
        refB = self.namespace.addRegister('b')
        refX = self.namespace.addArgument('X')
        loadA1 = self.namespace.emitLoad(refA)
        self.namespace.emitStore(refX, loadA1)
        loadA2 = self.namespace.emitLoad(refA)
        self.namespace.emitStore(refB, loadA2)

        def correct():
            loadA = Load(refA.bits.storage)
            yield loadA
            yield Store(loadA.expr, refX.bits.storage)
            yield Store(loadA.expr, refB.bits.storage)

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct())

    def test_local_value(self):
        '''Test whether load and stores of variables are removed.'''
        refA = self.namespace.addRegister('a')
        refB = self.namespace.addRegister('b')
        refV = self.namespace.addVariable('V')
        loadA = self.namespace.emitLoad(refA)
        self.namespace.emitStore(refV, loadA)
        loadV = self.namespace.emitLoad(refV)
        self.namespace.emitStore(refB, loadV)

        def correct():
            loadA = Load(refA.bits.storage)
            yield loadA
            yield Store(loadA.expr, refB.bits.storage)

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct())

    def test_unused_storage_removal(self):
        '''Test whether unused storages are removed.'''
        refA = self.namespace.addRegister('a')
        loadA = self.namespace.emitLoad(refA)
        refM = self.namespace.addIOStorage('mem', loadA)

        correct = (
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_return_value(self):
        '''Test whether a return value is stored correctly.'''
        refV = self.namespace.addArgument('V')
        refA = self.namespace.addRegister('a')
        refRet = self.namespace.addVariable('ret')
        loadA = self.namespace.emitLoad(refA)
        loadV = self.namespace.emitLoad(refV)
        combined = OrOperator(loadA, loadV)
        self.namespace.emitStore(refRet, combined)

        correct = (
            Load(refA.bits.storage),
            Load(refV.bits.storage),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)
        retVal, retWidth = self.getRetVal(code)
        valueV = code.nodes[0].expr
        valueA = code.nodes[1].expr
        self.assertEqual(retWidth, 8)
        self.assertOr(
            retVal,
            simplifyExpression(valueA),
            simplifyExpression(valueV)
            )

    def test_retbits_override(self):
        '''Test code block creation with a non-default returned bit string.'''
        refV = self.namespace.addVariable('V', IntType.u(20))
        value = IntLiteral(604)
        self.namespace.emitStore(refV, value)

        code = self.namespace.createCodeBlock(refV)
        self.assertEqual(len(code.returned), 1)
        retBits,= code.returned
        self.assertEqual(retBits.width, 20)
        self.assertRetVal(code, 604)

    def test_return_io_index(self):
        '''Test returning an I/O reference with a simplifiable index.'''
        addr = AddOperator(IntLiteral(1), IntLiteral(1))
        memByte = self.namespace.addIOStorage('mem', addr)
        self.namespace.addRetReference(memByte)

        code = self.createSimplifiedCode()
        self.assertEqual(len(code.returned), 1)
        retBits,= code.returned
        self.assertIsInstance(retBits, SingleStorage)
        self.assertEqual(retBits.width, 8)
        storage = retBits.storage
        self.assertIsInstance(storage, IOStorage)
        self.assertIntLiteral(storage.index, 2)

    def test_return_redundant_load_index(self):
        '''Test returning a redundant loaded value.'''
        refA = self.namespace.addRegister('a', IntType.u(16))
        self.namespace.emitStore(refA, IntLiteral(0x4120))
        loadA = self.namespace.emitLoad(refA)
        memByte = self.namespace.addIOStorage('mem', loadA)
        self.namespace.addRetReference(memByte)

        code = self.createSimplifiedCode()
        self.assertEqual(len(code.returned), 1)
        retBits,= code.returned
        self.assertIsInstance(retBits, SingleStorage)
        self.assertEqual(retBits.width, 8)
        storage = retBits.storage
        self.assertIsInstance(storage, IOStorage)
        self.assertIntLiteral(storage.index, 0x4120)

    def test_return_fixed_value_ref(self):
        '''Test returning a reference to a fixed value.'''
        add = AddOperator(IntLiteral(1), IntLiteral(2))
        value = FixedValue(add, 8)
        self.namespace.addRetReference(Reference(value, IntType.u(8)))
        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, ())
        self.assertEqual(len(code.returned), 1)
        retBits,= code.returned
        self.assertIsInstance(retBits, FixedValue)
        self.assertEqual(retBits.width, 8)
        self.assertIntLiteral(retBits.expr, 3)

    def test_return_complex_ref(self):
        '''Test returning a non-trivial reference.'''
        refH = self.namespace.addRegister('h')
        refL = self.namespace.addRegister('l')
        bitsHL = ConcatenatedBits(refL.bits, refH.bits)
        slicedBits = SlicedBits(bitsHL, IntLiteral(0), 8)
        self.namespace.addRetReference(Reference(slicedBits, IntType.u(8)))
        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, ())
        self.assertEqual(len(code.returned), 1)
        retBits,= code.returned
        self.assertEqual(retBits.width, 8)
        # Note that we only simplify expressions, not references, so the
        # reference itself is still complex. All we really check here is
        # that code block creation doesn't break, but that is worthwhile
        # in itself.

    def run_repeated_increase(self, counterRef, counterRemains):
        '''Helper method for repeated increase tests.'''
        def emitInc():
            loadCounter= self.namespace.emitLoad(counterRef)
            incA = AddOperator(loadCounter, IntLiteral(1))
            self.namespace.emitStore(counterRef, incA)

        initCounter = IntLiteral(23)
        self.namespace.emitStore(counterRef, initCounter)
        emitInc()
        emitInc()
        emitInc()
        finalCounter = self.namespace.emitLoad(counterRef)
        ret = self.namespace.addVariable('ret')
        self.namespace.emitStore(ret, finalCounter)

        code = self.createSimplifiedCode()
        retVal, retWidth = self.getRetVal(code)
        correct = []
        if counterRemains:
            correct.insert(0, Store(retVal, counterRef.bits.storage))
        self.assertNodes(code.nodes, correct)
        self.assertRetVal(code, 26)
        self.assertEqual(retWidth, 8)

    def test_repeated_increase_reg(self):
        '''Test removal of redundant loads and stores to a register.'''
        refA = self.namespace.addRegister('a')
        self.run_repeated_increase(refA, True)

    def test_repeated_increase_var(self):
        '''Test removal of redundant loads and stores to a local variable.'''
        refA = self.namespace.addVariable('A')
        self.run_repeated_increase(refA, False)

    def run_signed_load(self, write, compare):
        '''Helper method for signed load tests.'''
        signedVar = self.namespace.addVariable('s', IntType.s(8))
        self.namespace.emitStore(signedVar, IntLiteral(write))
        loaded = self.namespace.emitLoad(signedVar)
        ret = self.namespace.addVariable('ret', IntType.int)
        self.namespace.emitStore(ret, loaded)

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

        signedVar = self.namespace.addVariable('s', IntType.int)
        self.namespace.emitStore(signedVar, IntLiteral(987654321))
        loaded = self.namespace.emitLoad(signedVar)
        ret = self.namespace.addVariable('ret', IntType.int)
        self.namespace.emitStore(ret, loaded)

        code = self.createSimplifiedCode()
        self.assertRetVal(code, 987654321)

    def test_6502_pull(self):
        '''Test simplification of the 6502 PULL instructions.'''

        refD = self.namespace.addArgument('D')
        refS = self.namespace.addRegister('s')
        loadS1 = self.namespace.emitLoad(refS)
        incS = AddOperator(loadS1, IntLiteral(1))
        self.namespace.emitStore(refS, incS)
        const1 = IntLiteral(1)
        loadS2 = self.namespace.emitLoad(refS)
        refM = self.namespace.addIOStorage('mem', makeConcat(const1, loadS2, 8))
        loadM = self.namespace.emitLoad(refM)
        self.namespace.emitStore(refD, loadM)

        code = self.createSimplifiedCode()
        ioStorage, = (
            node.storage
            for node in code.nodes
            if isinstance(node.storage, IOStorage)
            )
        def correct():
            loadS = Load(refS.bits.storage)
            yield loadS
            expr = truncate(incS, 8).substitute(
                lambda expr: loadS.expr if expr is loadS1 else None
                )
            yield Store(expr, refS.bits.storage)
            loadM = Load(ioStorage)
            yield loadM
            yield Store(loadM.expr, refD.bits.storage)
        self.assertNodes(code.nodes, correct())

if __name__ == '__main__':
    verbose = True
    unittest.main()
