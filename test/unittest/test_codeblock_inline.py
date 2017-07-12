from utils_codeblock import NodeChecker, TestCodeBlockBuilder

from retroasm.codeblock import (
    ConcatenatedReference, FixedValue, Load, SlicedReference, Store
    )
from retroasm.expression import AddOperator, IntLiteral
from retroasm.function import Function
from retroasm.types import IntType

import unittest

verbose = False

def createSimplifiedCode(builder):
    if verbose:
        print('=' * 40)
        builder.dump()
    code = builder.createCodeBlock()
    if verbose:
        print('-' * 40)
        code.dump()
    return code

class CodeBlockInlineTests(NodeChecker, unittest.TestCase):

    def test_easy(self):
        '''Test whether inlining works when there are no complications.'''
        inner = TestCodeBlockBuilder()
        innerA = inner.addRegister('a', IntType.u(16))
        const = IntLiteral(12345)
        inner.emitStore(innerA, const)

        # Share the global namespace to make sure that the outer and inner block
        # are using the same registers.
        outer = TestCodeBlockBuilder(inner.globalBuilder)
        outerA = outer.addRegister('a', IntType.u(16))
        zero = IntLiteral(0)
        outer.emitStore(outerA, zero)
        outer.inlineBlock(inner.createCodeBlock(), {})
        loadA = outer.emitLoad(outerA)
        outerRet = outer.addVariable('ret', IntType.u(16))
        outer.emitStore(outerRet, loadA)

        code = createSimplifiedCode(outer)
        retVal, retWidth = self.getRetVal(code)
        correct = (
            Store(retVal, outerA.storage),
            Store(retVal, outerRet.storage),
            )
        self.assertNodes(code.nodes, correct)
        self.assertRetVal(code, 12345)
        self.assertEqual(retWidth, 16)

    def test_arg_ret(self):
        '''Test whether inlining works with an argument and return value.'''
        inc = TestCodeBlockBuilder()
        incArgRef = inc.addValueArgument('V')
        incArgVal = inc.emitLoad(incArgRef)
        incAdd = AddOperator(incArgVal, IntLiteral(1))
        incRet = inc.addVariable('ret')
        inc.emitStore(incRet, incAdd)
        incCode = inc.createCodeBlock()

        outer = TestCodeBlockBuilder()
        step0 = IntLiteral(100)
        step1 = outer.emitLoad(outer.inlineBlock(incCode, {'V': step0}))
        step2 = outer.emitLoad(outer.inlineBlock(incCode, {'V': step1}))
        step3 = outer.emitLoad(outer.inlineBlock(incCode, {'V': step2}))
        outerRet = outer.addVariable('ret')
        outer.emitStore(outerRet, step3)

        code = createSimplifiedCode(outer)
        retVal, retWidth = self.getRetVal(code)
        correct = (
            Store(retVal, outerRet.storage),
            )
        self.assertNodes(code.nodes, correct)
        self.assertRetVal(code, 103)
        self.assertEqual(retWidth, 8)

    def test_multiret(self):
        '''Test whether inlining works when "ret" is written multiple times.'''
        inner = TestCodeBlockBuilder()
        val0 = IntLiteral(1000)
        val1 = IntLiteral(2000)
        val2 = IntLiteral(3000)
        innerRet = inner.addVariable('ret', IntType.u(16))
        inner.emitStore(innerRet, val0)
        inner.emitStore(innerRet, val1)
        inner.emitStore(innerRet, val2)
        innerCode = inner.createCodeBlock()

        outer = TestCodeBlockBuilder()
        inlinedVal = outer.emitLoad(outer.inlineBlock(innerCode, {}))
        outerRet = outer.addVariable('ret', IntType.u(16))
        outer.emitStore(outerRet, inlinedVal)

        code = createSimplifiedCode(outer)
        retVal, retWidth = self.getRetVal(code)
        correct = (
            Store(retVal, outerRet.storage),
            )
        self.assertNodes(code.nodes, correct)
        self.assertRetVal(code, 3000)
        self.assertEqual(retWidth, 16)

    def test_ret_truncate(self):
        '''Test whether the value returned by a block is truncated.'''
        inner = TestCodeBlockBuilder()
        innerVal = IntLiteral(0x8472)
        innerRet = inner.addVariable('ret')
        inner.emitStore(innerRet, innerVal)
        innerCode = inner.createCodeBlock()
        func = Function('get', IntType.u(8), {}, innerCode)

        outer = TestCodeBlockBuilder()
        outerVal = outer.emitLoad(outer.inlineFunctionCall(func, {}, None))
        outerRet = outer.addVariable('ret', IntType.u(16))
        outer.emitStore(outerRet, outerVal)

        code = createSimplifiedCode(outer)
        retVal, retWidth = self.getRetVal(code)
        correct = (
            Store(retVal, outerRet.storage),
            )
        self.assertNodes(code.nodes, correct)
        self.assertRetVal(code, 0x72)
        self.assertEqual(retWidth, 16)

    def test_arg_truncate(self):
        '''Test whether expressions passed via value arguments are truncated.'''
        # Note: Default width is 8 bits.
        inc = TestCodeBlockBuilder()
        incArgRef = inc.addValueArgument('V')
        incArgVal = inc.emitLoad(incArgRef)
        incAdd = AddOperator(incArgVal, IntLiteral(1))
        incRet = inc.addVariable('ret')
        inc.emitStore(incRet, incAdd)
        incCode = inc.createCodeBlock()
        func = Function('inc', IntType.u(9), {'V': IntType.u(8)}, incCode)

        outer = TestCodeBlockBuilder()
        step0 = IntLiteral(0x89FE)
        step1 = outer.emitLoad(outer.inlineBlock(incCode, {'V': step0}))
        step2 = outer.emitLoad(outer.inlineBlock(incCode, {'V': step1}))
        step3 = outer.emitLoad(outer.inlineBlock(incCode, {'V': step2}))
        outerRet = outer.addVariable('ret', IntType.u(16))
        outer.emitStore(outerRet, step3)

        code = createSimplifiedCode(outer)
        retVal, retWidth = self.getRetVal(code)
        correct = (
            Store(retVal, outerRet.storage),
            )
        self.assertNodes(code.nodes, correct)
        self.assertRetVal(code, 1)
        self.assertEqual(retWidth, 16)

    def test_pass_by_reference(self):
        '''Test whether pass-by-reference arguments work correctly.'''
        inc = TestCodeBlockBuilder()
        incArgRef = inc.addReferenceArgument('R')
        incArgVal = inc.emitLoad(incArgRef)
        incAdd = AddOperator(incArgVal, IntLiteral(1))
        inc.emitStore(incArgRef, incAdd)
        incCode = inc.createCodeBlock()

        outer = TestCodeBlockBuilder()
        outerA = outer.addRegister('a')
        regA = outer.namespace['a']
        initA = IntLiteral(100)
        outer.emitStore(outerA, initA)
        outer.inlineBlock(incCode, {'R': regA})
        outer.inlineBlock(incCode, {'R': regA})
        outer.inlineBlock(incCode, {'R': regA})
        outerRet = outer.addVariable('ret')
        finalA = outer.emitLoad(outerA)
        outer.emitStore(outerRet, finalA)

        code = createSimplifiedCode(outer)
        code.verify()
        retVal, retWidth = self.getRetVal(code)
        correct = (
            Store(retVal, outerA.storage),
            Store(retVal, outerRet.storage),
            )
        self.assertNodes(code.nodes, correct)
        self.assertRetVal(code, 103)
        self.assertEqual(retWidth, 8)

    def test_pass_concat_by_reference(self):
        '''Test concatenated storages as pass-by-reference arguments.'''
        inc = TestCodeBlockBuilder()
        incArgRef = inc.addReferenceArgument('R', IntType.u(16))
        incArgVal = inc.emitLoad(incArgRef)
        incAdd = AddOperator(incArgVal, IntLiteral(0x1234))
        inc.emitStore(incArgRef, incAdd)
        incCode = inc.createCodeBlock()

        outer = TestCodeBlockBuilder()
        outerH = outer.addRegister('h')
        outerL = outer.addRegister('l')
        regH = outer.namespace['h']
        regL = outer.namespace['l']
        regHL = ConcatenatedReference(regL, regH)

        initH = IntLiteral(0xab)
        initL = IntLiteral(0xcd)
        outer.emitStore(outerH, initH)
        outer.emitStore(outerL, initL)
        outer.inlineBlock(incCode, {'R': regHL})
        outer.inlineBlock(incCode, {'R': regHL})
        outer.inlineBlock(incCode, {'R': regHL})
        outerRet = outer.addVariable('ret', IntType.u(16))
        finalHL = outer.emitLoad(regHL)
        outer.emitStore(outerRet, finalHL)

        code = createSimplifiedCode(outer)
        code.verify()
        self.assertEqual(len(code.nodes), 3)
        self.assertRetVal(code, 0xabcd + 3 * 0x1234)

    def test_pass_concat_fixed_by_reference(self):
        '''Test concatenated storages arguments containing FixedValues.'''
        inc = TestCodeBlockBuilder()
        incArgRef = inc.addReferenceArgument('R', IntType.u(16))
        incArgVal = inc.emitLoad(incArgRef)
        incAdd = AddOperator(incArgVal, IntLiteral(0x1234))
        inc.emitStore(incArgRef, incAdd)
        incCode = inc.createCodeBlock()

        outer = TestCodeBlockBuilder()
        outerH = outer.addRegister('h')
        outerL = FixedValue(IntLiteral(0xcd), IntType.u(8))
        regH = outer.namespace['h']
        regHL = ConcatenatedReference(outerL, regH)

        initH = IntLiteral(0xab)
        outer.emitStore(outerH, initH)
        outer.inlineBlock(incCode, {'R': regHL})
        outer.inlineBlock(incCode, {'R': regHL})
        outer.inlineBlock(incCode, {'R': regHL})
        outerRet = outer.addVariable('ret', IntType.u(16))
        finalHL = outer.emitLoad(regHL)
        outer.emitStore(outerRet, finalHL)

        code = createSimplifiedCode(outer)
        code.verify()
        self.assertEqual(len(code.nodes), 2)
        self.assertRetVal(code, 0xabcd + 3 * 0x1300)

    def test_pass_slice_by_reference(self):
        '''Test sliced storages as pass-by-reference arguments.'''
        inc = TestCodeBlockBuilder()
        incArgRef = inc.addReferenceArgument('R')
        incArgVal = inc.emitLoad(incArgRef)
        incAdd = AddOperator(incArgVal, IntLiteral(0x12))
        inc.emitStore(incArgRef, incAdd)
        incCode = inc.createCodeBlock()

        outer = TestCodeBlockBuilder()
        outerR = outer.addRegister('r', IntType.u(16))
        regR = outer.namespace['r']
        initR = IntLiteral(0xcdef)
        outer.emitStore(outerR, initR)
        sliceR = SlicedReference(regR, IntLiteral(4), IntLiteral(8))
        outer.inlineBlock(incCode, {'R': sliceR})
        outer.inlineBlock(incCode, {'R': sliceR})
        outer.inlineBlock(incCode, {'R': sliceR})
        outerRet = outer.addVariable('ret', IntType.u(16))
        finalR = outer.emitLoad(outerR)
        outer.emitStore(outerRet, finalR)

        code = createSimplifiedCode(outer)
        code.verify()
        self.assertEqual(len(code.nodes), 2)
        self.assertRetVal(code, 0xc00f | (((0xde + 3 * 0x12) & 0xff) << 4))

    def test_inline_unsigned_reg(self):
        '''Test reading of an unsigned register.'''
        inner = TestCodeBlockBuilder()
        innerA = inner.addRegister('a')
        innerLoad = inner.emitLoad(innerA)
        innerRet = inner.addVariable('ret', IntType.u(16))
        inner.emitStore(innerRet, innerLoad)
        innerCode = inner.createCodeBlock()

        outer = TestCodeBlockBuilder(inner.globalBuilder)
        outerA = outer.addRegister('a')
        initA = IntLiteral(0xb2)
        outer.emitStore(outerA, initA)
        retRef = outer.inlineBlock(innerCode, {})
        outerRet = outer.addVariable('ret', IntType.u(16))
        retVal = outer.emitLoad(retRef)
        outer.emitStore(outerRet, retVal)

        code = createSimplifiedCode(outer)
        code.verify()
        self.assertEqual(len(code.nodes), 2)
        self.assertRetVal(code, 0x00b2)

    def test_inline_signed_reg(self):
        '''Test reading of a signed register.'''
        inner = TestCodeBlockBuilder()
        innerA = inner.addRegister('a', IntType.s(8))
        innerLoad = inner.emitLoad(innerA)
        innerRet = inner.addVariable('ret', IntType.u(16))
        inner.emitStore(innerRet, innerLoad)
        innerCode = inner.createCodeBlock()

        outer = TestCodeBlockBuilder(inner.globalBuilder)
        outerA = outer.addRegister('a', IntType.s(8))
        initA = IntLiteral(0xb2)
        outer.emitStore(outerA, initA)
        retRef = outer.inlineBlock(innerCode, {})
        outerRet = outer.addVariable('ret', IntType.u(16))
        retVal = outer.emitLoad(retRef)
        outer.emitStore(outerRet, retVal)

        code = createSimplifiedCode(outer)
        code.verify()
        self.assertEqual(len(code.nodes), 2)
        self.assertRetVal(code, 0xffb2)

    def test_load_from_unsigned_reference_arg(self):
        '''Test reading of a value passed via an unsigned reference.'''
        inner = TestCodeBlockBuilder()
        argRef = inner.addReferenceArgument('R')
        argVal = inner.emitLoad(argRef)
        innerRet = inner.addVariable('ret', IntType.u(16))
        inner.emitStore(innerRet, argVal)
        innerCode = inner.createCodeBlock()

        outer = TestCodeBlockBuilder()
        fixedVal = FixedValue(IntLiteral(0xa4), IntType.u(8))
        retRef = outer.inlineBlock(innerCode, {'R': fixedVal})
        outerRet = outer.addVariable('ret', IntType.u(16))
        retVal = outer.emitLoad(retRef)
        outer.emitStore(outerRet, retVal)

        code = createSimplifiedCode(outer)
        code.verify()
        self.assertEqual(len(code.nodes), 1)
        self.assertRetVal(code, 0x00a4)

    def test_load_from_signed_reference_arg(self):
        '''Test reading of a value passed via a signed reference.'''
        inner = TestCodeBlockBuilder()
        argRef = inner.addReferenceArgument('R', IntType.s(8))
        argVal = inner.emitLoad(argRef)
        innerRet = inner.addVariable('ret', IntType.u(16))
        inner.emitStore(innerRet, argVal)
        innerCode = inner.createCodeBlock()

        outer = TestCodeBlockBuilder()
        fixedVal = FixedValue(IntLiteral(0xa4), IntType.u(8))
        retRef = outer.inlineBlock(innerCode, {'R': fixedVal})
        outerRet = outer.addVariable('ret', IntType.u(16))
        retVal = outer.emitLoad(retRef)
        outer.emitStore(outerRet, retVal)

        code = createSimplifiedCode(outer)
        code.verify()
        self.assertEqual(len(code.nodes), 1)
        self.assertRetVal(code, 0xffa4)

    def test_return_simple_reference(self):
        '''Test returning a reference to a global.'''
        inner = TestCodeBlockBuilder()
        innerA = inner.addRegister('a')
        inner.addRetReference(innerA)
        innerCode = inner.createCodeBlock()
        self.assertIsNotNone(innerCode.retRef)

        outer = TestCodeBlockBuilder(inner.globalBuilder)
        retRef = outer.inlineBlock(innerCode, {})
        outerA = outer.addRegister('a')
        fake = IntLiteral(0xdc)
        outer.emitStore(outerA, fake)
        value = IntLiteral(0xba)
        outer.emitStore(retRef, value)
        outerRet = outer.addVariable('ret')
        retVal = outer.emitLoad(outerA)
        outer.emitStore(outerRet, retVal)

        code = createSimplifiedCode(outer)
        code.verify()
        self.assertEqual(len(code.nodes), 2)
        self.assertRetVal(code, 0xba)

    def test_return_io_reference(self):
        '''Test returning a reference to an index in an I/O channel.'''
        inner = TestCodeBlockBuilder()
        addrArg = inner.addValueArgument('A', IntType.u(16))
        addrVal = inner.emitLoad(addrArg)
        memByte = inner.addIOStorage('mem', addrVal)
        inner.addRetReference(memByte)
        innerCode = inner.createCodeBlock()
        self.assertIsNotNone(innerCode.retRef)

        outer = TestCodeBlockBuilder()
        addr = IntLiteral(0x4002)
        retRef = outer.inlineBlock(innerCode, {'A': addr})
        outerRet = outer.addVariable('ret')
        retVal = outer.emitLoad(retRef)
        outer.emitStore(outerRet, retVal)

        code = createSimplifiedCode(outer)
        retVal, retWidth = self.getRetVal(code)
        def correct():
            load = Load(retRef.storage)
            yield load
            addrVal = code.nodes[0].expr
            expr = retVal.substitute(
                lambda expr: load.expr if expr is addrVal else None
                )
            yield Store(expr, outerRet.storage)
        self.assertNodes(code.nodes, correct())
        self.assertEqual(retWidth, 8)

if __name__ == '__main__':
    verbose = True
    unittest.main()
