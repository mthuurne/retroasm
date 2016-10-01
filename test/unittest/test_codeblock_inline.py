from utils_codeblock import NodeChecker, TestCodeBlockBuilder

from retroasm.codeblock import BoundReference, Store
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
        innerA = inner.addRegister('a', 16)
        const = inner.emitCompute(IntLiteral(12345))
        inner.emitStore(innerA, const)

        # Share the global context to make sure that the outer and inner block
        # are using the same registers.
        outer = TestCodeBlockBuilder(inner.globalBuilder)
        outerA = outer.addRegister('a', 16)
        zero = outer.emitCompute(IntLiteral(0))
        outer.emitStore(outerA, zero)
        outer.inlineBlock(inner.createCodeBlock(), {})
        loadA = outer.emitLoad(outerA)
        outerRet = outer.addVariable('ret', 16)
        outer.emitStore(outerRet, loadA)

        code = createSimplifiedCode(outer)
        retCid, retWidth = self.getRetVal(code)
        sidA = self.getSid(outerA)
        correct = (
            Store(retCid, sidA),
            )
        self.assertNodes(code.nodes, correct)
        self.assertRetVal(code, 12345)
        self.assertEqual(retWidth, 16)

    def test_arg_ret(self):
        '''Test whether inlining works with an argument and return value.'''
        inc = TestCodeBlockBuilder()
        incArgRef = inc.addValueArgument('V')
        incArgVal = inc.emitLoad(incArgRef)
        incAdd = inc.emitCompute(AddOperator(incArgVal, IntLiteral(1)))
        incRet = inc.addVariable('ret')
        inc.emitStore(incRet, incAdd)
        incCode = inc.createCodeBlock()

        outer = TestCodeBlockBuilder()
        step0 = outer.emitCompute(IntLiteral(100))
        step1 = outer.emitLoad(outer.inlineBlock(incCode, {'V': step0}))
        step2 = outer.emitLoad(outer.inlineBlock(incCode, {'V': step1}))
        step3 = outer.emitLoad(outer.inlineBlock(incCode, {'V': step2}))
        outerRet = outer.addVariable('ret')
        outer.emitStore(outerRet, step3)

        code = createSimplifiedCode(outer)
        self.assertEqual(len(code.nodes), 0)
        self.assertRetVal(code, 103)

    def test_multiret(self):
        '''Test whether inlining works when "ret" is written multiple times.'''
        inner = TestCodeBlockBuilder()
        val0 = inner.emitCompute(IntLiteral(1000))
        val1 = inner.emitCompute(IntLiteral(2000))
        val2 = inner.emitCompute(IntLiteral(3000))
        innerRet = inner.addVariable('ret', 16)
        inner.emitStore(innerRet, val0)
        inner.emitStore(innerRet, val1)
        inner.emitStore(innerRet, val2)
        innerCode = inner.createCodeBlock()

        outer = TestCodeBlockBuilder()
        inlinedVal = outer.emitLoad(outer.inlineBlock(innerCode, {}))
        outerRet = outer.addVariable('ret', 16)
        outer.emitStore(outerRet, inlinedVal)

        code = createSimplifiedCode(outer)
        self.assertEqual(len(code.nodes), 0)
        self.assertRetVal(code, 3000)

    def test_ret_truncate(self):
        '''Test whether the value returned by a block is truncated.'''
        inner = TestCodeBlockBuilder()
        innerVal = inner.emitCompute(IntLiteral(0x8472))
        innerRet = inner.addVariable('ret', 8)
        inner.emitStore(innerRet, innerVal)
        innerCode = inner.createCodeBlock()
        func = Function('get', IntType(8), {}, innerCode)

        outer = TestCodeBlockBuilder()
        outerVal = outer.emitLoad(outer.inlineFunctionCall(func, {}, None))
        outerRet = outer.addVariable('ret', 16)
        outer.emitStore(outerRet, outerVal)

        code = createSimplifiedCode(outer)
        self.assertEqual(len(code.nodes), 0)
        self.assertRetVal(code, 0x72)

    def test_arg_truncate(self):
        '''Test whether expressions passed via value arguments are truncated.'''
        # Note: Default width is 8 bits.
        inc = TestCodeBlockBuilder()
        incArgRef = inc.addValueArgument('V')
        incArgVal = inc.emitLoad(incArgRef)
        incAdd = inc.emitCompute(AddOperator(incArgVal, IntLiteral(1)))
        incRet = inc.addVariable('ret')
        inc.emitStore(incRet, incAdd)
        incCode = inc.createCodeBlock()
        func = Function('inc', IntType(9), {'V': IntType(8)}, incCode)

        outer = TestCodeBlockBuilder()
        step0 = outer.emitCompute(IntLiteral(0x89FE))
        step1 = outer.emitLoad(outer.inlineBlock(incCode, {'V': step0}))
        step2 = outer.emitLoad(outer.inlineBlock(incCode, {'V': step1}))
        step3 = outer.emitLoad(outer.inlineBlock(incCode, {'V': step2}))
        outerRet = outer.addVariable('ret', 16)
        outer.emitStore(outerRet, step3)

        code = createSimplifiedCode(outer)
        self.assertEqual(len(code.nodes), 0)
        self.assertRetVal(code, 1)

    def test_pass_by_reference(self):
        '''Test whether pass-by-reference arguments work correctly.'''
        inc = TestCodeBlockBuilder()
        incArgRef = inc.addReferenceArgument('R')
        incArgVal = inc.emitLoad(incArgRef)
        incAdd = inc.emitCompute(AddOperator(incArgVal, IntLiteral(1)))
        inc.emitStore(incArgRef, incAdd)
        incCode = inc.createCodeBlock()

        outer = TestCodeBlockBuilder()
        outerA = outer.addRegister('a')
        regA = outer.context['a']
        initA = outer.emitCompute(IntLiteral(100))
        outer.emitStore(outerA, initA)
        outer.inlineBlock(incCode, {'R': regA})
        outer.inlineBlock(incCode, {'R': regA})
        outer.inlineBlock(incCode, {'R': regA})
        outerRet = outer.addVariable('ret')
        finalA = outer.emitLoad(outerA)
        outer.emitStore(outerRet, finalA)

        code = createSimplifiedCode(outer)
        code.verify()
        retCid, retWidth = self.getRetVal(code)
        sidA = self.getSid(outerA)
        correct = (
            Store(retCid, sidA),
            )
        self.assertNodes(code.nodes, correct)
        self.assertRetVal(code, 103)
        self.assertEqual(retWidth, 8)

    def test_pass_concat_by_reference(self):
        '''Test concatenated storages as pass-by-reference arguments.'''
        inc = TestCodeBlockBuilder()
        incArgRef = inc.addReferenceArgument('R', 16)
        incArgVal = inc.emitLoad(incArgRef)
        incAdd = inc.emitCompute(
            AddOperator(incArgVal, IntLiteral(0x1234))
            )
        inc.emitStore(incArgRef, incAdd)
        incCode = inc.createCodeBlock()

        outer = TestCodeBlockBuilder()
        outerH = outer.addRegister('h')
        outerL = outer.addRegister('l')
        regH = outer.context['h']
        regL = outer.context['l']
        regHL = regL.concat(regH)

        initH = outer.emitCompute(IntLiteral(0xab))
        initL = outer.emitCompute(IntLiteral(0xcd))
        outer.emitStore(outerH, initH)
        outer.emitStore(outerL, initL)
        outer.inlineBlock(incCode, {'R': regHL})
        outer.inlineBlock(incCode, {'R': regHL})
        outer.inlineBlock(incCode, {'R': regHL})
        outerRet = outer.addVariable('ret', 16)
        finalHL = outer.emitLoad(regHL)
        outer.emitStore(outerRet, finalHL)

        code = createSimplifiedCode(outer)
        code.verify()
        self.assertEqual(len(code.nodes), 2)
        self.assertRetVal(code, 0xabcd + 3 * 0x1234)

    def test_pass_concat_fixed_by_reference(self):
        '''Test concatenated storages arguments containing FixedValues.'''
        inc = TestCodeBlockBuilder()
        incArgRef = inc.addReferenceArgument('R', 16)
        incArgVal = inc.emitLoad(incArgRef)
        incAdd = inc.emitCompute(AddOperator(incArgVal, IntLiteral(0x1234)))
        inc.emitStore(incArgRef, incAdd)
        incCode = inc.createCodeBlock()

        outer = TestCodeBlockBuilder()
        outerH = outer.addRegister('h')
        outerL = outer.emitFixedValue(IntLiteral(0xcd), 8)
        regH = outer.context['h']
        fixedL = BoundReference.single(outerL, 8)
        regHL = fixedL.concat(regH)

        initH = outer.emitCompute(IntLiteral(0xab))
        outer.emitStore(outerH, initH)
        outer.inlineBlock(incCode, {'R': regHL})
        outer.inlineBlock(incCode, {'R': regHL})
        outer.inlineBlock(incCode, {'R': regHL})
        outerRet = outer.addVariable('ret', 16)
        finalHL = outer.emitLoad(regHL)
        outer.emitStore(outerRet, finalHL)

        code = createSimplifiedCode(outer)
        code.verify()
        self.assertEqual(len(code.nodes), 1)
        self.assertRetVal(code, 0xabcd + 3 * 0x1300)

    def test_pass_slice_by_reference(self):
        '''Test sliced storages as pass-by-reference arguments.'''
        inc = TestCodeBlockBuilder()
        incArgRef = inc.addReferenceArgument('R')
        incArgVal = inc.emitLoad(incArgRef)
        incAdd = inc.emitCompute(AddOperator(incArgVal, IntLiteral(0x12)))
        inc.emitStore(incArgRef, incAdd)
        incCode = inc.createCodeBlock()

        outer = TestCodeBlockBuilder()
        outerR = outer.addRegister('r', 16)
        regR = outer.context['r']
        initR = outer.emitCompute(IntLiteral(0xcdef))
        outer.emitStore(outerR, initR)
        sliceR = regR.slice(4, 8)
        outer.inlineBlock(incCode, {'R': sliceR})
        outer.inlineBlock(incCode, {'R': sliceR})
        outer.inlineBlock(incCode, {'R': sliceR})
        outerRet = outer.addVariable('ret', 16)
        finalR = outer.emitLoad(outerR)
        outer.emitStore(outerRet, finalR)

        code = createSimplifiedCode(outer)
        code.verify()
        self.assertEqual(len(code.nodes), 1)
        self.assertRetVal(code, 0xc00f | (((0xde + 3 * 0x12) & 0xff) << 4))

if __name__ == '__main__':
    verbose = True
    unittest.main()
