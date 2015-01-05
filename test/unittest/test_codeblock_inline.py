from utils_codeblock import NodeChecker, TestCodeBlockBuilder

from retroasm.codeblock import ComputedConstant, Load, Store
from retroasm.expression import (
    AddOperator, AndOperator, Concatenation, IntLiteral
    )

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
        innerA = inner.addRegister('a')
        const = inner.emitCompute(IntLiteral.create(12345))
        inner.emitStore(innerA, const)

        outer = TestCodeBlockBuilder()
        # Make sure that outer and inner block are using the same registers.
        outer.registers = inner.registers
        outerA = outer.addRegister('a')
        zero = outer.emitCompute(IntLiteral.create(0))
        outer.emitStore(outerA, zero)
        outer.inlineBlock(inner.createCodeBlock(), {})
        loadA = outer.emitLoad(outerA)
        outerRet = outer.addVariable('ret')
        outer.emitStore(outerRet, loadA)

        code = createSimplifiedCode(outer)
        correct = (
            Store(code.retCid, outerA),
            )
        self.assertNodes(code.nodes, correct)
        self.assertIntLiteral(code.constants[code.retCid], 12345)

    def test_arg_ret(self):
        '''Test whether inlining works with an argument and return value.'''
        inc = TestCodeBlockBuilder()
        incArgRid = inc.addValueArgument('V')
        incArgVal = inc.emitLoad(incArgRid)
        incAdd = inc.emitCompute(AddOperator(incArgVal, IntLiteral.create(1)))
        incRet = inc.addVariable('ret')
        inc.emitStore(incRet, incAdd)
        incCode = inc.createCodeBlock()

        outer = TestCodeBlockBuilder()
        step0 = outer.emitCompute(IntLiteral.create(100))
        step1 = outer.inlineBlock(incCode, {'V': step0})
        step2 = outer.inlineBlock(incCode, {'V': step1})
        step3 = outer.inlineBlock(incCode, {'V': step2})
        outerRet = outer.addVariable('ret')
        outer.emitStore(outerRet, step3)

        code = createSimplifiedCode(outer)
        self.assertEqual(len(code.nodes), 0)
        self.assertIntLiteral(code.constants[code.retCid], 103)

    def test_pass_by_reference(self):
        '''Test whether pass-by-reference arguments work correctly.'''
        inc = TestCodeBlockBuilder()
        incArgRid = inc.addLocalReference('R')
        incArgVal = inc.emitLoad(incArgRid)
        incAdd = inc.emitCompute(AddOperator(incArgVal, IntLiteral.create(1)))
        inc.emitStore(incArgRid, incAdd)
        incCode = inc.createCodeBlock()

        outer = TestCodeBlockBuilder()
        outerA = outer.addRegister('a')
        regA = outer.references[outerA]
        initA = outer.emitCompute(IntLiteral.create(100))
        outer.emitStore(outerA, initA)
        outer.inlineBlock(incCode, {'R': regA})
        outer.inlineBlock(incCode, {'R': regA})
        outer.inlineBlock(incCode, {'R': regA})
        outerRet = outer.addVariable('ret')
        finalA = outer.emitLoad(outerA)
        outer.emitStore(outerRet, finalA)

        code = createSimplifiedCode(outer)
        code.verify()
        correct = (
            Store(code.retCid, outerA),
            )
        self.assertNodes(code.nodes, correct)
        self.assertIntLiteral(code.constants[code.retCid], 103)

    def test_pass_concat_by_reference(self):
        '''Test concatenated storages as pass-by-reference arguments.'''
        inc = TestCodeBlockBuilder()
        incArgRid = inc.addLocalReference('R')
        incArgVal = inc.emitLoad(incArgRid)
        incAdd = inc.emitCompute(
            AddOperator(incArgVal, IntLiteral.create(0x1234))
            )
        inc.emitStore(incArgRid, incAdd)
        incCode = inc.createCodeBlock()

        outer = TestCodeBlockBuilder()
        outerH = outer.addRegister('h')
        outerL = outer.addRegister('l')
        regH = outer.references[outerH]
        regL = outer.references[outerL]
        initH = outer.emitCompute(IntLiteral.create(0xab))
        initL = outer.emitCompute(IntLiteral.create(0xcd))
        outer.emitStore(outerH, initH)
        outer.emitStore(outerL, initL)
        regHL = Concatenation(regH, regL)
        outer.inlineBlock(incCode, {'R': regHL})
        outer.inlineBlock(incCode, {'R': regHL})
        outer.inlineBlock(incCode, {'R': regHL})
        outerRet = outer.addVariable('ret', 16)
        finalH = outer.emitLoad(outerH)
        finalL = outer.emitLoad(outerL)
        finalHL = outer.emitCompute(Concatenation(finalH, finalL))
        outer.emitStore(outerRet, finalHL)

        code = createSimplifiedCode(outer)
        code.verify()
        self.assertEqual(len(code.nodes), 2)
        self.assertIntLiteral(code.constants[code.retCid], 0xabcd + 3 * 0x1234)

if __name__ == '__main__':
    verbose = True
    unittest.main()
