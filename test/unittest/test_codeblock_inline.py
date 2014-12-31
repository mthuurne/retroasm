from utils_codeblock import NodeChecker, TestCodeBlockBuilder

from retroasm.codeblock import ComputedConstant, Load, Store
from retroasm.expression import AddOperator, AndOperator, IntLiteral

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

if __name__ == '__main__':
    verbose = True
    unittest.main()
