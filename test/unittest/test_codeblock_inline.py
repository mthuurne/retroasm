from utils_codeblock import NodeChecker, TestNamespace

from retroasm.codeblock import Load, Store
from retroasm.expression import AddOperator, IntLiteral
from retroasm.function import Function
from retroasm.reference import (
    ConcatenatedBits, FixedValue, Reference, SlicedBits
    )
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

def args(**kvargs):
    '''Argument fetcher helper function for inlineBlock().
    '''
    return kvargs.__getitem__

class CodeBlockInlineTests(NodeChecker, unittest.TestCase):

    def test_easy(self):
        '''Test whether inlining works when there are no complications.'''
        inner = TestNamespace()
        innerA = inner.addRegister('a', IntType.u(16))
        const = IntLiteral(12345)
        inner.emitStore(innerA, const)

        # Share the global namespace to make sure that the outer and inner block
        # are using the same registers.
        outer = TestNamespace(inner)
        outerA = outer.addRegister('a', IntType.u(16))
        zero = IntLiteral(0)
        outer.emitStore(outerA, zero)
        outer.inlineBlock(inner.createCodeBlock(), args())
        loadA = outer.emitLoad(outerA)
        outerRet = outer.addVariable('ret', IntType.u(16))
        outer.emitStore(outerRet, loadA)

        code = createSimplifiedCode(outer)
        retVal, retWidth = self.getRetVal(code)
        correct = (
            Store(retVal, outerA.bits.storage),
            )
        self.assertNodes(code.nodes, correct)
        self.assertRetVal(code, 12345)
        self.assertEqual(retWidth, 16)

    def test_arg_ret(self):
        '''Test whether inlining works with an argument and return value.'''
        inc = TestNamespace()
        incArgRef = inc.addValueArgument('V')
        incArgVal = inc.emitLoad(incArgRef)
        incAdd = AddOperator(incArgVal, IntLiteral(1))
        incRet = inc.addVariable('ret')
        inc.emitStore(incRet, incAdd)
        incCode = inc.createCodeBlock()

        outer = TestNamespace()
        argsV = lambda value: args(V=FixedValue(value, 8))
        step0 = IntLiteral(100)
        step1 = outer.emitLoad(outer.inlineBlock(incCode, argsV(step0)))
        step2 = outer.emitLoad(outer.inlineBlock(incCode, argsV(step1)))
        step3 = outer.emitLoad(outer.inlineBlock(incCode, argsV(step2)))
        outerRet = outer.addVariable('ret')
        outer.emitStore(outerRet, step3)

        code = createSimplifiedCode(outer)
        correct = (
            )
        self.assertNodes(code.nodes, correct)
        retVal, retWidth = self.getRetVal(code)
        self.assertRetVal(code, 103)
        self.assertEqual(retWidth, 8)

    def test_multiret(self):
        '''Test whether inlining works when "ret" is written multiple times.'''
        inner = TestNamespace()
        val0 = IntLiteral(1000)
        val1 = IntLiteral(2000)
        val2 = IntLiteral(3000)
        innerRet = inner.addVariable('ret', IntType.u(16))
        inner.emitStore(innerRet, val0)
        inner.emitStore(innerRet, val1)
        inner.emitStore(innerRet, val2)
        innerCode = inner.createCodeBlock()

        outer = TestNamespace()
        inlinedVal = outer.emitLoad(outer.inlineBlock(innerCode, args()))
        outerRet = outer.addVariable('ret', IntType.u(16))
        outer.emitStore(outerRet, inlinedVal)

        code = createSimplifiedCode(outer)
        correct = (
            )
        self.assertNodes(code.nodes, correct)
        retVal, retWidth = self.getRetVal(code)
        self.assertRetVal(code, 3000)
        self.assertEqual(retWidth, 16)

    def test_ret_truncate(self):
        '''Test whether the value returned by a block is truncated.'''
        inner = TestNamespace()
        innerVal = IntLiteral(0x8472)
        innerRet = inner.addVariable('ret')
        inner.emitStore(innerRet, innerVal)
        innerCode = inner.createCodeBlock()
        func = Function(IntType.u(8), {}, innerCode)

        outer = TestNamespace()
        outerVal = outer.emitLoad(outer.inlineFunctionCall(func, {}))
        outerRet = outer.addVariable('ret', IntType.u(16))
        outer.emitStore(outerRet, outerVal)

        code = createSimplifiedCode(outer)
        correct = (
            )
        self.assertNodes(code.nodes, correct)
        retVal, retWidth = self.getRetVal(code)
        self.assertRetVal(code, 0x72)
        self.assertEqual(retWidth, 16)

    def test_pass_by_reference(self):
        '''Test whether pass-by-reference arguments work correctly.'''
        inc = TestNamespace()
        incArgRef = inc.addReferenceArgument('R')
        incArgVal = inc.emitLoad(incArgRef)
        incAdd = AddOperator(incArgVal, IntLiteral(1))
        inc.emitStore(incArgRef, incAdd)
        incCode = inc.createCodeBlock()

        outer = TestNamespace()
        outerA = outer.addRegister('a')
        initA = IntLiteral(100)
        outer.emitStore(outerA, initA)
        outer.inlineBlock(incCode, args(R=outerA.bits))
        outer.inlineBlock(incCode, args(R=outerA.bits))
        outer.inlineBlock(incCode, args(R=outerA.bits))
        outerRet = outer.addVariable('ret')
        finalA = outer.emitLoad(outerA)
        outer.emitStore(outerRet, finalA)

        code = createSimplifiedCode(outer)
        retVal, retWidth = self.getRetVal(code)
        correct = (
            Store(retVal, outerA.bits.storage),
            )
        self.assertNodes(code.nodes, correct)
        self.assertRetVal(code, 103)
        self.assertEqual(retWidth, 8)

    def test_pass_concat_by_reference(self):
        '''Test concatenated storages as pass-by-reference arguments.'''
        inc = TestNamespace()
        incArgRef = inc.addReferenceArgument('R', IntType.u(16))
        incArgVal = inc.emitLoad(incArgRef)
        incAdd = AddOperator(incArgVal, IntLiteral(0x1234))
        inc.emitStore(incArgRef, incAdd)
        incCode = inc.createCodeBlock()

        outer = TestNamespace()
        outerH = outer.addRegister('h')
        outerL = outer.addRegister('l')
        bitsHL = ConcatenatedBits(outerL.bits, outerH.bits)

        initH = IntLiteral(0xab)
        initL = IntLiteral(0xcd)
        outer.emitStore(outerH, initH)
        outer.emitStore(outerL, initL)
        outer.inlineBlock(incCode, args(R=bitsHL))
        outer.inlineBlock(incCode, args(R=bitsHL))
        outer.inlineBlock(incCode, args(R=bitsHL))
        outerRet = outer.addVariable('ret', IntType.u(16))
        finalHL = outer.emitLoad(Reference(bitsHL, IntType.u(16)))
        outer.emitStore(outerRet, finalHL)

        finalVal = 0xabcd + 3 * 0x1234
        code = createSimplifiedCode(outer)
        correct = (
            Store(IntLiteral(finalVal & 0xFF), outerL.bits.storage),
            Store(IntLiteral(finalVal >> 8), outerH.bits.storage),
            )
        self.assertNodes(code.nodes, correct)
        retVal, retWidth = self.getRetVal(code)
        self.assertRetVal(code, finalVal)
        self.assertEqual(retWidth, 16)

    def test_pass_concat_fixed_by_reference(self):
        '''Test concatenated storages arguments containing FixedValues.'''
        inc = TestNamespace()
        incArgRef = inc.addReferenceArgument('R', IntType.u(16))
        incArgVal = inc.emitLoad(incArgRef)
        incAdd = AddOperator(incArgVal, IntLiteral(0x1234))
        inc.emitStore(incArgRef, incAdd)
        incCode = inc.createCodeBlock()

        outer = TestNamespace()
        outerH = outer.addRegister('h')
        outerL = FixedValue(IntLiteral(0xcd), 8)
        bitsHL = ConcatenatedBits(outerL, outerH.bits)

        initH = IntLiteral(0xab)
        outer.emitStore(outerH, initH)
        outer.inlineBlock(incCode, args(R=bitsHL))
        outer.inlineBlock(incCode, args(R=bitsHL))
        outer.inlineBlock(incCode, args(R=bitsHL))
        outerRet = outer.addVariable('ret', IntType.u(16))
        finalHL = outer.emitLoad(Reference(bitsHL, IntType.u(16)))
        outer.emitStore(outerRet, finalHL)

        finalVal = 0xabcd + 3 * 0x1300
        code = createSimplifiedCode(outer)
        correct = (
            Store(IntLiteral(finalVal >> 8), outerH.bits.storage),
            )
        self.assertNodes(code.nodes, correct)
        retVal, retWidth = self.getRetVal(code)
        self.assertRetVal(code, finalVal)
        self.assertEqual(retWidth, 16)

    def test_pass_slice_by_reference(self):
        '''Test sliced storages as pass-by-reference arguments.'''
        inc = TestNamespace()
        incArgRef = inc.addReferenceArgument('R')
        incArgVal = inc.emitLoad(incArgRef)
        incAdd = AddOperator(incArgVal, IntLiteral(0x12))
        inc.emitStore(incArgRef, incAdd)
        incCode = inc.createCodeBlock()

        outer = TestNamespace()
        outerR = outer.addRegister('r', IntType.u(16))
        initR = IntLiteral(0xcdef)
        outer.emitStore(outerR, initR)
        sliceR = SlicedBits(outerR.bits, IntLiteral(4), 8)
        outer.inlineBlock(incCode, args(R=sliceR))
        outer.inlineBlock(incCode, args(R=sliceR))
        outer.inlineBlock(incCode, args(R=sliceR))
        outerRet = outer.addVariable('ret', IntType.u(16))
        finalR = outer.emitLoad(outerR)
        outer.emitStore(outerRet, finalR)

        finalVal = 0xc00f | (((0xde + 3 * 0x12) & 0xff) << 4)
        code = createSimplifiedCode(outer)
        correct = (
            Store(IntLiteral(finalVal), outerR.bits.storage),
            )
        self.assertNodes(code.nodes, correct)
        retVal, retWidth = self.getRetVal(code)
        self.assertRetVal(code, finalVal)
        self.assertEqual(retWidth, 16)

    def test_inline_unsigned_reg(self):
        '''Test reading of an unsigned register.'''
        inner = TestNamespace()
        innerA = inner.addRegister('a')
        innerLoad = inner.emitLoad(innerA)
        innerRet = inner.addVariable('ret', IntType.u(16))
        inner.emitStore(innerRet, innerLoad)
        innerCode = inner.createCodeBlock()

        outer = TestNamespace(inner)
        outerA = outer.addRegister('a')
        initA = IntLiteral(0xb2)
        outer.emitStore(outerA, initA)
        retBits = outer.inlineBlock(innerCode, args())
        outerRet = outer.addVariable('ret', IntType.u(16))
        retVal = outer.emitLoad(retBits)
        outer.emitStore(outerRet, retVal)

        finalVal = 0x00b2
        code = createSimplifiedCode(outer)
        correct = (
            Store(initA, outerA.bits.storage),
            )
        self.assertNodes(code.nodes, correct)
        self.assertRetVal(code, finalVal)

    def test_inline_signed_reg(self):
        '''Test reading of a signed register.'''
        inner = TestNamespace()
        innerA = inner.addRegister('a', IntType.s(8))
        innerLoad = inner.emitLoad(innerA)
        innerRet = inner.addVariable('ret', IntType.u(16))
        inner.emitStore(innerRet, innerLoad)
        innerCode = inner.createCodeBlock()

        outer = TestNamespace(inner)
        outerA = outer.addRegister('a', IntType.s(8))
        initA = IntLiteral(0xb2)
        outer.emitStore(outerA, initA)
        retBits = outer.inlineBlock(innerCode, args())
        outerRet = outer.addVariable('ret', IntType.u(16))
        retVal = outer.emitLoad(retBits)
        outer.emitStore(outerRet, retVal)

        finalVal = 0xffb2
        code = createSimplifiedCode(outer)
        correct = (
            Store(initA, outerA.bits.storage),
            )
        self.assertNodes(code.nodes, correct)
        retVal, retWidth = self.getRetVal(code)
        self.assertRetVal(code, finalVal)
        self.assertEqual(retWidth, 16)

    def test_load_from_unsigned_reference_arg(self):
        '''Test reading of a value passed via an unsigned reference.'''
        inner = TestNamespace()
        argRef = inner.addReferenceArgument('R')
        argVal = inner.emitLoad(argRef)
        innerRet = inner.addVariable('ret', IntType.u(16))
        inner.emitStore(innerRet, argVal)
        innerCode = inner.createCodeBlock()

        outer = TestNamespace()
        fixedVal = FixedValue(IntLiteral(0xa4), 8)
        retBits = outer.inlineBlock(innerCode, args(R=fixedVal))
        outerRet = outer.addVariable('ret', IntType.u(16))
        retVal = outer.emitLoad(retBits)
        outer.emitStore(outerRet, retVal)

        code = createSimplifiedCode(outer)
        correct = (
            )
        self.assertNodes(code.nodes, correct)
        retVal, retWidth = self.getRetVal(code)
        self.assertRetVal(code, 0x00a4)
        self.assertEqual(retWidth, 16)

    def test_load_from_signed_reference_arg(self):
        '''Test reading of a value passed via a signed reference.'''
        inner = TestNamespace()
        argRef = inner.addReferenceArgument('R', IntType.s(8))
        argVal = inner.emitLoad(argRef)
        innerRet = inner.addVariable('ret', IntType.u(16))
        inner.emitStore(innerRet, argVal)
        innerCode = inner.createCodeBlock()

        outer = TestNamespace()
        fixedVal = FixedValue(IntLiteral(0xa4), 8)
        retBits = outer.inlineBlock(innerCode, args(R=fixedVal))
        outerRet = outer.addVariable('ret', IntType.u(16))
        retVal = outer.emitLoad(retBits)
        outer.emitStore(outerRet, retVal)

        code = createSimplifiedCode(outer)
        correct = (
            )
        self.assertNodes(code.nodes, correct)
        retVal, retWidth = self.getRetVal(code)
        self.assertRetVal(code, 0xffa4)
        self.assertEqual(retWidth, 16)

    def test_return_simple_reference(self):
        '''Test returning a reference to a global.'''
        inner = TestNamespace()
        innerA = inner.addRegister('a')
        inner.addRetReference(innerA)
        innerCode = inner.createCodeBlock()
        self.assertIsNotNone(innerCode.retBits)

        outer = TestNamespace(inner)
        retBits = outer.inlineBlock(innerCode, args())
        outerA = outer.addRegister('a')
        fake = IntLiteral(0xdc)
        outer.emitStore(outerA, fake)
        value = IntLiteral(0xba)
        outer.emitStore(retBits, value)
        outerRet = outer.addVariable('ret')
        retVal = outer.emitLoad(outerA)
        outer.emitStore(outerRet, retVal)

        code = createSimplifiedCode(outer)
        correct = (
            Store(value, outerA.bits.storage),
            )
        self.assertNodes(code.nodes, correct)
        retVal, retWidth = self.getRetVal(code)
        self.assertRetVal(code, 0xba)
        self.assertEqual(retWidth, 8)

    def test_return_io_reference(self):
        '''Test returning a reference to an index in an I/O channel.'''
        inner = TestNamespace()
        addrArg = inner.addValueArgument('A', IntType.u(16))
        addrVal = inner.emitLoad(addrArg)
        memByte = inner.addIOStorage('mem', addrVal)
        inner.addRetReference(memByte)
        innerCode = inner.createCodeBlock()
        self.assertIsNotNone(innerCode.retBits)

        outer = TestNamespace()
        addr = FixedValue(IntLiteral(0x4002), 16)
        retBits = outer.inlineBlock(innerCode, args(A=addr))
        outerRet = outer.addVariable('ret')
        retVal = outer.emitLoad(retBits)
        outer.emitStore(outerRet, retVal)

        code = createSimplifiedCode(outer)
        correct = (
            Load(retBits.storage),
            )
        self.assertNodes(code.nodes, correct)
        retVal, retWidth = self.getRetVal(code)
        self.assertEqual(retWidth, 8)
        self.assertEqual(retVal, code.nodes[0].expr)

if __name__ == '__main__':
    verbose = True
    unittest.main()
