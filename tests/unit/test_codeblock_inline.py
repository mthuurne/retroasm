from __future__ import annotations

from typing import Callable

from retroasm.codeblock import CodeBlock, Load, Store
from retroasm.expression import AddOperator, Expression, IntLiteral, XorOperator
from retroasm.function import Function
from retroasm.namespace import LocalNamespace
from retroasm.reference import (
    BitString,
    ConcatenatedBits,
    FixedValue,
    Reference,
    SingleStorage,
    SlicedBits,
)
from retroasm.types import IntType

from .utils_codeblock import TestNamespace, assertNodes, assertRetVal, getRetVal

verbose = False


def createSimplifiedCode(namespace: LocalNamespace) -> CodeBlock:
    if verbose:
        print("=" * 40)
        namespace.dump()
    if "ret" in namespace:
        retRef = namespace.elements["ret"]
        assert isinstance(retRef, Reference), retRef
    else:
        retRef = None
    code = namespace.createCodeBlock(retRef)
    if verbose:
        print("-" * 40)
        code.dump()
    return code


def args(**kvargs: BitString) -> Callable[[str], BitString]:
    """Argument fetcher helper function for inlineBlock()."""
    return kvargs.__getitem__


def test_inline_easy() -> None:
    """Test whether inlining works when there are no complications."""
    inner = TestNamespace()
    innerA = inner.addRegister("a", IntType.u(16))
    const = IntLiteral(12345)
    inner.emitStore(innerA, const)

    # Share the global namespace to make sure that the outer and inner block
    # are using the same registers.
    outer = TestNamespace(inner)
    outerA = outer.addRegister("a", IntType.u(16))
    zero = IntLiteral(0)
    outer.emitStore(outerA, zero)
    outer.inlineBlock(inner.createCodeBlock(None), args())
    loadA = outer.emitLoad(outerA)
    outerRet = outer.addVariable("ret", IntType.u(16))
    outer.emitStore(outerRet, loadA)

    code = createSimplifiedCode(outer)
    retVal, retWidth = getRetVal(code)
    correct = (Store(retVal, outerA.bits.storage),)
    assertNodes(code.nodes, correct)
    assertRetVal(code, 12345)
    assert retWidth == 16


def test_inline_arg_ret() -> None:
    """Test whether inlining works with an argument and return value."""
    inc = TestNamespace()
    incArgRef = inc.addArgument("V")
    incArgVal = inc.emitLoad(incArgRef)
    incAdd = AddOperator(incArgVal, IntLiteral(1))
    incRet = inc.addVariable("ret")
    inc.emitStore(incRet, incAdd)
    incCode = inc.createCodeBlock(incRet)

    outer = TestNamespace()

    def argsV(value: Expression) -> Callable[[str], BitString]:
        return args(V=FixedValue(value, 8))

    step0 = IntLiteral(100)
    (ret1,) = outer.inlineBlock(incCode, argsV(step0))
    step1 = outer.emitLoad(ret1)
    (ret2,) = outer.inlineBlock(incCode, argsV(step1))
    step2 = outer.emitLoad(ret2)
    (ret3,) = outer.inlineBlock(incCode, argsV(step2))
    step3 = outer.emitLoad(ret3)
    outerRet = outer.addVariable("ret")
    outer.emitStore(outerRet, step3)

    code = createSimplifiedCode(outer)
    correct = ()
    assertNodes(code.nodes, correct)
    retVal, retWidth = getRetVal(code)
    assertRetVal(code, 103)
    assert retWidth == 8


def test_inline_multiret() -> None:
    """Test whether inlining works when "ret" is written multiple times."""
    inner = TestNamespace()
    val0 = IntLiteral(1000)
    val1 = IntLiteral(2000)
    val2 = IntLiteral(3000)
    innerRet = inner.addVariable("ret", IntType.u(16))
    inner.emitStore(innerRet, val0)
    inner.emitStore(innerRet, val1)
    inner.emitStore(innerRet, val2)
    innerCode = inner.createCodeBlock(innerRet)

    outer = TestNamespace()
    (inlinedRet,) = outer.inlineBlock(innerCode, args())
    inlinedVal = outer.emitLoad(inlinedRet)
    outerRet = outer.addVariable("ret", IntType.u(16))
    outer.emitStore(outerRet, inlinedVal)

    code = createSimplifiedCode(outer)
    correct = ()
    assertNodes(code.nodes, correct)
    retVal, retWidth = getRetVal(code)
    assertRetVal(code, 3000)
    assert retWidth == 16


def test_ret_truncate() -> None:
    """Test whether the value returned by a block is truncated."""
    inner = TestNamespace()
    innerVal = IntLiteral(0x8472)
    innerRet = inner.addVariable("ret")
    inner.emitStore(innerRet, innerVal)
    innerCode = inner.createCodeBlock(innerRet)
    func = Function(IntType.u(8), {}, innerCode)

    outer = TestNamespace()
    inlineRet = outer.inlineFunctionCall(func, {})
    assert inlineRet is not None
    outerVal = outer.emitLoad(inlineRet)
    outerRet = outer.addVariable("ret", IntType.u(16))
    outer.emitStore(outerRet, outerVal)

    code = createSimplifiedCode(outer)
    correct = ()
    assertNodes(code.nodes, correct)
    retVal, retWidth = getRetVal(code)
    assertRetVal(code, 0x72)
    assert retWidth == 16


def test_pass_by_reference() -> None:
    """Test whether pass-by-reference arguments work correctly."""
    inc = TestNamespace()
    incArgRef = inc.addArgument("R")
    incArgVal = inc.emitLoad(incArgRef)
    incAdd = AddOperator(incArgVal, IntLiteral(1))
    inc.emitStore(incArgRef, incAdd)
    incCode = inc.createCodeBlock(None)

    outer = TestNamespace()
    outerA = outer.addRegister("a")
    initA = IntLiteral(100)
    outer.emitStore(outerA, initA)
    outer.inlineBlock(incCode, args(R=outerA.bits))
    outer.inlineBlock(incCode, args(R=outerA.bits))
    outer.inlineBlock(incCode, args(R=outerA.bits))
    outerRet = outer.addVariable("ret")
    finalA = outer.emitLoad(outerA)
    outer.emitStore(outerRet, finalA)

    code = createSimplifiedCode(outer)
    retVal, retWidth = getRetVal(code)
    correct = (Store(retVal, outerA.bits.storage),)
    assertNodes(code.nodes, correct)
    assertRetVal(code, 103)
    assert retWidth == 8


def test_pass_concat_by_reference() -> None:
    """Test concatenated storages as pass-by-reference arguments."""
    inc = TestNamespace()
    incArgRef = inc.addArgument("R", IntType.u(16))
    incArgVal = inc.emitLoad(incArgRef)
    incAdd = AddOperator(incArgVal, IntLiteral(0x1234))
    inc.emitStore(incArgRef, incAdd)
    incCode = inc.createCodeBlock(None)

    outer = TestNamespace()
    outerH = outer.addRegister("h")
    outerL = outer.addRegister("l")
    bitsHL = ConcatenatedBits(outerL.bits, outerH.bits)

    initH = IntLiteral(0xAB)
    initL = IntLiteral(0xCD)
    outer.emitStore(outerH, initH)
    outer.emitStore(outerL, initL)
    outer.inlineBlock(incCode, args(R=bitsHL))
    outer.inlineBlock(incCode, args(R=bitsHL))
    outer.inlineBlock(incCode, args(R=bitsHL))
    outerRet = outer.addVariable("ret", IntType.u(16))
    finalHL = outer.emitLoad(Reference(bitsHL, IntType.u(16)))
    outer.emitStore(outerRet, finalHL)

    finalVal = 0xABCD + 3 * 0x1234
    code = createSimplifiedCode(outer)
    correct = (
        Store(IntLiteral(finalVal & 0xFF), outerL.bits.storage),
        Store(IntLiteral(finalVal >> 8), outerH.bits.storage),
    )
    assertNodes(code.nodes, correct)
    retVal, retWidth = getRetVal(code)
    assertRetVal(code, finalVal)
    assert retWidth == 16


def test_pass_concat_fixed_by_reference() -> None:
    """Test concatenated storages arguments containing FixedValues."""
    inc = TestNamespace()
    incArgRef = inc.addArgument("R", IntType.u(16))
    incArgVal = inc.emitLoad(incArgRef)
    incAdd = AddOperator(incArgVal, IntLiteral(0x1234))
    inc.emitStore(incArgRef, incAdd)
    incCode = inc.createCodeBlock(None)

    outer = TestNamespace()
    outerH = outer.addRegister("h")
    outerL = FixedValue(IntLiteral(0xCD), 8)
    bitsHL = ConcatenatedBits(outerL, outerH.bits)

    initH = IntLiteral(0xAB)
    outer.emitStore(outerH, initH)
    outer.inlineBlock(incCode, args(R=bitsHL))
    outer.inlineBlock(incCode, args(R=bitsHL))
    outer.inlineBlock(incCode, args(R=bitsHL))
    outerRet = outer.addVariable("ret", IntType.u(16))
    finalHL = outer.emitLoad(Reference(bitsHL, IntType.u(16)))
    outer.emitStore(outerRet, finalHL)

    finalVal = 0xABCD + 3 * 0x1300
    code = createSimplifiedCode(outer)
    correct = (Store(IntLiteral(finalVal >> 8), outerH.bits.storage),)
    assertNodes(code.nodes, correct)
    retVal, retWidth = getRetVal(code)
    assertRetVal(code, finalVal)
    assert retWidth == 16


def test_pass_slice_by_reference() -> None:
    """Test sliced storages as pass-by-reference arguments."""
    inc = TestNamespace()
    incArgRef = inc.addArgument("R")
    incArgVal = inc.emitLoad(incArgRef)
    incAdd = AddOperator(incArgVal, IntLiteral(0x12))
    inc.emitStore(incArgRef, incAdd)
    incCode = inc.createCodeBlock(None)

    outer = TestNamespace()
    outerR = outer.addRegister("r", IntType.u(16))
    initR = IntLiteral(0xCDEF)
    outer.emitStore(outerR, initR)
    sliceR = SlicedBits(outerR.bits, IntLiteral(4), 8)
    outer.inlineBlock(incCode, args(R=sliceR))
    outer.inlineBlock(incCode, args(R=sliceR))
    outer.inlineBlock(incCode, args(R=sliceR))
    outerRet = outer.addVariable("ret", IntType.u(16))
    finalR = outer.emitLoad(outerR)
    outer.emitStore(outerRet, finalR)

    finalVal = 0xC00F | (((0xDE + 3 * 0x12) & 0xFF) << 4)
    code = createSimplifiedCode(outer)
    correct = (Store(IntLiteral(finalVal), outerR.bits.storage),)
    assertNodes(code.nodes, correct)
    retVal, retWidth = getRetVal(code)
    assertRetVal(code, finalVal)
    assert retWidth == 16


def test_inline_unsigned_reg() -> None:
    """Test reading of an unsigned register."""
    inner = TestNamespace()
    innerA = inner.addRegister("a")
    innerLoad = inner.emitLoad(innerA)
    innerRet = inner.addVariable("ret", IntType.u(16))
    inner.emitStore(innerRet, innerLoad)
    innerCode = inner.createCodeBlock(innerRet)

    outer = TestNamespace(inner)
    outerA = outer.addRegister("a")
    initA = IntLiteral(0xB2)
    outer.emitStore(outerA, initA)
    (retBits,) = outer.inlineBlock(innerCode, args())
    outerRet = outer.addVariable("ret", IntType.u(16))
    retVal = outer.emitLoad(retBits)
    outer.emitStore(outerRet, retVal)

    finalVal = 0x00B2
    code = createSimplifiedCode(outer)
    correct = (Store(initA, outerA.bits.storage),)
    assertNodes(code.nodes, correct)
    assertRetVal(code, finalVal)


def test_inline_signed_reg() -> None:
    """Test reading of a signed register."""
    inner = TestNamespace()
    innerA = inner.addRegister("a", IntType.s(8))
    innerLoad = inner.emitLoad(innerA)
    innerRet = inner.addVariable("ret", IntType.u(16))
    inner.emitStore(innerRet, innerLoad)
    innerCode = inner.createCodeBlock(innerRet)

    outer = TestNamespace(inner)
    outerA = outer.addRegister("a", IntType.s(8))
    initA = IntLiteral(0xB2)
    outer.emitStore(outerA, initA)
    (retBits,) = outer.inlineBlock(innerCode, args())
    outerRet = outer.addVariable("ret", IntType.u(16))
    retVal = outer.emitLoad(retBits)
    outer.emitStore(outerRet, retVal)

    finalVal = 0xFFB2
    code = createSimplifiedCode(outer)
    correct = (Store(initA, outerA.bits.storage),)
    assertNodes(code.nodes, correct)
    retVal, retWidth = getRetVal(code)
    assertRetVal(code, finalVal)
    assert retWidth == 16


def test_load_from_unsigned_reference_arg() -> None:
    """Test reading of a value passed via an unsigned reference."""
    inner = TestNamespace()
    argRef = inner.addArgument("R")
    argVal = inner.emitLoad(argRef)
    innerRet = inner.addVariable("ret", IntType.u(16))
    inner.emitStore(innerRet, argVal)
    innerCode = inner.createCodeBlock(innerRet)

    outer = TestNamespace()
    fixedVal = FixedValue(IntLiteral(0xA4), 8)
    (retBits,) = outer.inlineBlock(innerCode, args(R=fixedVal))
    outerRet = outer.addVariable("ret", IntType.u(16))
    retVal = outer.emitLoad(retBits)
    outer.emitStore(outerRet, retVal)

    code = createSimplifiedCode(outer)
    correct = ()
    assertNodes(code.nodes, correct)
    retVal, retWidth = getRetVal(code)
    assertRetVal(code, 0x00A4)
    assert retWidth == 16


def test_load_from_signed_reference_arg() -> None:
    """Test reading of a value passed via a signed reference."""
    inner = TestNamespace()
    argRef = inner.addArgument("R", IntType.s(8))
    argVal = inner.emitLoad(argRef)
    innerRet = inner.addVariable("ret", IntType.u(16))
    inner.emitStore(innerRet, argVal)
    innerCode = inner.createCodeBlock(innerRet)

    outer = TestNamespace()
    fixedVal = FixedValue(IntLiteral(0xA4), 8)
    (retBits,) = outer.inlineBlock(innerCode, args(R=fixedVal))
    outerRet = outer.addVariable("ret", IntType.u(16))
    retVal = outer.emitLoad(retBits)
    outer.emitStore(outerRet, retVal)

    code = createSimplifiedCode(outer)
    correct = ()
    assertNodes(code.nodes, correct)
    retVal, retWidth = getRetVal(code)
    assertRetVal(code, 0xFFA4)
    assert retWidth == 16


def test_return_simple_reference() -> None:
    """Test returning a reference to a global."""
    inner = TestNamespace()
    innerA = inner.addRegister("a")
    inner.addRetReference(innerA)
    innerRet = inner["ret"]
    assert isinstance(innerRet, Reference), innerRet
    innerCode = inner.createCodeBlock(innerRet)
    assert len(innerCode.returned) == 1

    outer = TestNamespace(inner)
    (retBits,) = outer.inlineBlock(innerCode, args())
    outerA = outer.addRegister("a")
    fake = IntLiteral(0xDC)
    outer.emitStore(outerA, fake)
    value = IntLiteral(0xBA)
    outer.emitStore(retBits, value)
    outerRet = outer.addVariable("ret")
    retVal = outer.emitLoad(outerA)
    outer.emitStore(outerRet, retVal)

    code = createSimplifiedCode(outer)
    correct = (Store(value, outerA.bits.storage),)
    assertNodes(code.nodes, correct)
    retVal, retWidth = getRetVal(code)
    assertRetVal(code, 0xBA)
    assert retWidth == 8


def test_return_io_reference() -> None:
    """Test returning a reference to an index in an I/O channel."""
    inner = TestNamespace()
    addrArg = inner.addArgument("A", IntType.u(16))
    addrVal = inner.emitLoad(addrArg)
    memByte = inner.addIOStorage("mem", addrVal)
    inner.addRetReference(memByte)
    innerRet = inner["ret"]
    assert isinstance(innerRet, Reference), innerRet
    innerCode = inner.createCodeBlock(innerRet)
    assert len(innerCode.returned) == 1

    outer = TestNamespace()
    addr = FixedValue(IntLiteral(0x4002), 16)
    (retBits,) = outer.inlineBlock(innerCode, args(A=addr))
    assert isinstance(retBits, SingleStorage), retBits
    outerRet = outer.addVariable("ret")
    retVal = outer.emitLoad(retBits)
    outer.emitStore(outerRet, retVal)

    code = createSimplifiedCode(outer)
    correct = (Load(retBits.storage),)
    assertNodes(code.nodes, correct)
    retVal, retWidth = getRetVal(code)
    assert retWidth == 8
    assert retVal == code.nodes[0].expr


def test_unique_loads() -> None:
    """Test whether multiple instances of the same load are kept separate.
    If the possibility of side effects is ignored, only one load will
    remain.
    If the load results are not kept separate, the load result will be
    XOR-ed with itself, resulting in a 0 result.
    """
    addr = IntLiteral(0xFFFF)

    inner = TestNamespace()
    memByte = inner.addIOStorage("mem", addr)
    loadR = inner.emitLoad(memByte)
    innerRet = inner.addVariable("ret")
    inner.emitStore(innerRet, loadR)
    innerCode = inner.createCodeBlock(innerRet)
    assert len(innerCode.returned) == 1

    outer = TestNamespace()
    (val1Bits,) = outer.inlineBlock(innerCode)
    assert isinstance(val1Bits, FixedValue)
    val1 = val1Bits.expr
    (val2Bits,) = outer.inlineBlock(innerCode)
    assert isinstance(val2Bits, FixedValue)
    val2 = val2Bits.expr
    outerRet = outer.addVariable("ret")
    outer.emitStore(outerRet, XorOperator(val1, val2))

    code = createSimplifiedCode(outer)
    correct = (
        Load(memByte.bits.storage),
        Load(memByte.bits.storage),
    )
    assertNodes(code.nodes, correct)
    retVal, retWidth = getRetVal(code)
    assert retWidth == 8
    assert isinstance(retVal, XorOperator)
