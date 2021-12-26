from __future__ import annotations

from pytest import fixture

from retroasm.codeblock import Load, Store
from retroasm.expression import (
    AddOperator,
    AndOperator,
    IntLiteral,
    OrOperator,
    truncate,
)
from retroasm.expression_simplifier import simplifyExpression
from retroasm.reference import (
    ConcatenatedBits,
    FixedValue,
    Reference,
    SingleStorage,
    SlicedBits,
)
from retroasm.storage import IOStorage
from retroasm.types import IntType

from .utils_codeblock import TestNamespace, assertNodes, assertRetVal, getRetVal
from .utils_expression import assertIntLiteral, assertOr, assertTrunc, makeConcat

verbose = False


@fixture
def namespace():
    return TestNamespace()


def createSimplifiedCode(namespace):
    if verbose:
        print("=" * 40)
        namespace.dump()
    retRef = namespace.elements["ret"] if "ret" in namespace else None
    code = namespace.createCodeBlock(retRef)
    if verbose:
        print("-" * 40)
        code.dump()
    return code


def test_no_change(namespace):
    """Test whether a basic sequence survives a simplification attempt."""
    refA = namespace.addRegister("a")
    refB = namespace.addRegister("b")
    loadA = namespace.emitLoad(refA)
    namespace.emitStore(refB, loadA)

    def checkNodes(code):
        assert len(code.nodes) == 2
        load, store = code.nodes
        assert isinstance(load, Load)
        assert isinstance(store, Store)
        assert load.storage == refA.bits.storage
        assert store.storage == refB.bits.storage
        assert store.expr is load.expr

    code = namespace.createCodeBlock(None)
    checkNodes(code)
    code = createSimplifiedCode(namespace)
    checkNodes(code)


def test_stored_expression(namespace):
    """Test whether stored expressions are simplified."""
    const1 = IntLiteral(2)
    const2 = AddOperator(IntLiteral(1), IntLiteral(1))
    refA = namespace.addRegister("a")
    refB = namespace.addRegister("b")
    namespace.emitStore(refA, const1)
    namespace.emitStore(refB, const2)

    correct = (
        Store(const1, refA.bits.storage),
        Store(const1, refB.bits.storage),
    )

    code = createSimplifiedCode(namespace)
    assertNodes(code.nodes, correct)


def test_unused_load(namespace):
    """Test whether unused loads are removed."""
    refA = namespace.addRegister("a")
    loadA = namespace.emitLoad(refA)
    andA = AndOperator(loadA, IntLiteral(0))
    namespace.emitStore(refA, andA)

    code = createSimplifiedCode(namespace)
    assert len(code.nodes) == 1
    node = code.nodes[0]
    assert isinstance(node, Store)
    assert node.storage is refA.bits.storage
    assertIntLiteral(node.expr, 0)


def test_unused_load_nonremoval(namespace):
    """Test whether unused loads are kept for possible side effects."""
    addr = IntLiteral(0xD0D0)
    refM = namespace.addIOStorage("mem", addr)
    loadM = namespace.emitLoad(refM)

    correct = (Load(refM.bits.storage),)

    code = createSimplifiedCode(namespace)
    assertNodes(code.nodes, correct)


def test_redundant_load_after_load(namespace):
    """Test whether redundant successive loads are removed."""
    refA = namespace.addRegister("a")
    refB = namespace.addRegister("b")
    refC = namespace.addRegister("c")
    loadA1 = namespace.emitLoad(refA)
    loadA2 = namespace.emitLoad(refA)
    namespace.emitStore(refB, loadA1)
    namespace.emitStore(refC, loadA2)

    def correct():
        loadA = Load(refA.bits.storage)
        yield loadA
        yield Store(loadA.expr, refB.bits.storage)
        yield Store(loadA.expr, refC.bits.storage)

    code = createSimplifiedCode(namespace)
    assertNodes(code.nodes, correct())


def test_redundant_load_after_store(namespace):
    """Test whether a redundant load after a store is removed."""
    refA = namespace.addRegister("a")
    refB = namespace.addRegister("b")
    loadA1 = namespace.emitLoad(refA)
    incA = AddOperator(loadA1, IntLiteral(1))
    namespace.emitStore(refA, incA)
    loadA2 = namespace.emitLoad(refA)
    namespace.emitStore(refB, loadA2)

    code = createSimplifiedCode(namespace)
    assert len(code.nodes) == 3
    load, store1, store2 = code.nodes
    assert isinstance(load, Load)
    assert isinstance(store1, Store)
    assert isinstance(store2, Store)
    assert load.storage == refA.bits.storage
    assert store1.storage == refA.bits.storage
    assert store2.storage == refB.bits.storage
    assert store1.expr == store2.expr
    assertTrunc(
        simplifyExpression(store1.expr),
        simplifyExpression(incA).substitute(
            lambda expr: load.expr if expr is loadA1 else None
        ),
        incA.mask.bit_length(),
        refA.width,
    )


def test_redundant_same_value_store(namespace):
    """Test removal of storing the same value in the same storage twice."""
    refA = namespace.addRegister("a")
    refB = namespace.addRegister("b")
    loadA = namespace.emitLoad(refA)
    namespace.emitStore(refB, loadA)
    namespace.emitStore(refB, loadA)

    def correct():
        loadA = Load(refA.bits.storage)
        yield loadA
        yield Store(loadA.expr, refB.bits.storage)

    code = createSimplifiedCode(namespace)
    assertNodes(code.nodes, correct())


def test_redundant_other_value_store(namespace):
    """Test removal of storing a different value in the same storage."""
    refA = namespace.addRegister("a")
    refB = namespace.addRegister("b")
    refC = namespace.addRegister("c")
    loadA = namespace.emitLoad(refA)
    loadB = namespace.emitLoad(refB)
    namespace.emitStore(refC, loadA)
    namespace.emitStore(refC, loadB)

    def correct():
        loadB = Load(refB.bits.storage)
        yield loadB
        yield Store(loadB.expr, refC.bits.storage)

    code = createSimplifiedCode(namespace)
    assertNodes(code.nodes, correct())


def test_uncertain_redundant_load(namespace):
    """Test whether aliasing prevents loads from being removed."""
    const = IntLiteral(23)
    refA = namespace.addRegister("a")
    refB = namespace.addRegister("b")
    refC = namespace.addRegister("c")
    refX = namespace.addArgument("X")
    loadA1 = namespace.emitLoad(refA)
    namespace.emitStore(refX, const)
    loadA2 = namespace.emitLoad(refA)
    namespace.emitStore(refB, loadA1)
    namespace.emitStore(refC, loadA2)

    def correct():
        loadA1 = Load(refA.bits.storage)
        yield loadA1
        yield Store(const, refX.bits.storage)
        loadA2 = Load(refA.bits.storage)
        yield loadA2
        yield Store(loadA1.expr, refB.bits.storage)
        yield Store(loadA2.expr, refC.bits.storage)

    code = createSimplifiedCode(namespace)
    assertNodes(code.nodes, correct())


def test_same_value_redundant_load(namespace):
    """Test handling of writing the same value to a potential alias."""
    refA = namespace.addRegister("a")
    refB = namespace.addRegister("b")
    refX = namespace.addArgument("X")
    loadA1 = namespace.emitLoad(refA)
    namespace.emitStore(refX, loadA1)
    loadA2 = namespace.emitLoad(refA)
    namespace.emitStore(refB, loadA2)

    def correct():
        loadA = Load(refA.bits.storage)
        yield loadA
        yield Store(loadA.expr, refX.bits.storage)
        yield Store(loadA.expr, refB.bits.storage)

    code = createSimplifiedCode(namespace)
    assertNodes(code.nodes, correct())


def test_local_value(namespace):
    """Test whether load and stores of variables are removed."""
    refA = namespace.addRegister("a")
    refB = namespace.addRegister("b")
    refV = namespace.addVariable("V")
    loadA = namespace.emitLoad(refA)
    namespace.emitStore(refV, loadA)
    loadV = namespace.emitLoad(refV)
    namespace.emitStore(refB, loadV)

    def correct():
        loadA = Load(refA.bits.storage)
        yield loadA
        yield Store(loadA.expr, refB.bits.storage)

    code = createSimplifiedCode(namespace)
    assertNodes(code.nodes, correct())


def test_unused_storage_removal(namespace):
    """Test whether unused storages are removed."""
    refA = namespace.addRegister("a")
    loadA = namespace.emitLoad(refA)
    refM = namespace.addIOStorage("mem", loadA)

    correct = ()

    code = createSimplifiedCode(namespace)
    assertNodes(code.nodes, correct)


def test_return_value(namespace):
    """Test whether a return value is stored correctly."""
    refV = namespace.addArgument("V")
    refA = namespace.addRegister("a")
    refRet = namespace.addVariable("ret")
    loadA = namespace.emitLoad(refA)
    loadV = namespace.emitLoad(refV)
    combined = OrOperator(loadA, loadV)
    namespace.emitStore(refRet, combined)

    correct = (
        Load(refA.bits.storage),
        Load(refV.bits.storage),
    )

    code = createSimplifiedCode(namespace)
    assertNodes(code.nodes, correct)
    retVal, retWidth = getRetVal(code)
    valueV = code.nodes[0].expr
    valueA = code.nodes[1].expr
    assert retWidth == 8
    assertOr(retVal, simplifyExpression(valueA), simplifyExpression(valueV))


def test_retbits_override(namespace):
    """Test code block creation with a non-default returned bit string."""
    refV = namespace.addVariable("V", IntType.u(20))
    value = IntLiteral(604)
    namespace.emitStore(refV, value)

    code = namespace.createCodeBlock(refV)
    assert len(code.returned) == 1
    (retBits,) = code.returned
    assert retBits.width == 20
    assertRetVal(code, 604)


def test_return_io_index(namespace):
    """Test returning an I/O reference with a simplifiable index."""
    addr = AddOperator(IntLiteral(1), IntLiteral(1))
    memByte = namespace.addIOStorage("mem", addr)
    namespace.addRetReference(memByte)

    code = createSimplifiedCode(namespace)
    assert len(code.returned) == 1
    (retBits,) = code.returned
    assert isinstance(retBits, SingleStorage)
    assert retBits.width == 8
    storage = retBits.storage
    assert isinstance(storage, IOStorage)
    assertIntLiteral(storage.index, 2)


def test_return_redundant_load_index(namespace):
    """Test returning a redundant loaded value."""
    refA = namespace.addRegister("a", IntType.u(16))
    namespace.emitStore(refA, IntLiteral(0x4120))
    loadA = namespace.emitLoad(refA)
    memByte = namespace.addIOStorage("mem", loadA)
    namespace.addRetReference(memByte)

    code = createSimplifiedCode(namespace)
    assert len(code.returned) == 1
    (retBits,) = code.returned
    assert isinstance(retBits, SingleStorage)
    assert retBits.width == 8
    storage = retBits.storage
    assert isinstance(storage, IOStorage)
    assertIntLiteral(storage.index, 0x4120)


def test_return_fixed_value_ref(namespace):
    """Test returning a reference to a fixed value."""
    add = AddOperator(IntLiteral(1), IntLiteral(2))
    value = FixedValue(add, 8)
    namespace.addRetReference(Reference(value, IntType.u(8)))
    code = createSimplifiedCode(namespace)
    assertNodes(code.nodes, ())
    assert len(code.returned) == 1
    (retBits,) = code.returned
    assert isinstance(retBits, FixedValue)
    assert retBits.width == 8
    assertIntLiteral(retBits.expr, 3)


def test_return_complex_ref(namespace):
    """Test returning a non-trivial reference."""
    refH = namespace.addRegister("h")
    refL = namespace.addRegister("l")
    bitsHL = ConcatenatedBits(refL.bits, refH.bits)
    slicedBits = SlicedBits(bitsHL, IntLiteral(0), 8)
    namespace.addRetReference(Reference(slicedBits, IntType.u(8)))
    code = createSimplifiedCode(namespace)
    assertNodes(code.nodes, ())
    assert len(code.returned) == 1
    (retBits,) = code.returned
    assert retBits.width == 8
    # Note that we only simplify expressions, not references, so the
    # reference itself is still complex. All we really check here is
    # that code block creation doesn't break, but that is worthwhile
    # in itself.


def run_repeated_increase(namespace, counterRef, counterRemains):
    """Helper method for repeated increase tests."""

    def emitInc():
        loadCounter = namespace.emitLoad(counterRef)
        incA = AddOperator(loadCounter, IntLiteral(1))
        namespace.emitStore(counterRef, incA)

    initCounter = IntLiteral(23)
    namespace.emitStore(counterRef, initCounter)
    emitInc()
    emitInc()
    emitInc()
    finalCounter = namespace.emitLoad(counterRef)
    ret = namespace.addVariable("ret")
    namespace.emitStore(ret, finalCounter)

    code = createSimplifiedCode(namespace)
    retVal, retWidth = getRetVal(code)
    correct = []
    if counterRemains:
        correct.insert(0, Store(retVal, counterRef.bits.storage))
    assertNodes(code.nodes, correct)
    assertRetVal(code, 26)
    assert retWidth == 8


def test_repeated_increase_reg(namespace):
    """Test removal of redundant loads and stores to a register."""
    refA = namespace.addRegister("a")
    run_repeated_increase(namespace, refA, True)


def test_repeated_increase_var(namespace):
    """Test removal of redundant loads and stores to a local variable."""
    refA = namespace.addVariable("A")
    run_repeated_increase(namespace, refA, False)


def run_signed_load(namespace, write, compare):
    """Helper method for signed load tests."""
    signedVar = namespace.addVariable("s", IntType.s(8))
    namespace.emitStore(signedVar, IntLiteral(write))
    loaded = namespace.emitLoad(signedVar)
    ret = namespace.addVariable("ret", IntType.int)
    namespace.emitStore(ret, loaded)

    code = createSimplifiedCode(namespace)
    assertRetVal(code, compare)


def test_signed_load_positive(namespace):
    """Test loading of a positive signed integer."""
    run_signed_load(namespace, 56, 56)


def test_signed_load_negative(namespace):
    """Test loading of a negative signed integer."""
    run_signed_load(namespace, -78, -78)


def test_signed_load_wrap(namespace):
    """Test loading of an unsigned integer as signed."""
    run_signed_load(namespace, 135, 135 - 256)


def test_signed_load_unlimited(namespace):
    """Test loading of an unlimited width integer."""

    # No sign extension should be happening here, but the code once
    # contained a bug where it would try to apply sign extension and
    # triggered an internal consistency check.

    signedVar = namespace.addVariable("s", IntType.int)
    namespace.emitStore(signedVar, IntLiteral(987654321))
    loaded = namespace.emitLoad(signedVar)
    ret = namespace.addVariable("ret", IntType.int)
    namespace.emitStore(ret, loaded)

    code = createSimplifiedCode(namespace)
    assertRetVal(code, 987654321)


def test_6502_pull(namespace):
    """Test simplification of the 6502 PULL instructions."""

    refD = namespace.addArgument("D")
    refS = namespace.addRegister("s")
    loadS1 = namespace.emitLoad(refS)
    incS = AddOperator(loadS1, IntLiteral(1))
    namespace.emitStore(refS, incS)
    const1 = IntLiteral(1)
    loadS2 = namespace.emitLoad(refS)
    refM = namespace.addIOStorage("mem", makeConcat(const1, loadS2, 8))
    loadM = namespace.emitLoad(refM)
    namespace.emitStore(refD, loadM)

    code = createSimplifiedCode(namespace)
    (ioStorage,) = (
        node.storage for node in code.nodes if isinstance(node.storage, IOStorage)
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

    assertNodes(code.nodes, correct())
