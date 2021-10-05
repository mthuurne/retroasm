from .utils_codeblock import TestNamespace

from retroasm.codeblock import Load, LoadedValue, Store
from retroasm.expression import (
    AndOperator, Expression, IntLiteral, LShift, OrOperator, RVShift
    )
from retroasm.reference import (
    BitString, ConcatenatedBits, SingleStorage, SlicedBits
    )
from retroasm.types import IntType, maskForWidth, widthForMask

from pytest import fixture, mark


@fixture
def namespace():
    return TestNamespace()

def sliceBits(ref, offset, width):
    return SlicedBits(ref, IntLiteral(offset), width)

def decomposeExpr(expr):
    assert isinstance(expr, Expression)
    if isinstance(expr, AndOperator):
        assert len(expr.exprs) == 2
        subExpr, maskExpr = expr.exprs
        assert isinstance(maskExpr, IntLiteral)
        mask = maskExpr.value
        maskWidth = widthForMask(mask)
        assert maskForWidth(maskWidth) == mask
        for dex, offset, width, shift in decomposeExpr(subExpr):
            width = min(width, maskWidth - shift)
            if width > 0:
                yield dex, offset, width, shift
    elif isinstance(expr, OrOperator):
        for subExpr in expr.exprs:
            yield from decomposeExpr(subExpr)
    elif isinstance(expr, LShift):
        for dex, offset, width, shift in decomposeExpr(expr.expr):
            yield dex, offset, width, shift + expr.offset
    elif isinstance(expr, RVShift):
        assert isinstance(expr.offset, IntLiteral)
        rvOffset = expr.offset.value
        for dex, offset, width, shift in decomposeExpr(expr.expr):
            shift -= rvOffset
            if shift < 0:
                droppedBits = -shift
                shift = 0
                offset += droppedBits
                width -= droppedBits
                if width <= 0:
                    continue
            yield dex, offset, width, shift
    else:
        yield expr, 0, widthForMask(expr.mask), 0

def iterSlices(bits):
    """Iterate through the SlicedBits contained in the given bit string."""
    assert isinstance(bits, BitString)
    if isinstance(bits, SingleStorage):
        pass
    elif isinstance(bits, ConcatenatedBits):
        for sub in bits:
            yield from iterSlices(sub)
    elif isinstance(bits, SlicedBits):
        yield bits
    else:
        raise TypeError(
            f"Unsupported BitString subtype: {type(bits).__name__}"
            )

def iterSliceLoads(bits):
    """Iterate through the storages that must be loaded when storing into
    a bit string that may contain slicing.
    Sliced bit strings must load their original version to combine it with
    the written value before the store happens. If sliced bit strings are
    nested, the same storage might exist within multiple parts of the tree.
    To avoid losing updates, each node must complete its load-combine-store
    cycle before other nodes can be processed.
    """
    for sliceBits in iterSlices(bits):
        yield from sliceBits.iterStorages()
        yield from iterSliceLoads(sliceBits.bits)

def checkFlatten(namespace, bits, expected):
    """Check that the produced bit string flattens to the expected output.
    This is more likely to find errors in the test cases rather than
    the code under test, but it is valuable anyway as a sanity check
    separate from the more complex load and store testing.
    """

    i = 0
    width = 0
    for base, base_seg in bits.decompose():
        try:
            expectedItem = expected[i]
        except IndexError:
            raise AssertionError(
                f"Bit string produced more than "
                f"the {len(expected)} expected items"
                )
        else:
            i += 1
        assert base.storage == expectedItem[0]
        assert base_seg == expectedItem[1]
        width += base_seg.width
    assert i >= len(expected), \
        f"Bit string produced only {i} of the {len(expected)} expected items"
    assert width <= bits.width

def checkLoad(namespace, bits, expected):
    """Check that loading from a bit string works as expected."""

    # Check that emitLoad only emits Load nodes.
    value = namespace.emitLoad(bits)
    nodes = namespace.builder.nodes
    for node in nodes:
        assert isinstance(node, Load)

    # Check that all underlying storages are loaded from.
    # Also check that the load order matches the depth-first tree walk.
    # Even storages that are not part of the decomposition should still be
    # loaded from since loading might trigger side effects.
    allStorages = tuple(bits.iterStorages())
    loadedStorages = tuple(node.storage for node in nodes)
    assert allStorages == loadedStorages

    # Check the loaded value expression's bit mask.
    assert widthForMask(value.mask) <= bits.width, \
        'loaded value is wider than bit string'

    # Check that the loaded expression's terms don't overlap.
    decomposedVal = tuple(decomposeExpr(value))
    mask = 0
    for dex, offset, width, shift in decomposedVal:
        termMask = maskForWidth(width) << shift
        assert mask & termMask == 0, 'loaded terms overlap'

    # Check loaded value.
    assert len(decomposedVal) == len(expected)
    offset = 0
    for actualItem, expectedItem in zip(decomposedVal, expected):
        valExpr, valOffset, valWidth, valShift = actualItem
        expStorage, expSegment = expectedItem
        assert isinstance(valExpr, LoadedValue)
        assert valExpr.load.storage == expStorage
        assert valShift == offset
        assert valOffset == expSegment.start
        assert valWidth == expSegment.width
        offset += valWidth

def checkStore(namespace, bits, expected):
    """Check that storing to a bit string works as expected."""

    # Check that emitStore only emits Load and Store nodes.
    nodes = namespace.builder.nodes
    valueRef = namespace.addArgument('V', IntType.int)
    value = namespace.emitLoad(valueRef)
    initIdx = len(nodes)
    namespace.emitStore(bits, value)
    loadNodes = []
    storeNodes = []
    for node in nodes[initIdx:]:
        assert isinstance(node, (Load, Store)), 'unexpected node type'
        (loadNodes if isinstance(node, Load) else storeNodes).append(node)

    # Check that all storages reachable through slicing are loaded from.
    # Also check that the load order is as expected (see iterSliceLoads
    # docstring).
    slicedStorages = tuple(iterSliceLoads(bits))
    loadedStorages = tuple(node.storage for node in loadNodes)
    assert slicedStorages == loadedStorages

    # Check that all underlying storages are stored to.
    # Also check that the store order matches the depth-first tree walk.
    allStorages = tuple(bits.iterStorages())
    storedStorages = tuple(node.storage for node in storeNodes)
    assert allStorages == storedStorages

    # Note: Verifying that the right values are being stored based on the
    #       expectation list is quite complex.
    #       We're better off writing a separate test for that.

checkParam = mark.parametrize('check', [checkFlatten, checkLoad, checkStore])

@checkParam
def test_decompose_single(namespace, check):
    """Test construction of SingleStorage."""
    ref0 = namespace.addArgument('R0')
    expected = namespace.parse('R0[0:8]')
    check(namespace, ref0.bits, expected)

@checkParam
def test_decompose_basic_concat(namespace, check):
    """Test construction of ConcatenatedBits."""
    ref0 = namespace.addArgument('R0', IntType.u(7))
    ref1 = namespace.addArgument('R1', IntType.u(3))
    ref2 = namespace.addArgument('R2', IntType.u(13))
    concat = ConcatenatedBits(ref2.bits, ref1.bits, ref0.bits)
    expected = namespace.parse('R2[0:13]', 'R1[0:3]', 'R0[0:7]')
    check(namespace, concat, expected)

@checkParam
def test_decompose_self_concat(namespace, check):
    """Test concatenation of a bit string to itself."""
    ref0 = namespace.addArgument('R0')
    concat = ConcatenatedBits(ref0.bits, ref0.bits)
    expected = namespace.parse('R0[0:8]', 'R0[0:8]')
    check(namespace, concat, expected)

@checkParam
def test_decompose_basic_slice(namespace, check):
    """Test construction of SlicedBits."""
    ref0 = namespace.addArgument('R0')
    sliced = sliceBits(ref0.bits, 2, 3)
    expected = namespace.parse('R0[2:5]')
    check(namespace, sliced, expected)

@checkParam
def test_decompose_slice_past_end(namespace, check):
    """Test clipping of slice width against parent width."""
    ref0 = namespace.addArgument('R0')
    sliced = sliceBits(ref0.bits, 2, 30)
    expected = namespace.parse('R0[2:8]')
    check(namespace, sliced, expected)

@checkParam
def test_decompose_slice_outside(namespace, check):
    """Test handling of slice index outside parent width."""
    ref0 = namespace.addArgument('R0')
    sliced = sliceBits(ref0.bits, 12, 30)
    expected = ()
    check(namespace, sliced, expected)

@checkParam
def test_decompose_slice_concat(namespace, check):
    """Test slicing concatenated values."""
    ref0 = namespace.addArgument('R0')
    ref1 = namespace.addArgument('R1')
    ref2 = namespace.addArgument('R2')
    concat = ConcatenatedBits(ref2.bits, ref1.bits, ref0.bits)
    sliced = sliceBits(concat, 5, 13)
    expected = namespace.parse('R2[5:8]', 'R1[0:8]', 'R0[0:2]')
    check(namespace, sliced, expected)

@checkParam
def test_decompose_combined(namespace, check):
    """Test combinations of slicing and concatenation."""
    ref0 = namespace.addArgument('R0')
    ref1 = namespace.addArgument('R1')
    concatA = ConcatenatedBits(ref0.bits, ref1.bits)
    sliceA = sliceBits(concatA, 5, 6)
    ref2 = namespace.addArgument('R2')
    concatB = ConcatenatedBits(sliceA, ref2.bits)
    storage = sliceBits(concatB, 4, 7)
    expected = namespace.parse('R1[1:3]', 'R2[0:5]')
    check(namespace, storage, expected)

@checkParam
def test_decompose_nested_slice(namespace, check):
    """Test taking a slice from sliced bit strings."""
    ref0 = namespace.addArgument('R0')
    ref1 = namespace.addArgument('R1')
    slice0 = sliceBits(ref0.bits, 2, 5)
    slice1 = sliceBits(ref1.bits, 1, 4)
    concat = ConcatenatedBits(slice0, slice1)
    sliceC = sliceBits(concat, 3, 3)
    expected = namespace.parse('R0[5:7]', 'R1[1:2]')
    check(namespace, sliceC, expected)
