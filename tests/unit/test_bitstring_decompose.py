from utils_codeblock import TestNamespace

from retroasm.codeblock import Load, LoadedValue, Store
from retroasm.expression import (
    AndOperator, Expression, IntLiteral, LShift, OrOperator, RVShift
    )
from retroasm.reference import (
    BitString, ConcatenatedBits, SingleStorage, SlicedBits
    )
from retroasm.types import IntType, maskForWidth, unlimited, widthForMask

import unittest


def sliceBits(ref, offset, width):
    return SlicedBits(ref, IntLiteral(offset), width)

class DecomposeTests:
    '''Abstract base class for bit string decompose tests.'''

    def setUp(self):
        self.namespace = TestNamespace()

    def decomposeExpr(self, expr):
        assert isinstance(expr, Expression)
        if isinstance(expr, AndOperator):
            assert len(expr.exprs) == 2
            subExpr, maskExpr = expr.exprs
            assert isinstance(maskExpr, IntLiteral)
            mask = maskExpr.value
            maskWidth = widthForMask(mask)
            assert maskForWidth(maskWidth) == mask
            for dex, offset, width, shift in self.decomposeExpr(subExpr):
                width = min(width, maskWidth - shift)
                if width > 0:
                    yield dex, offset, width, shift
        elif isinstance(expr, OrOperator):
            for subExpr in expr.exprs:
                yield from self.decomposeExpr(subExpr)
        elif isinstance(expr, LShift):
            for dex, offset, width, shift in self.decomposeExpr(expr.expr):
                yield dex, offset, width, shift + expr.offset
        elif isinstance(expr, RVShift):
            assert isinstance(expr.offset, IntLiteral)
            rvOffset = expr.offset.value
            for dex, offset, width, shift in self.decomposeExpr(expr.expr):
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

    def assertDecomposed(self, bits, expected):
        '''Perform all decomposition checks we have.'''
        raise NotImplementedError

    def test_single(self):
        '''Test construction of SingleStorage.'''
        ref0 = self.namespace.addArgument('R0')
        expected = (
            (ref0.bits.storage, 0, 8),
            )
        self.assertDecomposed(ref0.bits, expected)

    def test_basic_concat(self):
        '''Checks construction of ConcatenatedBits.'''
        ref0 = self.namespace.addArgument('R0', IntType.u(7))
        ref1 = self.namespace.addArgument('R1', IntType.u(3))
        ref2 = self.namespace.addArgument('R2', IntType.u(13))
        concat = ConcatenatedBits(ref2.bits, ref1.bits, ref0.bits)
        expected = (
            (ref2.bits.storage, 0, 13),
            (ref1.bits.storage, 0, 3),
            (ref0.bits.storage, 0, 7),
            )
        self.assertDecomposed(concat, expected)

    def test_self_concat(self):
        '''Checks concatenation of a bit string to itself.'''
        ref0 = self.namespace.addArgument('R0')
        concat = ConcatenatedBits(ref0.bits, ref0.bits)
        expected = (
            (ref0.bits.storage, 0, 8),
            (ref0.bits.storage, 0, 8),
            )
        self.assertDecomposed(concat, expected)

    def test_basic_slice(self):
        '''Checks construction of SlicedBits.'''
        ref0 = self.namespace.addArgument('R0')
        sliced = sliceBits(ref0.bits, 2, 3)
        expected = (
            (ref0.bits.storage, 2, 3),
            )
        self.assertDecomposed(sliced, expected)

    def test_slice_past_end(self):
        '''Checks clipping of slice width against parent width.'''
        ref0 = self.namespace.addArgument('R0')
        sliced = sliceBits(ref0.bits, 2, 30)
        expected = (
            (ref0.bits.storage, 2, 6),
            )
        self.assertDecomposed(sliced, expected)

    def test_slice_outside(self):
        '''Checks handling of slice index outside parent width.'''
        ref0 = self.namespace.addArgument('R0')
        sliced = sliceBits(ref0.bits, 12, 30)
        expected = (
            )
        self.assertDecomposed(sliced, expected)

    def test_slice_concat(self):
        '''Checks slicing concatenated values.'''
        ref0 = self.namespace.addArgument('R0')
        ref1 = self.namespace.addArgument('R1')
        ref2 = self.namespace.addArgument('R2')
        concat = ConcatenatedBits(ref2.bits, ref1.bits, ref0.bits)
        sliced = sliceBits(concat, 5, 13)
        expected = (
            (ref2.bits.storage, 5, 3),
            (ref1.bits.storage, 0, 8),
            (ref0.bits.storage, 0, 2),
            )
        self.assertDecomposed(sliced, expected)

    def test_combined(self):
        '''Checks combinations of slicing and concatenation.'''
        ref0 = self.namespace.addArgument('R0')
        ref1 = self.namespace.addArgument('R1')
        concatA = ConcatenatedBits(ref0.bits, ref1.bits)
        sliceA = sliceBits(concatA, 5, 6)
        ref2 = self.namespace.addArgument('R2')
        concatB = ConcatenatedBits(sliceA, ref2.bits)
        storage = sliceBits(concatB, 4, 7)
        expected = (
            (ref1.bits.storage, 1, 2),
            (ref2.bits.storage, 0, 5),
            )
        self.assertDecomposed(storage, expected)

    def test_nested_slice(self):
        '''Checks taking a slice from sliced bit strings.'''
        ref0 = self.namespace.addArgument('R0')
        ref1 = self.namespace.addArgument('R1')
        slice0 = sliceBits(ref0.bits, 2, 5)
        slice1 = sliceBits(ref1.bits, 1, 4)
        concat = ConcatenatedBits(slice0, slice1)
        sliceC = sliceBits(concat, 3, 3)
        expected = (
            (ref0.bits.storage, 5, 2),
            (ref1.bits.storage, 1, 1),
            )
        self.assertDecomposed(sliceC, expected)

class DecomposeFlattenTests(DecomposeTests, unittest.TestCase):
    '''Tests whether the data structure that contains the bit string matches
    our expected output. This is more likely to find errors in the test
    cases rather than the code under test, but it is valuable anyway as
    a sanity check separate from the more complex load and store testing.
    '''

    def flattenBits(self, bits):
        assert isinstance(bits, BitString)
        if isinstance(bits, SingleStorage):
            yield bits.storage, 0, bits.width
        elif isinstance(bits, ConcatenatedBits):
            for sub in bits:
                yield from self.flattenBits(sub)
        elif isinstance(bits, SlicedBits):
            assert isinstance(bits.offset, IntLiteral)
            offset = bits.offset.value
            width = bits.width
            for storage, subOffset, subWidth in self.flattenBits(bits.bits):
                start = subOffset + max(offset, 0)
                end = subOffset + min(offset + width, subWidth)
                if start < end:
                    yield storage, start, end - start
                offset -= subWidth
        else:
            self.fail('Unsupported BitString subtype: %s' % type(bits).__name__)

    def assertDecomposed(self, bits, expected):
        # Check that a bit string flattens as expected.
        i = 0
        width = 0
        for actualItem in self.flattenBits(bits):
            try:
                expectedItem = expected[i]
            except IndexError:
                self.fail(
                    'Bit string produced more than the %d expected items'
                    % len(expected)
                    )
            else:
                i += 1
            assert actualItem == expectedItem
            width += actualItem[2]
        if i < len(expected):
            self.fail(
                'Bit string produced only %d of the %d expected items'
                % (i, len(expected))
                )
        assert width <= bits.width

class DecomposeLoadTests(DecomposeTests, unittest.TestCase):
    '''Tests loading from bit strings.'''

    def assertDecomposed(self, bits, expected):
        # Check that emitLoad only emits Load nodes.
        value = self.namespace.emitLoad(bits)
        nodes = self.namespace.builder.nodes
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
        decomposedVal = tuple(self.decomposeExpr(value))
        mask = 0
        for dex, offset, width, shift in decomposedVal:
            termMask = maskForWidth(width) << shift
            assert mask & termMask == 0, 'loaded terms overlap'

        # Check loaded value.
        assert len(decomposedVal) == len(expected)
        offset = 0
        for actualItem, expectedItem in zip(decomposedVal, expected):
            valExpr, valOffset, valWidth, valShift = actualItem
            expStorage, expOffset, expWidth = expectedItem
            assert isinstance(valExpr, LoadedValue)
            assert valExpr.load.storage == expStorage
            assert valShift == offset
            assert valOffset == expOffset
            assert valWidth == expWidth
            offset += valWidth

class DecomposeStoreTests(DecomposeTests, unittest.TestCase):
    '''Tests storing to bit strings.'''

    def iterSlices(self, bits):
        '''Iterates through the SlicedBits contained in the given bit string.
        '''
        assert isinstance(bits, BitString)
        if isinstance(bits, SingleStorage):
            pass
        elif isinstance(bits, ConcatenatedBits):
            for sub in bits:
                yield from self.iterSlices(sub)
        elif isinstance(bits, SlicedBits):
            yield bits
        else:
            self.fail('Unsupported BitString subtype: %s' % type(bits).__name__)

    def iterSliceLoads(self, bits):
        '''Iterates through the storages that must be loaded when storing into
        a bit string that may contain slicing.
        Sliced bit strings must load their original version to combine it with
        the written value before the store happens. If sliced bit strings are
        nested, the same storage might exist within multiple parts of the tree.
        To avoid losing updates, each node must complete its load-combine-store
        cycle before other nodes can be processed.
        '''
        for sliceBits in self.iterSlices(bits):
            yield from sliceBits.iterStorages()
            yield from self.iterSliceLoads(sliceBits.bits)

    def assertDecomposed(self, bits, expected):
        # Check that emitStore only emits Load and Store nodes.
        nodes = self.namespace.builder.nodes
        valueRef = self.namespace.addArgument('V', IntType.int)
        value = self.namespace.emitLoad(valueRef)
        initIdx = len(nodes)
        self.namespace.emitStore(bits, value)
        loadNodes = []
        storeNodes = []
        for node in nodes[initIdx:]:
            assert isinstance(node, (Load, Store)), 'unexpected node type'
            (loadNodes if isinstance(node, Load) else storeNodes).append(node)

        # Check that all storages reachable through slicing are loaded from.
        # Also check that the load order is as expected (see iterSliceLoads
        # docstring).
        slicedStorages = tuple(self.iterSliceLoads(bits))
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
