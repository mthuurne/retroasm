from utils_codeblock import TestCodeBlockBuilder

from retroasm.codeblock import (
    ConstantValue, ConcatenatedReference, Load, Reference, SingleReference,
    SlicedReference, Store
    )
from retroasm.expression import (
    AndOperator, Expression, IntLiteral, LShift, OrOperator, RVShift
    )
from retroasm.types import IntType, maskForWidth, unlimited, widthForMask

import unittest

def sliceRef(ref, offset, width):
    return SlicedReference(ref, IntLiteral(offset), IntLiteral(width))

class DecomposeTests:
    '''Abstract base class for reference decompose tests.'''

    def setUp(self):
        self.builder = TestCodeBlockBuilder()

    def decomposeExpr(self, expr):
        self.assertIsInstance(expr, Expression)
        if isinstance(expr, ConstantValue):
            maskWidth = widthForMask(expr.mask)
            yield expr.cid, 0, maskWidth, 0
        elif isinstance(expr, AndOperator):
            self.assertEqual(len(expr.exprs), 2)
            subExpr, maskExpr = expr.exprs
            self.assertIsInstance(maskExpr, IntLiteral)
            mask = maskExpr.value
            maskWidth = widthForMask(mask)
            self.assertEqual(maskForWidth(maskWidth), mask)
            for cid, offset, width, shift in self.decomposeExpr(subExpr):
                width = min(width, maskWidth - shift)
                if width > 0:
                    yield cid, offset, width, shift
        elif isinstance(expr, OrOperator):
            for subExpr in expr.exprs:
                yield from self.decomposeExpr(subExpr)
        elif isinstance(expr, LShift):
            for cid, offset, width, shift in self.decomposeExpr(expr.expr):
                yield cid, offset, width, shift + expr.offset
        elif isinstance(expr, RVShift):
            self.assertIsInstance(expr.offset, IntLiteral)
            rvOffset = expr.offset.value
            for cid, offset, width, shift in self.decomposeExpr(expr.expr):
                shift -= rvOffset
                if shift < 0:
                    droppedBits = -shift
                    shift = 0
                    offset += droppedBits
                    width -= droppedBits
                    if width <= 0:
                        continue
                yield cid, offset, width, shift
        else:
            self.fail('Unsupported Expression subtype: %s' % type(expr).__name__)

    def assertDecomposed(self, ref, expected):
        '''Perform all decomposition checks we have.'''
        raise NotImplementedError

    def test_single(self):
        '''Test construction of SingleReference.'''
        ref0 = self.builder.addReferenceArgument('R0')
        expected = (
            (ref0.sid, 0, 8),
            )
        self.assertDecomposed(ref0, expected)

    def test_basic_concat(self):
        '''Checks construction of ConcatenatedReference.'''
        ref0 = self.builder.addReferenceArgument('R0', IntType.u(7))
        ref1 = self.builder.addReferenceArgument('R1', IntType.u(3))
        ref2 = self.builder.addReferenceArgument('R2', IntType.u(13))
        concat = ConcatenatedReference(ref2, ref1, ref0)
        expected = (
            (ref2.sid, 0, 13),
            (ref1.sid, 0, 3),
            (ref0.sid, 0, 7),
            )
        self.assertDecomposed(concat, expected)

    def test_self_concat(self):
        '''Checks concatenation of a reference to itself.'''
        ref0 = self.builder.addReferenceArgument('R0')
        concat = ConcatenatedReference(ref0, ref0)
        expected = (
            (ref0.sid, 0, 8),
            (ref0.sid, 0, 8),
            )
        self.assertDecomposed(concat, expected)

    def test_basic_slice(self):
        '''Checks construction of SlicedReference.'''
        ref0 = self.builder.addReferenceArgument('R0')
        sliced = sliceRef(ref0, 2, 3)
        expected = (
            (ref0.sid, 2, 3),
            )
        self.assertDecomposed(sliced, expected)

    def test_slice_past_end(self):
        '''Checks clipping of slice width against parent width.'''
        ref0 = self.builder.addReferenceArgument('R0')
        sliced = sliceRef(ref0, 2, 30)
        expected = (
            (ref0.sid, 2, 6),
            )
        self.assertDecomposed(sliced, expected)

    def test_slice_outside(self):
        '''Checks handling of slice index outside parent width.'''
        ref0 = self.builder.addReferenceArgument('R0')
        sliced = sliceRef(ref0, 12, 30)
        expected = (
            )
        self.assertDecomposed(sliced, expected)

    def test_slice_concat(self):
        '''Checks slicing concatenated values.'''
        ref0 = self.builder.addReferenceArgument('R0')
        ref1 = self.builder.addReferenceArgument('R1')
        ref2 = self.builder.addReferenceArgument('R2')
        concat = ConcatenatedReference(ref2, ref1, ref0)
        sliced = sliceRef(concat, 5, 13)
        expected = (
            (ref2.sid, 5, 3),
            (ref1.sid, 0, 8),
            (ref0.sid, 0, 2),
            )
        self.assertDecomposed(sliced, expected)

    def test_combined(self):
        '''Checks combinations of slicing and concatenation.'''
        ref0 = self.builder.addReferenceArgument('R0')
        ref1 = self.builder.addReferenceArgument('R1')
        concatA = ConcatenatedReference(ref0, ref1)
        sliceA = sliceRef(concatA, 5, 6)
        ref2 = self.builder.addReferenceArgument('R2')
        concatB = ConcatenatedReference(sliceA, ref2)
        storage = sliceRef(concatB, 4, 7)
        expected = (
            (ref1.sid, 1, 2),
            (ref2.sid, 0, 5),
            )
        self.assertDecomposed(storage, expected)

    def test_nested_slice(self):
        '''Checks taking a slice from sliced references.'''
        ref0 = self.builder.addReferenceArgument('R0')
        ref1 = self.builder.addReferenceArgument('R1')
        slice0 = sliceRef(ref0, 2, 5)
        slice1 = sliceRef(ref1, 1, 4)
        concat = ConcatenatedReference(slice0, slice1)
        sliceC = sliceRef(concat, 3, 3)
        expected = (
            (ref0.sid, 5, 2),
            (ref1.sid, 1, 1),
            )
        self.assertDecomposed(sliceC, expected)

class DecomposeFlattenTests(DecomposeTests, unittest.TestCase):
    '''Tests whether the data structure that contains the reference matches
    our expected output. This is more likely to find errors in the test
    cases rather than the code under test, but it is valuable anyway as
    a sanity check separate from the more complex load and store testing.
    '''

    def flattenRef(self, ref):
        self.assertIsInstance(ref, Reference)
        if isinstance(ref, SingleReference):
            yield ref.sid, 0, ref.width
        elif isinstance(ref, ConcatenatedReference):
            for subRef in ref:
                yield from self.flattenRef(subRef)
        elif isinstance(ref, SlicedReference):
            self.assertIsInstance(ref.offset, IntLiteral)
            offset = ref.offset.value
            width = ref.width
            for sid, subOffset, subWidth in self.flattenRef(ref.ref):
                start = subOffset + max(offset, 0)
                end = subOffset + min(offset + width, subWidth)
                if start < end:
                    yield sid, start, end - start
                offset -= subWidth
        else:
            self.fail('Unsupported Reference subtype: %s' % type(ref).__name__)

    def assertDecomposed(self, ref, expected):
        # Check that a reference flattens as expected.
        i = 0
        width = 0
        for actualItem in self.flattenRef(ref):
            try:
                expectedItem = expected[i]
            except IndexError:
                self.fail(
                    'Reference produced more than the %d expected items'
                    % len(expected)
                    )
            else:
                i += 1
            self.assertEqual(actualItem, expectedItem)
            width += actualItem[2]
        if i < len(expected):
            self.fail(
                'Reference produced only %d of the %d expected items'
                % (i, len(expected))
                )
        self.assertLessEqual(width, ref.width)

class DecomposeLoadTests(DecomposeTests, unittest.TestCase):
    '''Tests loading from references.'''

    def assertDecomposed(self, ref, expected):
        # Check that emitLoad only emits Load nodes.
        value = self.builder.emitLoad(ref)
        nodes = self.builder.nodes
        for node in nodes:
            self.assertIsInstance(node, Load)

        # Check that all referenced storages are loaded from.
        # Also check that the load order matches the depth-first ref tree walk.
        # Even storages that are not part of the decomposition should still be
        # loaded from since loading might trigger side effects.
        refSIDs = tuple(ref.iterSIDs())
        loadedSIDs = tuple(node.sid for node in nodes)
        self.assertEqual(refSIDs, loadedSIDs)

        # Check the loaded value expression's bit mask.
        self.assertLessEqual(widthForMask(value.mask), ref.width,
            'loaded value is wider than reference')

        # Check that the loaded expression's terms don't overlap.
        decomposedVal = tuple(self.decomposeExpr(value))
        mask = 0
        for cid, offset, width, shift in decomposedVal:
            termMask = maskForWidth(width) << shift
            self.assertEqual(mask & termMask, 0, 'loaded terms overlap')

        # Check loaded value.
        self.assertEqual(len(decomposedVal), len(expected))
        offset = 0
        constants = self.builder.constants
        for actualItem, expectedItem in zip(decomposedVal, expected):
            valCid, valOffset, valWidth, valShift = actualItem
            expSid, expOffset, expWidth = expectedItem
            const = constants[valCid]
            self.assertEqual(valShift, offset)
            self.assertEqual(const.sid, expSid)
            self.assertEqual(valOffset, expOffset)
            self.assertEqual(valWidth, expWidth)
            offset += valWidth

class DecomposeStoreTests(DecomposeTests, unittest.TestCase):
    '''Tests storing to references.'''

    def iterSlices(self, ref):
        '''Iterates through the SlicedReferences contained in the given
        reference.
        '''
        self.assertIsInstance(ref, Reference)
        if isinstance(ref, SingleReference):
            pass
        elif isinstance(ref, ConcatenatedReference):
            for subRef in ref:
                yield from self.iterSlices(subRef)
        elif isinstance(ref, SlicedReference):
            yield ref
        else:
            self.fail('Unsupported Reference subtype: %s' % type(ref).__name__)

    def iterSliceLoads(self, ref):
        '''Iterates through the storage IDs of the storages that must be loaded
        when storing into a reference that may contain slicing.
        Sliced references must load their original version to combine it with
        the written value before the store happens. If sliced references are
        nested, the same storage might be referenced within multiple parts of
        the tree. To avoid losing updates, each node must complete its
        load-combine-store cycle before other nodes can be processed.
        '''
        for sliceRef in self.iterSlices(ref):
            yield from sliceRef.iterSIDs()
            yield from self.iterSliceLoads(sliceRef.ref)

    def assertDecomposed(self, ref, expected):
        # Check that emitStore only emits Load and Store nodes.
        nodes = self.builder.nodes
        valueRef = self.builder.addValueArgument('V', IntType.int)
        value = self.builder.emitLoad(valueRef)
        initIdx = len(nodes)
        self.builder.emitStore(ref, value)
        loadNodes = []
        storeNodes = []
        for node in nodes[initIdx:]:
            self.assertIsInstance(node, (Load, Store), 'unexpected node type')
            (loadNodes if isinstance(node, Load) else storeNodes).append(node)

        # Check that all storages referenced through slicing are loaded from.
        # Also check that the load order is as expected (see iterSliceLoads
        # docstring).
        slicedSIDs = tuple(self.iterSliceLoads(ref))
        loadedSIDs = tuple(node.sid for node in loadNodes)
        self.assertEqual(slicedSIDs, loadedSIDs)

        # Check that all referenced storages are stored to.
        # Also check that the store order matches the depth-first ref tree walk.
        refSIDs = tuple(ref.iterSIDs())
        storedSIDs = tuple(node.sid for node in storeNodes)
        self.assertEqual(refSIDs, storedSIDs)

        # Note: Verifying that the right values are being stored based on the
        #       expectation list is quite complex.
        #       We're better off writing a separate test for that.

if __name__ == '__main__':
    unittest.main()
