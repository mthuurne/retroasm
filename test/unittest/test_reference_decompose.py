from utils_codeblock import TestCodeBlockBuilder

from retroasm.codeblock import (
    ConstantValue, ConcatenatedReference, Load, Reference, SingleReference,
    SlicedReference
    )
from retroasm.expression import (
    AndOperator, Expression, IntLiteral, LShift, OrOperator, RShift
    )
from retroasm.types import IntType, maskForWidth, unlimited, widthForMask

import unittest

class DecomposeTests(unittest.TestCase):

    def setUp(self):
        self.builder = TestCodeBlockBuilder()

    def decomposeRef(self, ref):
        self.assertIsInstance(ref, Reference)
        if isinstance(ref, SingleReference):
            yield ref.sid, 0, ref.width
        elif isinstance(ref, ConcatenatedReference):
            for subRef in ref:
                yield from self.decomposeRef(subRef)
        elif isinstance(ref, SlicedReference):
            offset = ref.offset
            width = ref.width
            for sid, subOffset, subWidth in self.decomposeRef(ref.ref):
                start = subOffset + max(offset, 0)
                end = subOffset + min(offset + width, subWidth)
                if start < end:
                    yield sid, start, end - start
                offset -= subWidth
        else:
            self.fail('Unsupported Reference subtype: %s' % type(ref).__name__)

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
        elif isinstance(expr, RShift):
            for cid, offset, width, shift in self.decomposeExpr(expr.expr):
                shift -= expr.offset
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

        # Check that a reference decomposes as expected.
        i = 0
        width = 0
        for actualItem in self.decomposeRef(ref):
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
        sliced = SlicedReference(ref0, 2, 3)
        expected = (
            (ref0.sid, 2, 3),
            )
        self.assertDecomposed(sliced, expected)

    def test_slice_past_end(self):
        '''Checks clipping of slice width against parent width.'''
        ref0 = self.builder.addReferenceArgument('R0')
        sliced = SlicedReference(ref0, 2, 30)
        expected = (
            (ref0.sid, 2, 6),
            )
        self.assertDecomposed(sliced, expected)

    def test_slice_outside(self):
        '''Checks handling of slice index outside parent width.'''
        ref0 = self.builder.addReferenceArgument('R0')
        sliced = SlicedReference(ref0, 12, 30)
        expected = (
            )
        self.assertDecomposed(sliced, expected)

    def test_slice_concat(self):
        '''Checks slicing concatenated values.'''
        ref0 = self.builder.addReferenceArgument('R0')
        ref1 = self.builder.addReferenceArgument('R1')
        ref2 = self.builder.addReferenceArgument('R2')
        concat = ConcatenatedReference(ref2, ref1, ref0)
        sliced = SlicedReference(concat, 5, 13)
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
        sliceA = SlicedReference(concatA, 5, 6)
        ref2 = self.builder.addReferenceArgument('R2')
        concatB = ConcatenatedReference(sliceA, ref2)
        storage = SlicedReference(concatB, 4, 7)
        expected = (
            (ref1.sid, 1, 2),
            (ref2.sid, 0, 5),
            )
        self.assertDecomposed(storage, expected)

if __name__ == '__main__':
    unittest.main()
