from retroasm.codeblock import BoundReference, ComputedConstant
from retroasm.codeblock_builder import (
    GlobalCodeBlockBuilder, LocalCodeBlockBuilder
    )
from retroasm.context import Context
from retroasm.expression import Expression, IntLiteral
from retroasm.expression_simplifier import simplifyExpression
from retroasm.storage import FixedValue, IOChannel, Register
from retroasm.types import IntType

class NodeChecker:

    def assertNode(self, actual, correct):
        self.assertIsInstance(actual, type(correct))
        self.assertEqual(actual.cid, correct.cid)
        self.assertEqual(actual.sid, correct.sid)

    def assertNodes(self, actual, correct):
        self.assertEqual(len(actual), len(correct))
        for a, c in zip(actual, correct):
            self.assertNode(a, c)

    def assertIntLiteral(self, constant, value):
        self.assertIsInstance(constant, ComputedConstant)
        self.assertIsInstance(constant.expr, IntLiteral)
        self.assertEqual(constant.expr.value, value)

    def getCid(self, expr):
        self.assertIsInstance(expr, Expression)
        simplified = simplifyExpression(expr)
        return simplified.cid

    def getSid(self, ref):
        self.assertIsInstance(ref, BoundReference)
        decomposed = tuple(ref)
        self.assertEqual(len(decomposed), 1)
        (sid, index, width_), = decomposed
        self.assertEqual(index, 0)
        return sid

    def getRetVal(self, code):
        retRef = code.retRef
        self.assertIsNotNone(retRef)
        self.assertIsInstance(retRef, BoundReference)
        decomposed = tuple(retRef)
        self.assertEqual(len(decomposed), 1)
        (sid, index, width), = decomposed
        self.assertEqual(index, 0)
        storage = code.storages[sid]
        self.assertIsInstance(storage, FixedValue)
        return storage.cid, width

    def assertRetVal(self, code, value):
        cid, width = self.getRetVal(code)
        constant = code.constants[cid]
        self.assertIsInstance(constant, ComputedConstant)
        self.assertIsInstance(constant.expr, IntLiteral)
        self.assertEqual(constant.expr.value & ((1 << width) - 1), value)

class TestCodeBlockBuilder(LocalCodeBlockBuilder):

    def __init__(self, globalBuilder=None):
        if globalBuilder is None:
            globalBuilder = GlobalCodeBlockBuilder()
        self.globalBuilder = globalBuilder
        LocalCodeBlockBuilder.__init__(self, globalBuilder)

    def emitLoad(self, boundRef, location=None):
        return super().emitLoad(boundRef, location)

    def emitStore(self, boundRef, expr, location=None):
        return super().emitStore(boundRef, expr, location)

    def addRegister(self, name, width=8):
        try:
            ref = self.context[name]
        except KeyError:
            # Insert register into global context.
            reg = Register(name, IntType(width))
            self.globalBuilder.emitRegister(reg, None)
            ref = self.context[name]

        # Check that existing global context entry is this register.
        assert isinstance(ref, BoundReference), ref
        (sid, index, cmpWidth), = ref
        assert index == 0, ref
        assert width == cmpWidth, ref
        reg = self.globalBuilder.storages[sid]
        assert reg.name == name, reg
        assert reg.width == width

        return ref

    def addIOStorage(self, channelName, index,
            elemType=IntType(8), addrType=IntType(16)):
        try:
            channel = self.globalBuilder.context[channelName]
        except KeyError:
            # Insert channel into global context.
            channel = IOChannel(channelName, elemType, addrType)
            self.globalBuilder.context.define(channelName, channel, None)
        else:
            # Check that existing global context entry is this channel.
            assert isinstance(channel, IOChannel), channel
            assert channel.elemType is elemType, channel
            assert channel.addrType is addrType, channel
        # Import channel from global context into local context.
        localChannel = self.context[channelName]
        assert localChannel is channel
        # Create I/O storage.
        return self.emitIOReference(localChannel, index)

    def addValueArgument(self, name, typ=IntType(8)):
        return self.emitValueArgument(name, typ, None)

    def addReferenceArgument(self, name, typ=IntType(8)):
        return self.emitReferenceArgument(name, typ, None)

    def addVariable(self, name, typ=IntType(8)):
        return self.emitVariable(name, typ, None)
