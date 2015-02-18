from retroasm.codeblock import ComputedConstant
from retroasm.codeblock_builder import (
    GlobalCodeBlockBuilder, LocalCodeBlockBuilder
    )
from retroasm.context import Context
from retroasm.expression import IntLiteral
from retroasm.storage import ComposedStorage, IOChannel, Register
from retroasm.types import IntType

class NodeChecker:

    def assertNode(self, actual, correct):
        self.assertIsInstance(actual, type(correct))
        self.assertEqual(actual.cid, correct.cid)
        self.assertEqual(actual.rid, correct.rid)

    def assertNodes(self, actual, correct):
        self.assertEqual(len(actual), len(correct))
        for a, c in zip(actual, correct):
            self.assertNode(a, c)

    def assertIntLiteral(self, constant, value):
        self.assertIsInstance(constant, ComputedConstant)
        self.assertIsInstance(constant.expr, IntLiteral)
        self.assertEqual(constant.expr.value, value)

class TestCodeBlockBuilder(LocalCodeBlockBuilder):

    def __init__(self, globalBuilder=None):
        if globalBuilder is None:
            globalBuilder = GlobalCodeBlockBuilder()
        self.globalBuilder = globalBuilder
        LocalCodeBlockBuilder.__init__(self, globalBuilder)

    def emitLoad(self, rid, location=None):
        return super().emitLoad(rid, location)

    def emitStore(self, rid, expr, location=None):
        return super().emitStore(rid, expr, location)

    def addRegister(self, name, width=8):
        try:
            storage = self.context[name]
        except KeyError:
            # Insert register into global context.
            reg = Register(name, IntType(width))
            self.globalBuilder.emitRegister(reg, None)
            storage = self.context[name]

        # Check that existing global context entry is this register.
        assert isinstance(storage, ComposedStorage), storage
        (rid, index, cmpWidth), = storage
        assert index == 0, storage
        assert width == cmpWidth, storage
        reg = self.globalBuilder.references[rid]
        assert reg.name == name, reg
        assert reg.width == width
        return rid

    def addIOReference(self, channelName, index, elemWidth=8, addrWidth=16):
        try:
            channel = self.globalBuilder.context[channelName]
        except KeyError:
            # Insert channel into global context.
            channel = IOChannel(
                channelName, IntType(elemWidth), IntType(addrWidth)
                )
            self.globalBuilder.context.define(channelName, channel, None)
        else:
            # Check that existing global context entry is this channel.
            assert isinstance(channel, IOChannel), channel
            assert channel.elemType.width == elemWidth, channel
            assert channel.addrType.width == addrWidth, channel
        # Import channel from global context into local context.
        localChannel = self.context[channelName]
        assert localChannel is channel
        # Create IOReference.
        return self.emitIOReference(localChannel, index)

    def addLocalReference(self, name, width=8):
        return self.emitLocalReference(name, IntType(width), None)

    def addValueArgument(self, name, width=8):
        return self.emitValueArgument(name, IntType(width), None)

    def addVariable(self, name, width=8):
        return self.emitVariable(name, IntType(width), None)
