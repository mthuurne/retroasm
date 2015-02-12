from retroasm.codeblock import ComputedConstant
from retroasm.codeblock_builder import CodeBlockBuilder
from retroasm.context import Context
from retroasm.expression import IntLiteral
from retroasm.storage import IOChannel, Register
from retroasm.types import IntType

class NodeChecker:

    def assertNode(self, actual, correct):
        self.assertEqual(type(actual), type(correct))
        self.assertEqual(actual.cid, correct.cid)
        self.assertEqual(actual.rid, correct.rid)

    def assertNodes(self, actual, correct):
        self.assertEqual(len(actual), len(correct))
        for a, c in zip(actual, correct):
            self.assertNode(a, c)

    def assertIntLiteral(self, constant, value):
        self.assertEqual(type(constant), ComputedConstant)
        self.assertEqual(type(constant.expr), IntLiteral)
        self.assertEqual(constant.expr.value, value)

class TestCodeBlockBuilder(CodeBlockBuilder):

    def __init__(self, globalContext=None):
        if globalContext is None:
            globalContext = Context()
        self.globalContext = globalContext
        CodeBlockBuilder.__init__(self, globalContext)

    def emitLoad(self, rid, location=None):
        return super().emitLoad(rid, location)

    def emitStore(self, rid, expr, location=None):
        return super().emitStore(rid, expr, location)

    def addRegister(self, name, width=8):
        try:
            reg = self.globalContext[name]
        except KeyError:
            # Insert register into global context.
            reg = Register(name, IntType(width))
            self.globalContext.define(name, reg, None)
        else:
            # Check that existing global context entry is this register.
            assert isinstance(reg, Register), reg
            assert reg.width == width, reg
        # Import register from global context into local context.
        (localRid, localIndex, localWidth), = self.context[name]
        assert localIndex == 0, localIndex
        assert localWidth == width, (localWidth, width)
        return localRid

    def addIOReference(self, channelName, index, elemWidth=8, addrWidth=16):
        try:
            channel = self.globalContext[channelName]
        except KeyError:
            # Insert channel into global context.
            channel = IOChannel(
                channelName, IntType(elemWidth), IntType(addrWidth)
                )
            self.globalContext.define(channelName, channel, None)
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
