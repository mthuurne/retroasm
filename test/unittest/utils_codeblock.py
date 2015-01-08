from retroasm.codeblock import ComputedConstant
from retroasm.codeblock_builder import CodeBlockBuilder
from retroasm.expression import IntLiteral
from retroasm.storage import (
    IOChannel, IOReference, LocalReference, Register, Variable
    )
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
            globalContext = {}
        self.globalContext = globalContext
        CodeBlockBuilder.__init__(self, globalContext)

    def addRegister(self, name, width=8):
        reg = self.globalContext.get(name)
        if reg is None:
            # Insert register into global context.
            reg = Register(name, IntType(width))
            self.globalContext[name] = reg
        else:
            # Check that existing global context entry is this register.
            assert isinstance(reg, Register), reg
            assert reg.width == width, reg
        # Import register from global context into local context.
        return self.context[name].rid

    def addIOReference(self, channelName, index, elemWidth=8, addrWidth=16):
        channel = self.globalContext.get(channelName)
        if channel is None:
            # Insert channel into global context.
            channel = IOChannel(
                channelName, IntType(elemWidth), IntType(addrWidth)
                )
            self.globalContext[channelName] = channel
        else:
            # Check that existing global context entry is this channel.
            assert isinstance(channel, IOChannel), channel
            assert channel.elemType.width == elemWidth, channel
            assert channel.addrType.width == addrWidth, channel
        # Import channel from global context into local context.
        channel = self.context[channelName]
        # Create IOReference.
        return self.emitIOReference(channel, index)

    def addLocalReference(self, name, width=8):
        return self.emitLocalReference(name, IntType(width))

    def addValueArgument(self, name, width=8):
        return self.emitValueArgument(name, IntType(width))

    def addVariable(self, name, width=8):
        return self.emitVariable(name, IntType(width))
