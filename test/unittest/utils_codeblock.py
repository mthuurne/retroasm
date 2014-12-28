from retroasm.codeblock_builder import CodeBlockBuilder
from retroasm.expression import (
    IOChannel, IOReference, IntType, LocalReference, LocalValue, Register
    )

class NodeChecker:

    def assertNode(self, actual, correct):
        self.assertEqual(type(actual), type(correct))
        self.assertEqual(actual.cid, correct.cid)
        self.assertEqual(actual.rid, correct.rid)

    def assertNodes(self, actual, correct):
        self.assertEqual(len(actual), len(correct))
        for a, c in zip(actual, correct):
            self.assertNode(a, c)

class TestCodeBlockBuilder(CodeBlockBuilder):

    def __init__(self):
        CodeBlockBuilder.__init__(self)
        self.channels = {}
        self.registers = {}

    def addRegister(self, name, width=8):
        reg = self.registers.get(name)
        if reg is None:
            reg = Register(name, IntType(width))
            self.registers[name] = reg
        else:
            assert reg.width == width
        return self.emitReference(reg)

    def addLocalReference(self, name, width=8):
        ref = LocalReference(name, IntType(width))
        return self.emitReference(ref)

    def addLocalValue(self, name, width=8):
        ref = LocalValue(name, IntType(width))
        return self.emitReference(ref)

    def addIOReference(self, channelName, index, elemWidth=8, addrWidth=16):
        channel = self.channels.get(channelName)
        if channel is None:
            channel = IOChannel(
                channelName, IntType(elemWidth), IntType(addrWidth)
                )
            self.channels[channelName] = channel
        else:
            assert channel.elemType.width == elemWidth, channel
            assert channel.addrType.width == addrWidth, channel
        indexConst = self.emitCompute(index)
        ioref = IOReference(channel, indexConst)
        return self.emitReference(ioref)
