from retroasm.codeblock import Load, Store
from retroasm.codeblock_builder import (
    SemanticsCodeBlockBuilder, StatelessCodeBlockBuilder
    )
from retroasm.expression import IntLiteral
from retroasm.namespace import (
    GlobalNamespace, LocalNamespace, createIOReference
    )
from retroasm.reference import FixedValue, Reference, SingleStorage
from retroasm.storage import IOChannel
from retroasm.types import IntType, unlimited


def assertNodes(actualNodes, correctNodes):
    correctNodes = tuple(correctNodes)
    loadMap = {}
    assert len(actualNodes) == len(correctNodes)
    for i, (actual, correct) in enumerate(zip(actualNodes, correctNodes)):
        msg = 'node %d of %d' % (i + 1, len(actualNodes))
        assert isinstance(actual, type(correct)), msg
        assert actual.storage == correct.storage, msg
        if isinstance(correct, Load):
            loadMap[actual.expr] = correct.expr
        elif isinstance(correct, Store):
            expr = actual.expr.substitute(loadMap.get)
            assert expr == correct.expr, msg
        else:
            raise AssertionError(
                f'unknown node type: {correct.__class__.__name__}'
                )

def getRetVal(code):
    retBits, = code.returned
    assert isinstance(retBits, FixedValue)
    return retBits.expr, retBits.width

def assertRetVal(code, value):
    expr, width = getRetVal(code)
    assert isinstance(expr, IntLiteral)
    mask = -1 if width is unlimited else ((1 << width) - 1)
    assert expr.value & mask == value

class TestNamespace(LocalNamespace):

    def __init__(self, globalNamespace=None):
        if globalNamespace is None:
            globalBuilder = StatelessCodeBlockBuilder()
            globalNamespace = GlobalNamespace(globalBuilder)
        localBuilder = SemanticsCodeBlockBuilder()
        LocalNamespace.__init__(self, globalNamespace, localBuilder)

    def emitLoad(self, ref):
        return ref.emitLoad(self.builder, None)

    def emitStore(self, ref, expr):
        ref.emitStore(self.builder, expr, None)

    def addRegister(self, name, typ=IntType.u(8)):
        try:
            ref = self[name]
        except KeyError:
            # Insert register into global namespace.
            self.parent.addVariable(name, typ, None)
            ref = self[name]

        # Check that existing global namespace entry is this register.
        globalRef = self.parent[name]
        assert isinstance(globalRef, Reference), globalRef
        assert isinstance(globalRef.bits, SingleStorage), globalRef.bits
        assert typ is globalRef.type, globalRef
        reg = globalRef.bits.storage
        assert ref.bits.storage is reg
        assert reg.width == typ.width

        return ref

    def addIOStorage(self, channelName, index,
            elemType=IntType.u(8), addrType=IntType.u(16)):
        try:
            channel = self.parent[channelName]
        except KeyError:
            # Insert channel into global namespace.
            channel = IOChannel(channelName, elemType, addrType)
            self.parent.define(channelName, channel, None)
        else:
            # Check that existing global namespace entry is this channel.
            assert isinstance(channel, IOChannel), channel
            assert channel.elemType is elemType, channel
            assert channel.addrType is addrType, channel
        # Import channel from global namespace into local namespace.
        localChannel = self[channelName]
        assert localChannel is channel
        # Create I/O storage.
        return createIOReference(localChannel, index)

    def addArgument(self, name, typ=IntType.u(8), location=None):
        return super().addArgument(name, typ, location)

    def addVariable(self, name, typ=IntType.u(8), location=None):
        return super().addVariable(name, typ, location)

    def addRetReference(self, value):
        return super().define('ret', value, None)

    def inlineBlock(self, code, argFetcher):
        return self.builder.inlineBlock(code, argFetcher)

    def inlineFunctionCall(self, func, argMap, location=None):
        return self.builder.inlineFunctionCall(func, argMap, location)
