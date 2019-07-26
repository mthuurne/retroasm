from retroasm.codeblock import Load, Store
from retroasm.codeblock_builder import (
    SemanticsCodeBlockBuilder, StatelessCodeBlockBuilder
    )
from retroasm.expression import Expression, IntLiteral
from retroasm.expression_simplifier import simplifyExpression
from retroasm.namespace import (
    GlobalNamespace, LocalNamespace, createIOReference
    )
from retroasm.reference import FixedValue, Reference, SingleStorage
from retroasm.storage import IOChannel, Variable
from retroasm.types import IntType, unlimited

class NodeChecker:

    def assertNodes(self, actualNodes, correctNodes):
        correctNodes = tuple(correctNodes)
        loadMap = {}
        self.assertEqual(len(actualNodes), len(correctNodes))
        for i, (actual, correct) in enumerate(zip(actualNodes, correctNodes)):
            msg = 'node %d of %d' % (i + 1, len(actualNodes))
            self.assertIsInstance(actual, type(correct), msg)
            self.assertEqual(actual.storage, correct.storage, msg)
            if isinstance(correct, Load):
                loadMap[actual.expr] = correct.expr
            elif isinstance(correct, Store):
                expr = actual.expr.substitute(loadMap.get)
                self.assertEqual(expr, correct.expr, msg)
            else:
                self.fail('unknown node type: %s' % correct.__class__.__name__)

    def assertIntLiteral(self, expr, value):
        self.assertIsInstance(expr, IntLiteral)
        self.assertEqual(expr.value, value)

    def getRetVal(self, code):
        retBits, = code.returned
        self.assertIsInstance(retBits, FixedValue)
        return retBits.expr, retBits.width

    def assertRetVal(self, code, value):
        expr, width = self.getRetVal(code)
        self.assertIsInstance(expr, IntLiteral)
        mask = -1 if width is unlimited else ((1 << width) - 1)
        self.assertEqual(expr.value & mask, value)

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

    def addValueArgument(self, name, typ=IntType.u(8), location=None):
        return super().addValueArgument(name, typ, location)

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
