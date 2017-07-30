from retroasm.codeblock import Load, Store
from retroasm.codeblock_builder import (
    GlobalCodeBlockBuilder, SemanticsCodeBlockBuilder
    )
from retroasm.expression import Expression, IntLiteral
from retroasm.expression_simplifier import simplifyExpression
from retroasm.namespace import Namespace, GlobalNamespace
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
        retBits = code.retBits
        self.assertIsInstance(retBits, FixedValue)
        return retBits.expr, retBits.width

    def assertRetVal(self, code, value):
        expr, width = self.getRetVal(code)
        self.assertIsInstance(expr, IntLiteral)
        mask = -1 if width is unlimited else ((1 << width) - 1)
        self.assertEqual(expr.value & mask, value)

class TestCodeBlockBuilder(SemanticsCodeBlockBuilder):

    def __init__(self, globalBuilder=None):
        if globalBuilder is None:
            globalBuilder = GlobalCodeBlockBuilder(GlobalNamespace())
        self.globalBuilder = globalBuilder
        SemanticsCodeBlockBuilder.__init__(self, globalBuilder.namespace)

    def emitLoad(self, ref):
        return ref.emitLoad(self, None)

    def emitStore(self, ref, expr):
        ref.emitStore(self, expr, None)

    def addRegister(self, name, typ=IntType.u(8)):
        try:
            ref = self.namespace[name]
        except KeyError:
            # Insert register into global namespace.
            self.globalBuilder.namespace.addVariable(name, typ, None)
            ref = self.namespace[name]

        # Check that existing global namespace entry is this register.
        globalRef = self.globalBuilder.namespace[name]
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
            channel = self.globalBuilder.namespace[channelName]
        except KeyError:
            # Insert channel into global namespace.
            channel = IOChannel(channelName, elemType, addrType)
            self.globalBuilder.namespace.define(channelName, channel, None)
        else:
            # Check that existing global namespace entry is this channel.
            assert isinstance(channel, IOChannel), channel
            assert channel.elemType is elemType, channel
            assert channel.addrType is addrType, channel
        # Import channel from global namespace into local namespace.
        localChannel = self.namespace[channelName]
        assert localChannel is channel
        # Create I/O storage.
        return self.emitIOReference(localChannel, index)

    def addValueArgument(self, name, typ=IntType.u(8)):
        return self.namespace.addValueArgument(self, name, typ, None)

    def addReferenceArgument(self, name, typ=IntType.u(8)):
        return self.namespace.addReferenceArgument(name, typ, None)

    def addVariable(self, name, typ=IntType.u(8)):
        return self.namespace.addVariable(name, typ, None)

    def addRetReference(self, value):
        return self.namespace.define('ret', value, None)
