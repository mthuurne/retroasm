from retroasm.codeblock import (
    ComputedConstant, ConstantValue, SingleReference, Store
    )
from retroasm.codeblock_builder import (
    GlobalCodeBlockBuilder, LocalCodeBlockBuilder
    )
from retroasm.expression import Expression, IntLiteral
from retroasm.expression_simplifier import simplifyExpression
from retroasm.namespace import Namespace, GlobalNamespace
from retroasm.storage import IOChannel, Variable
from retroasm.types import IntType, unlimited

class NodeChecker:

    def assertNode(self, actual, correct):
        self.assertIsInstance(actual, type(correct))
        self.assertEqual(actual.expr, correct.expr)
        self.assertEqual(actual.storage, correct.storage)

    def assertNodes(self, actual, correct):
        self.assertEqual(len(actual), len(correct))
        for a, c in zip(actual, correct):
            self.assertNode(a, c)

    def assertIntLiteral(self, expr, value):
        self.assertIsInstance(expr, IntLiteral)
        self.assertEqual(expr.value, value)

    def getCid(self, expr):
        self.assertIsInstance(expr, Expression)
        simplified = simplifyExpression(expr)
        return simplified.cid

    def getRetVal(self, code):
        retRef = code.retRef
        self.assertIsNotNone(retRef)
        retStorage = retRef.storage
        retVal = None
        for node in code.nodes:
            if node.storage is retStorage:
                self.assertIsInstance(node, Store)
                self.assertIsNone(retVal)
                retVal = node.expr
        return retVal, retRef.width

    def assertRetVal(self, code, value):
        expr, width = self.getRetVal(code)
        self.assertIsInstance(expr, ConstantValue)
        constant = code.constants[expr.cid]
        self.assertIsInstance(constant, ComputedConstant)
        self.assertIsInstance(constant.expr, IntLiteral)
        mask = -1 if width is unlimited else ((1 << width) - 1)
        self.assertEqual(constant.expr.value & mask, value)

class TestCodeBlockBuilder(LocalCodeBlockBuilder):

    def __init__(self, globalBuilder=None):
        if globalBuilder is None:
            globalBuilder = GlobalCodeBlockBuilder(GlobalNamespace())
        self.globalBuilder = globalBuilder
        LocalCodeBlockBuilder.__init__(self, globalBuilder)

    def emitLoad(self, ref):
        return ref.emitLoad(None)

    def emitStore(self, ref, expr):
        ref.emitStore(expr, None)

    def addRegister(self, name, typ=IntType.u(8)):
        try:
            ref = self.namespace[name]
        except KeyError:
            # Insert register into global namespace.
            self.globalBuilder.emitVariable(name, typ, None)
            ref = self.namespace[name]

        # Check that existing global namespace entry is this register.
        globalRef = self.globalBuilder.namespace[name]
        assert isinstance(globalRef, SingleReference), globalRef
        assert typ is globalRef.type, globalRef
        reg = globalRef.storage
        assert reg.name == name, reg
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
        return self.emitValueArgument(name, typ, None)

    def addReferenceArgument(self, name, typ=IntType.u(8)):
        return self.emitReferenceArgument(name, typ, None)

    def addVariable(self, name, typ=IntType.u(8)):
        return self.emitVariable(name, typ, None)

    def addRetReference(self, value):
        return self.defineReference('ret', value, None)
