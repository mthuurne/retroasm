from retroasm.expression import AddOperator, IntLiteral, IntType, Register
from retroasm.func_parser import CodeBlockBuilder, Load, Store

import unittest

class TestCodeBlockBuilder(CodeBlockBuilder):

    def addRegister(self, name, width=8):
        reg = Register(name, IntType(width))
        return self.emitReference(reg)

class CodeBlockTests(unittest.TestCase):

    def assertNode(self, actual, correct):
        self.assertEqual(type(actual), type(correct))
        self.assertEqual(actual.cid, correct.cid)
        self.assertEqual(actual.rid, correct.rid)

    def assertNodes(self, actual, correct):
        self.assertEqual(len(actual), len(correct))
        for a, c in zip(actual, correct):
            self.assertNode(a, c)

    def setUp(self):
        self.builder = TestCodeBlockBuilder()

    def test_no_change(self):
        '''Test whether a basic sequence survives a simplification attempt.'''
        ridA = self.builder.addRegister('a')
        loadA = self.builder.emitLoad(ridA)
        incA = self.builder.emitCompute(
            AddOperator(loadA, IntLiteral.create(1))
            )
        storeA = self.builder.emitStore(ridA, incA)

        correct = (
            Load(loadA.cid, ridA),
            Store(incA.cid, ridA),
            )

        code = self.builder.createCodeBlock()
        self.assertNodes(code.nodes, correct)
        code.simplifyConstants()
        self.assertNodes(code.nodes, correct)

if __name__ == '__main__':
    unittest.main()
