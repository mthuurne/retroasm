from retroasm.expression import (
    AddOperator, AndOperator, IntLiteral, IntType, Register
    )
from retroasm.func_parser import CodeBlockBuilder, Load, Store

import unittest

verbose = False

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

    def createSimplifiedCode(self):
        code = self.builder.createCodeBlock()
        if verbose:
            print('=' * 40)
            code.dump()
        code.verify()
        code.simplify()
        if verbose:
            print('-' * 40)
            code.dump()
        code.verify()
        return code

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
        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

    def test_unused_load(self):
        '''Test whether unused loads are removed.'''
        ridA = self.builder.addRegister('a')
        loadA = self.builder.emitLoad(ridA)
        andA = self.builder.emitCompute(
            AndOperator(loadA, IntLiteral.create(0))
            )
        storeA = self.builder.emitStore(ridA, andA)

        correct = (
            Store(andA.cid, ridA),
            )

        code = self.createSimplifiedCode()
        self.assertNodes(code.nodes, correct)

if __name__ == '__main__':
    verbose = True
    unittest.main()
