from .codeblock import Assignment
from .expression import (
    AddOperator, AndOperator, Complement, Concatenation, IntLiteral,
    OrOperator, Slice, XorOperator
    )
from .expression_parser import (
    AssignmentNode, IdentifierNode, NumberNode, Operator, OperatorNode,
    parseType
    )
from .function import Function, FunctionCall
from .storage import IOChannel, IOReference
from .types import IntType, unlimited

class BadExpression(Exception):
    '''Raised when the input text cannot be parsed into an expression.
    The 'location' attribute contains the location in the instruction set
    definition file that triggered this exception, as a metadata dictionary
    that can be used with LineReader, or None if this information is not
    available.
    '''

    def __init__(self, msg, location=None):
        Exception.__init__(self, msg)
        self.location = location

def _convertIdentifier(node, context):
    decl = node.decl
    name = node.name
    if decl is None:
        # Look up identifier in context.
        try:
            return context[name]
        except KeyError:
            raise BadExpression('unknown name "%s"' % name, node.location)
    else:
        # Variable declaration.
        typ = parseType(decl)
        try:
            return context.addVariable(name, typ)
        except AttributeError:
            raise BadExpression(
                'attempt to declare variable "%s %s" in a context that does '
                'not support variable declarations' % (decl, name),
                node.location
                )

def _convertFunctionCall(nameNode, *argNodes, context):
    assert isinstance(nameNode, IdentifierNode), nameNode
    assert nameNode.decl is None, nameNode
    name = nameNode.name

    try:
        func = context[nameNode.name]
    except KeyError:
        raise BadExpression('no function named "%s"' % name, nameNode.location)
    if not isinstance(func, Function):
        raise BadExpression('"%s" is not a function' % name, nameNode.location)

    args = tuple(createExpression(node, context) for node in argNodes)
    return FunctionCall(func, args)

def _convertLookup(exprNode, indexNode, context):
    expr = createExpression(exprNode, context)
    index = createExpression(indexNode, context)
    if isinstance(expr, IOChannel):
        return IOReference(expr, index)
    else:
        return Slice(expr, index, 1)

def _convertSlice(location, exprNode, startNode, endNode, context):
    expr = createExpression(exprNode, context)

    if startNode is None:
        index = 0
    else:
        start = createExpression(startNode, context)
        start = start.simplify()
        try:
            index = start.value
        except AttributeError:
            raise BadExpression(
                'start index is not constant: %s' % start,
                startNode.location
                )

    if endNode is None:
        width = expr.width
        if width is unlimited:
            raise BadExpression(
                'omitting the end index not allowed when slicing '
                'an unlimited width expression: %s' % expr,
                location
                )
    else:
        end = createExpression(endNode, context)
        end = end.simplify()
        try:
            width = end.value - index
        except AttributeError:
            raise BadExpression(
                'end index is not constant: %s' % end,
                endNode.location
                )

    return Slice(expr, index, width)

def _convertOperator(node, context):
    operator = node.operator
    if operator is Operator.call:
        return _convertFunctionCall(*node.operands, context=context)
    elif operator is Operator.lookup:
        return _convertLookup(*node.operands, context=context)
    elif operator is Operator.slice:
        return _convertSlice(node.location, *node.operands, context=context)

    exprs = tuple(createExpression(node, context) for node in node.operands)
    if operator is Operator.bitwise_and:
        return AndOperator(*exprs)
    elif operator is Operator.bitwise_or:
        return OrOperator(*exprs)
    elif operator is Operator.bitwise_xor:
        return XorOperator(*exprs)
    elif operator is Operator.add:
        return AddOperator(*exprs)
    elif operator is Operator.sub:
        expr1, expr2 = exprs
        return AddOperator(expr1, Complement(expr2))
    elif operator is Operator.complement:
        return Complement(*exprs)
    elif operator is Operator.concatenation:
        return Concatenation(*exprs)
    else:
        assert False, operator

def createExpression(node, context):
    if isinstance(node, NumberNode):
        return IntLiteral(node.value, IntType(node.width))
    elif isinstance(node, IdentifierNode):
        return _convertIdentifier(node, context)
    elif isinstance(node, OperatorNode):
        return _convertOperator(node, context)
    else:
        assert False, node

def createStatement(tree, context):
    if isinstance(tree, AssignmentNode):
        lhs = createExpression(tree.lhs, context)
        rhs = createExpression(tree.rhs, context)
        return Assignment(lhs, rhs)
    else:
        return createExpression(tree, context)
