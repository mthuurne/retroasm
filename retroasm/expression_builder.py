from .codeblock import Assignment
from .expression import (
    AddOperator, AndOperator, Complement, Concatenation, IntLiteral,
    OrOperator, Slice, XorOperator
    )
from .expression_parser import (
    AssignmentNode, IdentifierNode, NumberNode, Operator, OperatorNode,
    ParseError, parseType
    )
from .function import Function, FunctionCall
from .linereader import getSpan
from .storage import IOChannel, IOReference
from .types import IntType, unlimited

def _convertIdentifier(node, context):
    decl = node.decl
    name = node.name
    if decl is None:
        # Look up identifier in context.
        try:
            return context[name]
        except KeyError:
            raise ParseError(
                'unknown name "%s"' % name,
                getSpan(node.location)
                )
    else:
        # Variable declaration.
        typ = parseType(decl)
        try:
            return context.addVariable(name, typ)
        except AttributeError:
            raise ParseError(
                'attempt to declare variable "%s %s" in a context that does '
                'not support variable declarations' % (decl, name),
                getSpan(node.location)
                )

def _convertFunctionCall(nameNode, *argNodes, context):
    assert isinstance(nameNode, IdentifierNode), nameNode
    assert nameNode.decl is None, nameNode
    name = nameNode.name

    try:
        func = context[nameNode.name]
    except KeyError:
        raise ParseError(
            'no function named "%s"' % name,
            getSpan(nameNode.location)
            )
    if not isinstance(func, Function):
        raise ParseError(
            '"%s" is not a function' % name,
            getSpan(nameNode.location)
            )

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
            raise ParseError(
                'start index is not constant: %s' % start,
                getSpan(startNode.location)
                )

    if endNode is None:
        width = expr.width
        if width is unlimited:
            raise ParseError(
                'omitting the end index not allowed when slicing '
                'an unlimited width expression: %s' % expr,
                getSpan(location)
                )
    else:
        end = createExpression(endNode, context)
        end = end.simplify()
        try:
            width = end.value - index
        except AttributeError:
            raise ParseError(
                'end index is not constant: %s' % end,
                getSpan(endNode.location)
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
