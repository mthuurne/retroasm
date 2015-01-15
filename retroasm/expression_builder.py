from .expression import (
    AddOperator, AndOperator, Complement, Concatenation, IntLiteral,
    OrOperator, Slice, Truncation, XorOperator
    )
from .expression_parser import (
    DefinitionNode, IdentifierNode, NumberNode, Operator, OperatorNode
    )
from .function import Function, FunctionCall
from .storage import IOChannel, IOReference, checkStorage
from .types import IntType, Reference, parseType, parseTypeDecl, unlimited

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

def _convertDefinition(node, context):
    ident = node.identifier
    value = node.value

    # Determine type.
    try:
        typ = parseTypeDecl(ident.decl)
    except ValueError as ex:
        raise BadExpression(
            'bad type name in definition: %s' % ex,
            ident.location
            )

    # Build value expression.
    expr = createExpression(value, context)

    # Validate the definition.
    if isinstance(typ, Reference):
        if not checkStorage(expr):
            raise BadExpression(
                'value for reference "%s" is not a storage' % ident.name,
                value.treeLocation
                )
        if typ.type is not expr.type:
            raise BadExpression(
                'declared type "%s" does not match the value\'s type "%s"'
                % (typ.type, expr.type),
                ident.location
                )
    else:
        declWidth = typ.width
        if expr.width > declWidth:
            expr = Truncation(expr, declWidth)

    # Add definition to context.
    try:
        if isinstance(typ, Reference):
            what = 'reference'
            return context.addReference(ident.name, expr)
        else:
            what = 'constant'
            return context.addConstant(ident.name, expr)
    except AttributeError:
        raise BadExpression(
            'attempt to define %s "%s %s" in a context that does not support '
            '%s definitions' % (what, ident.decl, ident.name, what),
            node.treeLocation
            )
    except ValueError as ex:
        raise BadExpression(
            'failed to define %s "%s %s": %s'
            % (what, ident.decl, ident.name, ex),
            node.treeLocation
            )

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
        try:
            return context.addVariable(name, parseType(decl))
        except AttributeError:
            raise BadExpression(
                'attempt to declare variable "%s %s" in a context that does '
                'not support variable declarations' % (decl, name),
                node.location
                )
        except ValueError as ex:
            raise BadExpression(
                'failed to declare variable "%s %s": %s' % (decl, name, ex),
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
                startNode.treeLocation
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
                endNode.treeLocation
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
    elif isinstance(node, DefinitionNode):
        return _convertDefinition(node, context)
    elif isinstance(node, IdentifierNode):
        return _convertIdentifier(node, context)
    elif isinstance(node, OperatorNode):
        return _convertOperator(node, context)
    else:
        assert False, node
