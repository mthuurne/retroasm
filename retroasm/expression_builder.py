from .expression import (
    AddOperator, AndOperator, Complement, Concatenation, Expression, IntLiteral,
    OrOperator, Slice, Truncation, XorOperator
    )
from .expression_parser import (
    DefinitionNode, DefinitionKind, IdentifierNode, NumberNode, Operator,
    OperatorNode
    )
from .function import Function, FunctionCall
from .linereader import getText
from .storage import IOChannel, IOReference, ReferencedValue, checkStorage
from .types import IntType, parseTypeDecl, unlimited

class BadExpression(Exception):
    '''Raised when the input text cannot be parsed into an expression.
    The 'location' attribute contains the location in the instruction set
    definition file that triggered this exception, as a metadata dictionary
    that can be used with LineReader, or None if this information is not
    available.
    '''

    @classmethod
    def withText(cls, msg, location):
        return cls('%s: %s' % (msg, getText(location)), location)

    def __init__(self, msg, location=None):
        Exception.__init__(self, msg)
        self.location = location

def convertDefinition(node, builder):
    # Determine type.
    try:
        typ = parseTypeDecl(node.decl.name)
    except ValueError as ex:
        raise BadExpression(
            'bad type name in definition: %s' % ex,
            node.decl.location
            )

    # Get name.
    nameNode = node.name
    name = nameNode.name

    # Build and validate value expression.
    kind = node.kind
    value = node.value
    if kind is DefinitionKind.constant:
        expr = createExpression(value, builder.context)
        declWidth = typ.width
        if expr.width > declWidth:
            expr = Truncation(expr, declWidth)
    elif kind is DefinitionKind.reference:
        expr = _createStorage(value, builder)
        if not checkStorage(expr):
            raise BadExpression(
                'value for reference "%s" is not a storage' % name,
                value.treeLocation
                )
        if typ.type is not expr.type:
            raise BadExpression(
                'declared type "%s" does not match the value\'s type "%s"'
                % (typ.type, expr.type),
                node.decl.location
                )
    elif kind is DefinitionKind.variable:
        assert value is None, value
    else:
        assert False, kind

    # Add definition to context.
    try:
        if kind is DefinitionKind.constant:
            value = expr.substitute(builder.constifyReferences)
            const = builder.emitCompute(value)
            builder.context[name] = const
            return const
        elif kind is DefinitionKind.reference:
            ref = expr.substitute(builder.constifyIOIndices)
            builder.context[name] = ref
            return ref
        elif kind is DefinitionKind.variable:
            rid = builder.emitVariable(name, typ)
            return ReferencedValue(rid, typ)
        else:
            assert False, kind
    except ValueError as ex:
        raise BadExpression(
            'failed to define %s "%s %s": %s' % (kind.name, typ, name, ex),
            nameNode.location
            )

def _convertIdentifier(node, context):
    name = node.name
    try:
        value = context[name]
    except KeyError:
        raise BadExpression('unknown name "%s"' % name, node.location)
    if isinstance(value, Function):
        raise BadExpression(
            'function "%s" is not called' % name, node.location
            )
    else:
        return value

def _convertFunctionCall(nameNode, *argNodes, context):
    assert isinstance(nameNode, IdentifierNode), nameNode
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
    expr = _createTop(exprNode, context)
    index = createExpression(indexNode, context)
    if isinstance(expr, IOChannel):
        return IOReference(expr, index)
    else:
        assert isinstance(expr, Expression), expr
        try:
            indexInt = index.simplify().value
        except AttributeError:
            raise BadExpression.withText(
                'bit index is not constant',
                indexNode.treeLocation
                )
        return Slice(expr, indexInt, 1)

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
            raise BadExpression.withText(
                'start index is not constant',
                startNode.treeLocation
                )

    if endNode is None:
        width = expr.width
        if width is unlimited:
            raise BadExpression.withText(
                'omitting the end index not allowed when slicing '
                'an unlimited width expression',
                location
                )
    else:
        end = createExpression(endNode, context)
        end = end.simplify()
        try:
            width = end.value - index
        except AttributeError:
            raise BadExpression.withText(
                'end index is not constant',
                endNode.treeLocation
                )

    try:
        return Slice(expr, index, width)
    except ValueError as ex:
        raise BadExpression('invalid slice: %s' % ex, location)

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

def _createTop(node, context):
    if isinstance(node, NumberNode):
        return IntLiteral(node.value, IntType(node.width))
    elif isinstance(node, IdentifierNode):
        return _convertIdentifier(node, context)
    elif isinstance(node, OperatorNode):
        return _convertOperator(node, context)
    elif isinstance(node, DefinitionNode):
        raise BadExpression(
            '%s definition not allowed here' % node.kind.name,
            node.treeLocation
            )
    else:
        assert False, node

def createExpression(node, context):
    expr = _createTop(node, context)
    if isinstance(expr, IOChannel):
        assert isinstance(node, IdentifierNode), node
        raise BadExpression(
            'I/O channel "%s" can only be used for lookup' % node.name,
            node.location
            )
    else:
        assert isinstance(expr, Expression), expr
        return expr

def buildExpression(node, builder):
    expr = createExpression(node, builder.context)
    # Substitute LoadedConstants for all references, such that we have
    # a side-effect free version of the right hand side expression.
    return expr.substitute(builder.constifyReferences)

def _convertStorageOperator(node, builder):
    operator = node.operator
    if operator is Operator.call:
        return _convertFunctionCall(*node.operands, context=builder.context)
    elif operator is Operator.lookup:
        return _convertLookup(*node.operands, context=builder.context)
    elif operator is Operator.slice:
        return _convertSlice(
            node.location, *node.operands, context=builder.context
            )
    elif operator is Operator.concatenation:
        return Concatenation(*(
            _createStorage(node, builder)
            for node in node.operands
            ))
    else:
        raise BadExpression.withText(
            'operator (%s) is not allowed here' % operator.name,
            node.location
            )

def _createStorage(node, builder):
    if isinstance(node, NumberNode):
        return IntLiteral(node.value, IntType(node.width))
    elif isinstance(node, DefinitionNode):
        return convertDefinition(node, builder)
    elif isinstance(node, IdentifierNode):
        expr = _convertIdentifier(node, builder.context)
        if isinstance(expr, IOChannel):
            raise BadExpression(
                'I/O channel "%s" can only be used for lookup' % node.name,
                node.location
                )
        else:
            assert isinstance(expr, Expression), expr
            return expr
    elif isinstance(node, OperatorNode):
        return _convertStorageOperator(node, builder)
    else:
        assert False, node

def buildStorage(node, builder):
    expr = _createStorage(node, builder)
    # Constify the I/O indices to force emission of all loads before
    # we emit any stores.
    return expr.substitute(builder.constifyIOIndices)
