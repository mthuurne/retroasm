from .codeblock import ConstantValue, Load, Store
from .expression import (
    AddOperator, AndOperator, Complement, Expression, IntLiteral, OrOperator,
    RShift, Truncation, XorOperator, concatenate
    )
from .expression_parser import (
    AssignmentNode, DefinitionNode, DefinitionKind, IdentifierNode, NumberNode,
    Operator, OperatorNode
    )
from .function import Function
from .linereader import getText
from .storage import (
    Concatenation, IOChannel, ReferencedValue, Variable, checkStorage,
    decomposeConcat
    )
from .types import IntType, Reference, parseTypeDecl, unlimited

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
        expr = buildExpression(value, builder)
        declWidth = typ.width
        if expr.width > declWidth:
            expr = Truncation(expr, declWidth)
    elif kind is DefinitionKind.reference:
        try:
            expr = buildStorage(value, builder)
        except BadExpression as ex:
            raise BadExpression(
                'bad value for reference "%s %s": %s' % (typ, name, ex),
                ex.location
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
            const = builder.emitCompute(expr)
            builder.context[name] = const
            return const
        elif kind is DefinitionKind.reference:
            builder.context[name] = expr
            return expr
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

def _convertIdentifier(node, builder):
    name = node.name
    try:
        value = builder.context[name]
    except KeyError:
        raise BadExpression('unknown name "%s"' % name, node.location)
    if isinstance(value, Function):
        raise BadExpression('function "%s" is not called' % name, node.location)
    else:
        return value

def _constifyIdentifier(node, builder):
    ident = _convertIdentifier(node, builder)
    if isinstance(ident, IOChannel):
        return ident

    def constify(expr):
        if isinstance(expr, ReferencedValue):
            rid = expr.rid
            ref = builder.references[rid]
            if isinstance(ref, Variable) and ref.name == 'ret':
                raise BadExpression(
                    'function return value "ret" is write-only',
                    node.location
                    )
            return builder.emitLoad(rid)
        return None

    if isinstance(ident, Concatenation):
        return concatenate(*(expr.substitute(constify) for expr in ident.exprs))
    else:
        return ident.substitute(constify)

def _convertFunctionCall(nameNode, *argNodes, builder):
    # Get function object.
    assert isinstance(nameNode, IdentifierNode), nameNode
    funcName = nameNode.name
    try:
        func = builder.context[funcName]
    except KeyError:
        raise BadExpression(
            'no function named "%s"' % funcName,
            nameNode.location
            )
    if not isinstance(func, Function):
        raise BadExpression(
            '"%s" is not a function' % funcName,
            nameNode.location
            )

    # Fill argument map.
    if len(argNodes) != len(func.args):
        raise BadExpression(
            'argument count mismatch: %s takes %d argument(s), '
            'while call provides %d argument(s)'
            % (funcName, len(func.args), len(argNodes))
            )
    argMap = {}
    for (name, decl), argNode in zip(func.args.items(), argNodes):
        if isinstance(decl, IntType):
            value = buildExpression(argNode, builder)
            argMap[name] = builder.emitCompute(value)
        elif isinstance(decl, Reference):
            try:
                value = buildStorage(argNode, builder)
            except BadExpression as ex:
                raise BadExpression(
                    'bad value for reference argument "%s %s": %s'
                    % (decl, name, ex),
                    ex.location
                    )
            if value.type is not decl.type:
                raise BadExpression.withText(
                    'storage of type "%s" passed for reference argument "%s %s"'
                    % (value.type, name, decl),
                    argNode.treeLocation
                    )
            else:
                argMap[name] = value
        else:
            assert False, decl

    # Inline function call.
    code = func.code
    if code is None:
        # Missing body, probably because of earlier errors.
        return IntLiteral.create(0)
    else:
        return builder.inlineBlock(code, argMap)

def _convertConcat(factory, node, builder):
    exprNode1, exprNode2 = node.operands
    expr1 = factory(exprNode1, builder)
    expr2 = factory(exprNode2, builder)
    if expr2.width is unlimited:
        node = exprNode2
        while isinstance(node, OperatorNode) and \
                node.operator is Operator.concatenation:
            node = node.operands[0]
        raise BadExpression.withText(
            'only the first concatenation operand is allowed to have '
            'unlimited width',
            node.treeLocation
            )
    if isinstance(expr1, Concatenation):
        yield from expr1.exprs
    else:
        yield expr1
    if isinstance(expr2, Concatenation):
        yield from expr2.exprs
    else:
        yield expr2

def _convertLookup(exprNode, indexNode, builder):
    index = buildExpression(indexNode, builder)
    if isinstance(exprNode, IdentifierNode):
        expr = _constifyIdentifier(exprNode, builder)
        if isinstance(expr, IOChannel):
            return builder.emitLoad(builder.emitIOReference(expr, index))
    else:
        expr = buildExpression(exprNode, builder)
    try:
        indexInt = index.simplify().value
    except AttributeError:
        raise BadExpression.withText(
            'bit index is not constant',
            indexNode.treeLocation
            )
    return Truncation(RShift(expr, indexInt), 1)

def _convertSlice(location, exprNode, startNode, endNode, builder):
    expr = buildExpression(exprNode, builder)

    if startNode is None:
        index = 0
    else:
        start = buildExpression(startNode, builder)
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
        end = buildExpression(endNode, builder)
        end = end.simplify()
        try:
            width = end.value - index
        except AttributeError:
            raise BadExpression.withText(
                'end index is not constant',
                endNode.treeLocation
                )

    try:
        return Truncation(RShift(expr, index), width)
    except ValueError as ex:
        raise BadExpression('invalid slice: %s' % ex, location)

def _convertOperator(node, builder):
    operator = node.operator
    if operator is Operator.call:
        return _convertFunctionCall(*node.operands, builder=builder)
    elif operator is Operator.lookup:
        return _convertLookup(*node.operands, builder=builder)
    elif operator is Operator.slice:
        return _convertSlice(node.location, *node.operands, builder=builder)
    elif operator is Operator.concatenation:
        return concatenate(*_convertConcat(buildExpression, node, builder))

    exprs = tuple(buildExpression(node, builder) for node in node.operands)
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
    else:
        assert False, operator

def buildExpression(node, builder):
    if isinstance(node, NumberNode):
        return IntLiteral(node.value, IntType(node.width))
    elif isinstance(node, IdentifierNode):
        expr = _constifyIdentifier(node, builder)
        if isinstance(expr, IOChannel):
            raise BadExpression(
                'I/O channel "%s" can only be used for lookup' % node.name,
                node.location
                )
        return expr
    elif isinstance(node, OperatorNode):
        return _convertOperator(node, builder)
    elif isinstance(node, DefinitionNode):
        raise BadExpression(
            '%s definition not allowed here' % node.kind.name,
            node.treeLocation
            )
    else:
        assert False, node

def _convertStorageLookup(node, builder):
    exprNode, indexNode = node.operands
    if isinstance(exprNode, IdentifierNode):
        ident = _convertIdentifier(exprNode, builder)
        if isinstance(ident, IOChannel):
            channel = ident
            index = buildExpression(indexNode, builder)
            rid = builder.emitIOReference(channel, index)
            return ReferencedValue(rid, channel.elemType)
    raise BadExpression(
        'slicing on the storage side is not supported yet',
        node.location
        )

def _convertStorageOperator(node, builder):
    operator = node.operator
    if operator is Operator.call:
        raise BadExpression(
            'function calls on the storage side are not supported yet',
            node.treeLocation
            )
    elif operator is Operator.lookup:
        return _convertStorageLookup(node, builder)
    elif operator is Operator.slice:
        raise BadExpression(
            'slicing on the storage side is not supported yet',
            node.location
            )
    elif operator is Operator.concatenation:
        return Concatenation(_convertConcat(buildStorage, node, builder))
    else:
        raise BadExpression.withText(
            'expected storage, found operator (%s)' % operator.name,
            node.treeLocation
            )

def buildStorage(node, builder):
    if isinstance(node, NumberNode):
        return IntLiteral(node.value, IntType(node.width))
    elif isinstance(node, DefinitionNode):
        if node.kind is DefinitionKind.constant:
            raise BadExpression(
                'definition of constant "%s" where storage is required'
                % node.name.name,
                node.treeLocation
                )
        return convertDefinition(node, builder)
    elif isinstance(node, IdentifierNode):
        expr = _convertIdentifier(node, builder)
        if checkStorage(expr):
            return expr
        elif isinstance(expr, IOChannel):
            raise BadExpression(
                'I/O channel "%s" can only be used for lookup' % node.name,
                node.location
                )
        elif isinstance(expr, ConstantValue):
            raise BadExpression(
                'use of constant "%s" where storage is required'
                % node.name,
                node.location
                )
        else:
            assert False, expr
    elif isinstance(node, OperatorNode):
        return _convertStorageOperator(node, builder)
    else:
        assert False, node

def emitCodeFromStatements(reader, builder, statements):
    '''Emits a code block from the given statements.
    '''
    for node in statements:
        numNodesBefore = len(builder.nodes)

        if isinstance(node, AssignmentNode):
            try:
                lhs = buildStorage(node.lhs, builder)
            except BadExpression as ex:
                reader.error(
                    'bad expression on left hand side of assignment: %s', ex,
                    location=ex.location
                    )
                continue

            try:
                rhs = buildExpression(node.rhs, builder)
            except BadExpression as ex:
                reader.error(
                    'bad expression on right hand side of assignment: %s', ex,
                    location=ex.location
                    )
                continue
            else:
                rhsConst = builder.emitCompute(rhs)

            try:
                for storage, offset in decomposeConcat(lhs):
                    if isinstance(storage, ReferencedValue):
                        rid = storage.rid
                    elif isinstance(storage, IOReference):
                        rid = builder._emitReference(storage)
                    else:
                        assert False, storage
                    sliced = builder.emitCompute(
                        Truncation(RShift(rhsConst, offset), storage.width)
                        )
                    builder.emitStore(rid, sliced)
            except ValueError as ex:
                reader.error('error on left hand side of assignment: %s', ex)
                continue

        elif isinstance(node, DefinitionNode):
            # Constant/reference/variable definition.
            try:
                convertDefinition(node, builder)
            except BadExpression as ex:
                reader.error(str(ex), location=ex.location)
            # Don't evaluate the expression, since that could emit loads.
            continue

        else:
            # Evaluate statement for its side effects.
            try:
                buildExpression(node, builder)
            except BadExpression as ex:
                reader.error(
                    'bad expression in statement: %s', ex,
                    location=ex.location
                    )
                continue

        stateChanged = False
        for execNode in builder.nodes[numNodesBefore:]:
            if isinstance(execNode, Load):
                storage = builder.references[execNode.rid]
                stateChanged |= storage.canLoadHaveSideEffect()
            elif isinstance(execNode, Store):
                stateChanged = True
        if not stateChanged:
            reader.warning('statement has no effect')
