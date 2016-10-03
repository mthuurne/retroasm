from .codeblock import BoundReference, Load, Store
from .context import NameExistsError
from .expression import (
    AddOperator, AndOperator, Complement, Expression, IntLiteral, Negation,
    OrOperator, XorOperator, truncate
    )
from .expression_parser import (
    AssignmentNode, DeclarationKind, DeclarationNode, DefinitionNode,
    IdentifierNode, NumberNode, Operator, OperatorNode
    )
from .expression_simplifier import simplifyExpression
from .function import Function
from .linereader import BadInput
from .storage import IOChannel
from .types import IntType, Reference, parseTypeDecl, unlimited
from .utils import Singleton

from inspect import signature

class BadExpression(BadInput):
    '''Raised when the input text cannot be parsed into an expression.
    '''

class Unit(Expression, metaclass=Singleton):
    '''Expression that represents the absense of a value.
    '''
    __slots__ = ()

    def _ctorargs(self, *exprs, **kwargs):
        return signature(self.__class__).bind()

    def __str__(self):
        return 'unit'

    def _equals(self, other):
        return self is other

    def _checkScalar(self):
        raise BadExpression(
            'attempt to use return value of function that returns nothing'
            )

unit = Unit()

def declareVariable(node, builder):
    assert node.kind is DeclarationKind.variable, node.kind

    # Determine type.
    try:
        typ = parseTypeDecl(node.type.name)
    except ValueError as ex:
        raise BadExpression(
            'bad type name in definition: %s' % ex,
            node.type.location
            )

    # Get name.
    nameNode = node.name
    name = nameNode.name

    # Add declaration to context.
    try:
        return builder.emitVariable(name, typ, nameNode.location)
    except NameExistsError as ex:
        raise BadExpression(
            'failed to declare variable "%s %s": %s' % (typ, name, ex),
            ex.location
            )

def convertDefinition(kind, nameNode, typ, value, builder):
    # Get name.
    name = nameNode.name

    # Build and validate value expression.
    if kind is DeclarationKind.constant:
        try:
            expr = buildExpression(value, builder)
        except BadExpression as ex:
            raise BadExpression(
                'bad value for constant "%s %s": %s' % (typ, name, ex),
                ex.location
                )
        declWidth = typ.width
        ref = builder.emitFixedValue(truncate(expr, declWidth), typ)
    elif kind is DeclarationKind.reference:
        try:
            ref = buildStorage(value, builder)
        except BadExpression as ex:
            raise BadExpression(
                'bad value for reference "%s %s": %s' % (typ, name, ex),
                ex.location
                )
        if typ.type.width != ref.width:
            raise BadExpression.withText(
                '%d-bit value does not match declared type "%s"'
                % (ref.width, typ.type),
                value.treeLocation
                )
    else:
        assert False, kind

    # Add definition to context.
    try:
        return builder.defineReference(name, ref, nameNode.location)
    except NameExistsError as ex:
        raise BadExpression(
            'failed to define %s "%s %s": %s' % (kind.name, typ, name, ex),
            ex.location
            )

def _convertIdentifier(node, builder):
    '''Looks up an identifier in the builder's context.
    Returns either an IOChannel or a BoundReference.
    '''
    name = node.name
    try:
        value = builder.context[name]
    except KeyError:
        raise BadExpression('unknown name "%s"' % name, node.location)
    if isinstance(value, Function):
        raise BadExpression('function "%s" is not called' % name, node.location)
    elif isinstance(value, (BoundReference, IOChannel)):
        return value
    else:
        assert False, repr(value)

def _convertFunctionCall(callNode, builder):
    nameNode, *argNodes = callNode.operands

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
            'argument count mismatch: function "%s" takes %d argument(s), '
            'while call provides %d argument(s)'
            % (funcName, len(func.args), len(argNodes)),
            callNode.location
            )
    argMap = {}
    for (name, decl), argNode in zip(func.args.items(), argNodes):
        if isinstance(decl, IntType):
            value = buildExpression(argNode, builder)
        elif isinstance(decl, Reference):
            try:
                value = buildStorage(argNode, builder)
            except BadExpression as ex:
                raise BadExpression(
                    'bad value for reference argument "%s %s": %s'
                    % (decl, name, ex),
                    ex.location
                    )
            if value.width != decl.type.width:
                raise BadExpression.withText(
                    'storage of %d bits wide passed for reference argument '
                    '"%s %s"' % (value.width, decl, name),
                    argNode.treeLocation
                    )
        else:
            assert False, decl
        argMap[name] = value

    # Inline function call.
    return builder.inlineFunctionCall(func, argMap, callNode.treeLocation)

def _convertArithmetic(node, builder):
    operator = node.operator
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
    elif operator is Operator.negation:
        return Negation(*exprs)
    else:
        assert False, operator

def _convertExpressionOperator(node, builder):
    operator = node.operator
    if operator is Operator.call:
        retRef = _convertFunctionCall(node, builder)
        if retRef is None:
            return unit
        else:
            return builder.emitLoad(retRef, node.treeLocation)
    elif operator is Operator.lookup:
        return builder.emitLoad(
            _convertStorageLookup(node, builder), node.treeLocation
            )
    elif operator is Operator.slice:
        return builder.emitLoad(
            _convertStorageSlice(node, builder), node.treeLocation
            )
    elif operator is Operator.concatenation:
        return builder.emitLoad(
            _convertStorageConcat(node, builder), node.treeLocation
            )
    else:
        return _convertArithmetic(node, builder)

def buildExpression(node, builder):
    if isinstance(node, NumberNode):
        return IntLiteral(node.value)
    elif isinstance(node, IdentifierNode):
        ident = _convertIdentifier(node, builder)
        if isinstance(ident, IOChannel):
            raise BadExpression(
                'I/O channel "%s" can only be used for lookup' % node.name,
                node.location
                )
        else:
            return builder.emitLoad(ident, node.location)
    elif isinstance(node, OperatorNode):
        return _convertExpressionOperator(node, builder)
    elif isinstance(node, DeclarationNode):
        raise BadExpression(
            'variable declaration is not allowed here',
            node.treeLocation
            )
    elif isinstance(node, DefinitionNode):
        raise BadExpression(
            'definition must be only statement on a line',
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
            return builder.emitIOReference(channel, index)

    storage = buildStorage(exprNode, builder)
    index = buildExpression(indexNode, builder)
    try:
        indexInt = simplifyExpression(index).value
    except AttributeError:
        raise BadExpression.withText(
            'bit index is not constant',
            indexNode.treeLocation
            )
    try:
        return storage.slice(indexInt, 1)
    except ValueError as ex:
        raise BadExpression('invalid lookup: %s' % ex, node.location)

def _convertStorageSlice(node, builder):
    exprNode, startNode, endNode = node.operands
    storage = buildStorage(exprNode, builder)

    if startNode is None:
        index = 0
    else:
        start = buildExpression(startNode, builder)
        start = simplifyExpression(start)
        try:
            index = start.value
        except AttributeError:
            raise BadExpression.withText(
                'start index is not constant',
                startNode.treeLocation
                )

    if endNode is None:
        width = storage.width
    else:
        end = buildExpression(endNode, builder)
        end = simplifyExpression(end)
        try:
            width = end.value - index
        except AttributeError:
            raise BadExpression.withText(
                'end index is not constant',
                endNode.treeLocation
                )

    try:
        return storage.slice(index, width)
    except ValueError as ex:
        raise BadExpression('invalid slice: %s' % ex, node.location)

def _convertStorageConcat(node, builder):
    exprNode1, exprNode2 = node.operands
    expr1 = buildStorage(exprNode1, builder)
    expr2 = buildStorage(exprNode2, builder)
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
    return expr2.concat(expr1)

def _convertStorageOperator(node, builder):
    operator = node.operator
    if operator is Operator.call:
        retRef = _convertFunctionCall(node, builder)
        if retRef is None:
            raise BadExpression(
                'function does not return anything; expected reference',
                node.treeLocation
                )
        else:
            return retRef
    elif operator is Operator.lookup:
        return _convertStorageLookup(node, builder)
    elif operator is Operator.slice:
        return _convertStorageSlice(node, builder)
    elif operator is Operator.concatenation:
        return _convertStorageConcat(node, builder)
    else:
        expr = _convertArithmetic(node, builder)
        typ = IntType.u(1) if operator is Operator.negation else IntType.int
        return builder.emitFixedValue(expr, typ)

def buildStorage(node, builder):
    if isinstance(node, NumberNode):
        literal = IntLiteral(node.value)
        typ = IntType(node.width, node.width is unlimited)
        return builder.emitFixedValue(literal, typ)
    elif isinstance(node, DeclarationNode):
        return declareVariable(node, builder)
    elif isinstance(node, DefinitionNode):
        raise BadExpression(
            'definition must be only statement on a line',
            node.treeLocation
            )
    elif isinstance(node, IdentifierNode):
        ident = _convertIdentifier(node, builder)
        if isinstance(ident, IOChannel):
            raise BadExpression(
                'I/O channel "%s" can only be used for lookup' % node.name,
                node.location
                )
        else:
            return ident
    elif isinstance(node, OperatorNode):
        return _convertStorageOperator(node, builder)
    else:
        assert False, node

def emitCodeFromStatements(reader, builder, statements, retType):
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
                rhs = builder.emitCompute(buildExpression(node.rhs, builder))
            except BadExpression as ex:
                reader.error(
                    'bad expression on right hand side of assignment: %s', ex,
                    location=ex.location
                    )
                continue

            builder.emitStore(lhs, rhs, node.lhs.treeLocation)

        elif isinstance(node, DefinitionNode):
            # Constant/reference definition.
            decl = node.decl
            kind = decl.kind
            nameNode = decl.name
            typeNode = decl.type
            if typeNode is None:
                # For a function returning a reference, the reference type
                # is declared in the function header rather than on the
                # definition line.
                assert kind == DeclarationKind.reference, kind
                assert nameNode.name == 'ret', nameNode.name
                if not isinstance(retType, Reference):
                    reader.error(
                        '"ret" defined as reference in function that returns %s'
                        % ('nothing' if retType is None else 'value'),
                        location=decl.location
                        )
                    continue
                typ = retType
            else:
                # Determine type.
                try:
                    typ = parseTypeDecl(typeNode.name)
                except ValueError as ex:
                    raise BadExpression(
                        'bad type name in definition: %s' % ex,
                        typeNode.location
                        )
            try:
                convertDefinition(kind, nameNode, typ, node.value, builder)
            except BadExpression as ex:
                reader.error(str(ex), location=ex.location)
            # Don't evaluate the expression, since that could emit loads.
            continue

        elif isinstance(node, DeclarationNode):
            # Variable declaration.
            try:
                declareVariable(node, builder)
            except BadExpression as ex:
                reader.error(str(ex), location=ex.location)
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
                storage = builder.storages[execNode.sid]
                stateChanged |= storage.canLoadHaveSideEffect()
            elif isinstance(execNode, Store):
                stateChanged = True
        if not stateChanged:
            reader.warning('statement has no effect')
