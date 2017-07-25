from .codeblock import Load, Store
from .expression import (
    AddOperator, AndOperator, Complement, Expression, IntLiteral, LVShift,
    Negation, OrOperator, RVShift, SignTest, XorOperator, optSlice, truncate
    )
from .expression_parser import (
    AssignmentNode, BranchNode, DeclarationKind, DeclarationNode,
    DefinitionNode, EmptyNode, IdentifierNode, LabelNode, MultiMatchNode,
    NumberNode, Operator, OperatorNode
    )
from .expression_simplifier import simplifyExpression
from .function import Function
from .linereader import BadInput
from .namespace import NameExistsError
from .reference import ConcatenatedBits, FixedValue, Reference, SlicedBits
from .storage import IOChannel
from .types import IntType, ReferenceType, parseTypeDecl, unlimited
from .utils import Singleton

class BadExpression(BadInput):
    '''Raised when the input text cannot be parsed into an expression.
    '''

class UnknownNameError(BadExpression):
    '''Raised when an expression contains an identifier that does not occur
    in any of its surrounding namespaces.
    '''

    def __init__(self, name, *args, **kvargs):
        BadExpression.__init__(self, *args, **kvargs)
        self.name = name

class Unit(Expression, metaclass=Singleton):
    '''Expression that represents the absense of a value.
    '''
    __slots__ = ()

    def _ctorargs(self):
        return ()

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

    # Add declaration to namespace.
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
        bits = FixedValue(truncate(expr, declWidth), declWidth)
        ref = Reference(bits, typ)
    elif kind is DeclarationKind.reference:
        try:
            ref = buildReference(value, builder)
        except BadExpression as ex:
            raise BadExpression(
                'bad value for reference "%s %s": %s' % (typ, name, ex),
                ex.location
                )
        if typ.type.width != ref.width:
            raise BadExpression.withText(
                '%s-bit value does not match declared type "%s"'
                % (ref.width, typ.type),
                value.treeLocation
                )
    else:
        assert False, kind

    # Add definition to namespace.
    try:
        return builder.defineReference(name, ref, nameNode.location)
    except NameExistsError as ex:
        raise NameExistsError(
            'failed to define %s "%s %s": %s' % (kind.name, typ, name, ex),
            ex.location
            )

def _convertIdentifier(node, builder):
    '''Looks up an identifier in the builder's namespace.
    Returns either an IOChannel or a Reference.
    '''
    name = node.name
    try:
        value = builder.namespace[name]
    except KeyError:
        raise UnknownNameError(name, 'unknown name "%s"' % name, node.location)
    if isinstance(value, Function):
        raise BadExpression('function "%s" is not called' % name, node.location)
    elif isinstance(value, (IOChannel, Reference)):
        return value
    else:
        assert False, (name, repr(value))

def _convertFunctionCall(callNode, builder):
    nameNode, *argNodes = callNode.operands

    # Get function object.
    assert isinstance(nameNode, IdentifierNode), nameNode
    funcName = nameNode.name
    try:
        func = builder.namespace[funcName]
    except KeyError:
        raise UnknownNameError(
            funcName,
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
        value = buildReference(argNode, builder)
        # Value arguments are not truncated when passed, but are truncated by
        # the local variable that they are stored into.
        # For reference arguments, we demand the passed width to match the
        # argument width, so truncation is never required.
        if isinstance(decl, ReferenceType) and value.width != decl.type.width:
            raise BadExpression.withText(
                '%s-bit reference passed for reference argument "%s %s"'
                % (value.width, decl, name),
                argNode.treeLocation
                )
        argMap[name] = value.bits

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
    elif operator is Operator.shift_left:
        return LVShift(*exprs)
    elif operator is Operator.shift_right:
        return RVShift(*exprs)
    elif operator is Operator.add:
        return AddOperator(*exprs)
    elif operator is Operator.sub:
        expr1, expr2 = exprs
        return AddOperator(expr1, Complement(expr2))
    elif operator is Operator.complement:
        return Complement(*exprs)
    elif operator is Operator.bitwise_complement:
        return XorOperator(IntLiteral(-1), *exprs)
    elif operator is Operator.negation:
        return Negation(*exprs)
    elif operator is Operator.equal:
        return Negation(XorOperator(*exprs))
    elif operator is Operator.unequal:
        return Negation(Negation(XorOperator(*exprs)))
    elif operator is Operator.lesser:
        expr1, expr2 = exprs
        return SignTest(AddOperator(expr1, Complement(expr2)))
    elif operator is Operator.greater:
        expr1, expr2 = exprs
        return SignTest(AddOperator(expr2, Complement(expr1)))
    elif operator is Operator.lesser_equal:
        expr1, expr2 = exprs
        return Negation(SignTest(AddOperator(expr2, Complement(expr1))))
    elif operator is Operator.greater_equal:
        expr1, expr2 = exprs
        return Negation(SignTest(AddOperator(expr1, Complement(expr2))))
    else:
        assert False, operator

def _convertExpressionOperator(node, builder):
    operator = node.operator
    if operator is Operator.call:
        retRef = _convertFunctionCall(node, builder)
        if retRef is None:
            return unit
        else:
            return retRef.emitLoad(builder, node.treeLocation)
    elif operator is Operator.lookup:
        return _convertReferenceLookup(node, builder).emitLoad(
            builder, node.treeLocation
            )
    elif operator is Operator.slice:
        return _convertReferenceSlice(node, builder).emitLoad(
            builder, node.treeLocation
            )
    elif operator is Operator.concatenation:
        return _convertReferenceConcat(node, builder).emitLoad(
            builder, node.treeLocation
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
            return ident.emitLoad(builder, node.location)
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
    elif isinstance(node, MultiMatchNode):
        raise BadExpression(
            'multi-match can only be used as a standalone encoding item',
            node.treeLocation
            )
    else:
        assert False, node

def _convertReferenceLookup(node, builder):
    exprNode, indexNode = node.operands
    if isinstance(exprNode, IdentifierNode):
        ident = _convertIdentifier(exprNode, builder)
        if isinstance(ident, IOChannel):
            channel = ident
            index = buildExpression(indexNode, builder)
            try:
                Expression.checkScalar(index)
            except BadExpression as ex:
                ex.location = indexNode.treeLocation
                raise ex
            return builder.emitIOReference(channel, index)

    ref = buildReference(exprNode, builder)
    index = buildExpression(indexNode, builder)
    try:
        bits = SlicedBits(ref.bits, index, 1)
    except ValueError as ex:
        raise BadExpression('invalid bitwise lookup: %s' % ex, node.location)
    else:
        return Reference(bits, IntType.u(1))

def _convertReferenceSlice(node, builder):
    exprNode, startNode, endNode = node.operands
    ref = buildReference(exprNode, builder)
    if startNode is None:
        offset = IntLiteral(0)
    else:
        offset = buildExpression(startNode, builder)
    if endNode is None:
        refWidth = ref.width
        end = unlimited if refWidth is unlimited else IntLiteral(refWidth)
    else:
        end = buildExpression(endNode, builder)
    if startNode is None or end is unlimited:
        width = end
    else:
        width = AddOperator(end, Complement(offset))
    try:
        if width is not unlimited:
            width = simplifyExpression(Expression.checkScalar(width))
            if isinstance(width, IntLiteral):
                width = width.value
            else:
                raise ValueError('slice width cannot be determined')
        bits = SlicedBits(ref.bits, offset, width)
    except ValueError as ex:
        raise BadExpression('invalid slice: %s' % ex, node.location)
    else:
        typ = IntType(width, width is unlimited)
        return Reference(bits, typ)

def _convertReferenceConcat(node, builder):
    exprNode1, exprNode2 = node.operands
    ref1 = buildReference(exprNode1, builder)
    ref2 = buildReference(exprNode2, builder)
    if ref2.width is unlimited:
        node = exprNode2
        while isinstance(node, OperatorNode) and \
                node.operator is Operator.concatenation:
            node = node.operands[0]
        raise BadExpression.withText(
            'only the first concatenation operand is allowed to have '
            'unlimited width',
            node.treeLocation
            )
    bits = ConcatenatedBits(ref2.bits, ref1.bits)
    width = bits.width
    typ = IntType(width, width != 0 and ref1.type.signed)
    return Reference(bits, typ)

comparisonOperators = (
    Operator.negation, Operator.equal, Operator.unequal,
    Operator.lesser, Operator.lesser_equal,
    Operator.greater, Operator.greater_equal,
    )

def _convertReferenceOperator(node, builder):
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
        return _convertReferenceLookup(node, builder)
    elif operator is Operator.slice:
        return _convertReferenceSlice(node, builder)
    elif operator is Operator.concatenation:
        return _convertReferenceConcat(node, builder)
    else:
        expr = _convertArithmetic(node, builder)
        typ = IntType.u(1) if operator in comparisonOperators else IntType.int
        return Reference(FixedValue(expr, typ.width), typ)

def buildReference(node, builder):
    if isinstance(node, NumberNode):
        literal = IntLiteral(node.value)
        typ = IntType(node.width, node.width is unlimited)
        return Reference(FixedValue(literal, node.width), typ)
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
    elif isinstance(node, MultiMatchNode):
        raise BadExpression(
            'multi-match can only be used as a standalone encoding item',
            node.treeLocation
            )
    elif isinstance(node, OperatorNode):
        return _convertReferenceOperator(node, builder)
    else:
        assert False, node

def emitCodeFromStatements(reader, builder, statements, retType):
    '''Emits a code block from the given statements.
    '''
    for node in statements:
        numNodesBefore = len(builder.nodes)

        if isinstance(node, AssignmentNode):
            try:
                lhs = buildReference(node.lhs, builder)
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

            lhs.emitStore(builder, rhs, node.lhs.treeLocation)

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
                if not isinstance(retType, ReferenceType):
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

        elif isinstance(node, BranchNode):
            # TODO: Add support.
            continue

        elif isinstance(node, LabelNode):
            # TODO: Add support.
            continue

        elif isinstance(node, EmptyNode):
            # Empty statement (NOP).
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
                stateChanged |= execNode.storage.canLoadHaveSideEffect()
            elif isinstance(execNode, Store):
                stateChanged = True
        if not stateChanged:
            reader.warning('statement has no effect')
