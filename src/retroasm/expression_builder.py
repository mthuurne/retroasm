from __future__ import annotations

from typing import Iterable, Sequence, cast

from .codeblock import Load, Store
from .expression import (
    AddOperator,
    AndOperator,
    Complement,
    Expression,
    IntLiteral,
    LVShift,
    Negation,
    OrOperator,
    RVShift,
    SignTest,
    XorOperator,
    truncate,
)
from .expression_parser import (
    AssignmentNode,
    BranchNode,
    DeclarationKind,
    DeclarationNode,
    DefinitionNode,
    EmptyNode,
    IdentifierNode,
    LabelNode,
    MultiMatchNode,
    NumberNode,
    Operator,
    OperatorNode,
    ParseNode,
)
from .expression_simplifier import simplifyExpression
from .function import Function
from .linereader import BadInput, InputLocation, LineReader
from .namespace import (
    BuilderNamespace,
    LocalNamespace,
    NameExistsError,
    createIOReference,
)
from .reference import (
    ConcatenatedBits,
    FixedValue,
    FixedValueReference,
    Reference,
    SingleStorage,
    SlicedBits,
    badReference,
    intReference,
)
from .storage import IOChannel, Keeper
from .types import (
    IntType,
    ReferenceType,
    Width,
    parse_type,
    parse_type_decl,
    unlimited,
    width_for_mask,
)
from .utils import bad_type


class BadExpression(BadInput):
    """Raised when the input text cannot be parsed into an expression."""


class UnknownNameError(BadExpression):
    """
    Raised when an expression contains an identifier that does not occur
    in any of its surrounding namespaces.
    """

    def __init__(self, name: str, msg: str, location: InputLocation):
        BadExpression.__init__(self, msg, location)
        self.name = name


def declareVariable(node: DeclarationNode, namespace: BuilderNamespace) -> Reference:
    assert node.kind is DeclarationKind.variable, node.kind

    # Determine type.
    # Note: The type node is None for 'ret' declarations, but those are
    #       reference declarations, not variable declarations.
    typeNode = node.type
    assert typeNode is not None, node
    try:
        typ = parse_type(typeNode.name)
    except ValueError as ex:
        raise BadExpression(
            f"bad type name in definition: {ex}", typeNode.location
        ) from ex

    # Get name.
    nameNode = node.name
    name = nameNode.name

    # Add declaration to namespace.
    try:
        return namespace.addVariable(name, typ, nameNode.location)
    except NameExistsError as ex:
        raise BadExpression(
            f'failed to declare variable "{typ} {name}": {ex}', *ex.locations
        ) from ex


def convertDefinition(
    kind: DeclarationKind,
    name: str,
    typ: IntType | ReferenceType,
    value: ParseNode,
    namespace: BuilderNamespace,
) -> Reference:
    """
    Build and validate the right hand side of a definition.
    Returns a Reference to the value.
    Raises BadExpression if validation fails.
    """
    if kind is DeclarationKind.constant:
        try:
            expr = buildExpression(value, namespace)
        except BadInput as ex:
            # Note: Catch BadInput rather than BadExpression because builder
            #       could throw IllegalStateAccess.
            raise BadExpression(
                f'bad value for constant "{typ} {name}": {ex}', *ex.locations
            ) from ex
        assert isinstance(typ, IntType), typ
        return FixedValueReference(truncate(expr, typ.width), typ)
    elif kind is DeclarationKind.reference:
        try:
            ref = buildReference(value, namespace)
        except BadExpression as ex:
            raise BadExpression(
                f'bad value for reference "{typ} {name}": {ex}', *ex.locations
            ) from ex
        assert isinstance(typ, ReferenceType), typ
        if typ.type.width != ref.width:
            raise BadExpression.withText(
                f"{ref.width}-bit value does not match " f'declared type "{typ.type}"',
                value.treeLocation,
            )
        return ref
    else:
        assert False, kind


def _convertIdentifier(
    node: IdentifierNode, namespace: BuilderNamespace
) -> IOChannel | Reference:
    """Looks up an identifier in a namespace."""
    name = node.name
    try:
        value = namespace[name]
    except KeyError:
        raise UnknownNameError(name, f'unknown name "{name}"', node.location) from None
    if isinstance(value, Function):
        raise BadExpression(f'function "{name}" is not called', node.location)
    elif isinstance(value, (IOChannel, Reference)):
        return value
    else:
        bad_type(value)


def _convertFunctionCall(
    callNode: OperatorNode, namespace: BuilderNamespace
) -> Reference | None:
    nameNode, *argNodes = callNode.operands
    builder = namespace.builder

    # Get function object.
    assert isinstance(nameNode, IdentifierNode), nameNode
    funcName = nameNode.name
    try:
        func = namespace[funcName]
    except KeyError:
        raise UnknownNameError(
            funcName, f'no function named "{funcName}"', nameNode.location
        ) from None
    if not isinstance(func, Function):
        raise BadExpression(f'"{funcName}" is not a function', nameNode.location)

    # Fill argument map.
    if len(argNodes) != len(func.args):
        raise BadExpression(
            f'argument count mismatch: function "{funcName}" '
            f"takes {len(func.args):d} argument(s), while call "
            f"provides {len(argNodes):d} argument(s)",
            callNode.location,
        )
    argMap = {}
    for (name, decl), argNode in zip(func.args.items(), argNodes):
        assert argNode is not None, callNode
        try:
            ref = buildReference(argNode, namespace)
        except BadExpression as ex:
            raise BadExpression(
                f'in call to function "{funcName}", argument "{name}": {ex}',
                *ex.locations,
            ) from ex
        if isinstance(decl, ReferenceType):
            # For reference arguments, we demand the passed width to match the
            # argument width, so truncation is never required.
            if ref.width != decl.type.width:
                raise BadExpression.withText(
                    f"{ref.width}-bit reference passed for "
                    f'reference argument "{decl} {name}"',
                    argNode.treeLocation,
                )
            bits = ref.bits
        else:
            # Value arguments must be evaluated and truncated when passed.
            value = ref.emitLoad(builder, argNode.treeLocation)
            argWidth = decl.width
            if width_for_mask(value.mask) > argWidth:
                value = truncate(value, argWidth)
            bits = FixedValue(value, argWidth)
        argMap[name] = bits

    # Inline function call.
    retBits = builder.inlineFunctionCall(func, argMap, callNode.treeLocation)
    if retBits is None:
        return None
    else:
        retType = func.retType
        if isinstance(retType, ReferenceType):
            retType = retType.type
        else:
            # If retType is None, retBits would be None too.
            assert retType is not None, func
        return Reference(retBits, retType)


def _convertArithmetic(node: OperatorNode, namespace: BuilderNamespace) -> Expression:
    operator = node.operator
    exprs: Sequence[Expression] = tuple(
        buildExpression(cast(ParseNode, node), namespace) for node in node.operands
    )
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


def _convertExpressionOperator(
    node: OperatorNode, namespace: BuilderNamespace
) -> Expression:
    operator = node.operator
    if operator is Operator.call:
        ref = _convertFunctionCall(node, namespace)
        if ref is None:
            raise BadExpression(
                "function does not return anything; expected value", node.treeLocation
            )
        else:
            return ref.emitLoad(namespace.builder, node.treeLocation)
    elif operator is Operator.lookup:
        return _convertReferenceLookup(node, namespace).emitLoad(
            namespace.builder, node.treeLocation
        )
    elif operator is Operator.slice:
        return _convertReferenceSlice(node, namespace).emitLoad(
            namespace.builder, node.treeLocation
        )
    elif operator is Operator.concatenation:
        return _convertReferenceConcat(node, namespace).emitLoad(
            namespace.builder, node.treeLocation
        )
    else:
        return _convertArithmetic(node, namespace)


def buildExpression(node: ParseNode, namespace: BuilderNamespace) -> Expression:
    if isinstance(node, NumberNode):
        return IntLiteral(node.value)
    elif isinstance(node, IdentifierNode):
        ident = _convertIdentifier(node, namespace)
        if isinstance(ident, IOChannel):
            raise BadExpression(
                f'I/O channel "{node.name}" can only be used for lookup', node.location
            )
        else:
            return ident.emitLoad(namespace.builder, node.location)
    elif isinstance(node, OperatorNode):
        return _convertExpressionOperator(node, namespace)
    elif isinstance(node, DeclarationNode):
        raise BadExpression(
            "variable declaration is not allowed here", node.treeLocation
        )
    elif isinstance(node, DefinitionNode):
        raise BadExpression(
            "definition must be only statement on a line", node.treeLocation
        )
    elif isinstance(node, MultiMatchNode):
        raise BadExpression(
            "multi-match can only be used as a standalone encoding item",
            node.treeLocation,
        )
    else:
        raise TypeError(type(node).__name__)


def _convertReferenceLookup(
    node: OperatorNode, namespace: BuilderNamespace
) -> Reference:
    exprNode, indexNode = node.operands
    assert indexNode is not None, node
    if isinstance(exprNode, IdentifierNode):
        ident = _convertIdentifier(exprNode, namespace)
        if isinstance(ident, IOChannel):
            channel = ident
            index = buildExpression(indexNode, namespace)
            return createIOReference(channel, index)
    else:
        assert exprNode is not None, node

    ref = buildReference(exprNode, namespace)
    index = buildExpression(indexNode, namespace)
    try:
        bits = SlicedBits(ref.bits, index, 1)
    except ValueError as ex:
        raise BadExpression(f"invalid bitwise lookup: {ex}", node.location) from ex
    else:
        return Reference(bits, IntType.u(1))


def _convertReferenceSlice(
    node: OperatorNode, namespace: BuilderNamespace
) -> Reference:
    exprNode, startNode, endNode = node.operands
    assert exprNode is not None, node
    ref = buildReference(exprNode, namespace)
    startExpr = (
        IntLiteral(0) if startNode is None else buildExpression(startNode, namespace)
    )
    if endNode is None:
        refWidth = ref.width
        if refWidth is unlimited:
            endExpr: Expression | None = None
        else:
            endExpr = IntLiteral(cast(int, refWidth))
    else:
        endExpr = buildExpression(endNode, namespace)
    widthExpr: Expression | None = (
        endExpr
        if startNode is None or endExpr is None
        else AddOperator(endExpr, Complement(startExpr))
    )
    try:
        if widthExpr is None:
            width: Width = unlimited
        else:
            widthExpr = simplifyExpression(widthExpr)
            if isinstance(widthExpr, IntLiteral):
                width = widthExpr.value
            else:
                raise ValueError("slice width cannot be determined")
        bits = SlicedBits(ref.bits, startExpr, width)
    except ValueError as ex:
        raise BadExpression(f"invalid slice: {ex}", node.location) from ex
    else:
        typ = IntType(width, width is unlimited)
        return Reference(bits, typ)


def _convertReferenceConcat(
    node: OperatorNode, namespace: BuilderNamespace
) -> Reference:
    exprNode1, exprNode2 = node.operands
    assert exprNode1 is not None, node
    assert exprNode2 is not None, node
    ref1 = buildReference(exprNode1, namespace)
    ref2 = buildReference(exprNode2, namespace)
    if ref2.width is unlimited:
        nonFirstNode = exprNode2
        while (
            isinstance(nonFirstNode, OperatorNode)
            and nonFirstNode.operator is Operator.concatenation
        ):
            assert nonFirstNode.operands[0] is not None, node
            nonFirstNode = nonFirstNode.operands[0]
        raise BadExpression.withText(
            "only the first concatenation operand is allowed to have "
            "unlimited width",
            nonFirstNode.treeLocation,
        )
    bits = ConcatenatedBits(ref2.bits, ref1.bits)
    width = bits.width
    typ = IntType(width, width != 0 and ref1.type.signed)
    return Reference(bits, typ)


comparisonOperators = (
    Operator.negation,
    Operator.equal,
    Operator.unequal,
    Operator.lesser,
    Operator.lesser_equal,
    Operator.greater,
    Operator.greater_equal,
)


def _convertReferenceOperator(
    node: OperatorNode, namespace: BuilderNamespace
) -> Reference:
    operator = node.operator
    if operator is Operator.call:
        ref = _convertFunctionCall(node, namespace)
        if ref is None:
            raise BadExpression(
                "function does not return anything; expected reference",
                node.treeLocation,
            )
        else:
            return ref
    elif operator is Operator.lookup:
        return _convertReferenceLookup(node, namespace)
    elif operator is Operator.slice:
        return _convertReferenceSlice(node, namespace)
    elif operator is Operator.concatenation:
        return _convertReferenceConcat(node, namespace)
    else:
        expr = _convertArithmetic(node, namespace)
        typ = IntType.u(1) if operator in comparisonOperators else IntType.int
        return FixedValueReference(expr, typ)


def buildReference(node: ParseNode, namespace: BuilderNamespace) -> Reference:
    if isinstance(node, NumberNode):
        return intReference(node.value, IntType(node.width, node.width is unlimited))
    elif isinstance(node, DeclarationNode):
        return declareVariable(node, namespace)
    elif isinstance(node, DefinitionNode):
        raise BadExpression(
            "definition must be only statement on a line", node.treeLocation
        )
    elif isinstance(node, IdentifierNode):
        ident = _convertIdentifier(node, namespace)
        if isinstance(ident, IOChannel):
            raise BadExpression(
                f'I/O channel "{node.name}" can only be used for lookup', node.location
            )
        else:
            return ident
    elif isinstance(node, MultiMatchNode):
        raise BadExpression(
            "multi-match can only be used as a standalone encoding item",
            node.treeLocation,
        )
    elif isinstance(node, OperatorNode):
        return _convertReferenceOperator(node, namespace)
    else:
        assert False, node


def buildStatementEval(
    reader: LineReader, whereDesc: str, namespace: LocalNamespace, node: ParseNode
) -> None:
    """
    Emits loads and stores on the given namespace that produce the (side)
    effects of evaluating the given node.
    Errors and warnings are logged on the given reader, using whereDesc as the
    description of the statement's origin.
    """
    builder = namespace.builder
    numNodesBefore = len(builder.nodes)

    if isinstance(node, AssignmentNode):
        try:
            lhs = buildReference(node.lhs, namespace)
        except BadExpression as ex:
            reader.error(
                "bad expression on left hand side of assignment in %s: %s",
                whereDesc,
                ex,
                location=ex.locations,
            )
            return

        try:
            rhs = buildExpression(node.rhs, namespace)
        except BadExpression as ex:
            reader.error(
                "bad expression on right hand side of assignment in %s: %s",
                whereDesc,
                ex,
                location=ex.locations,
            )
            return

        lhs.emitStore(builder, rhs, node.lhs.treeLocation)

    elif isinstance(node, EmptyNode):
        # Empty statement (NOP).
        # This is supposed to have no effect, so skip no-effect check.
        return

    elif isinstance(node, OperatorNode) and node.operator is Operator.call:
        # Function call.
        try:
            ref_ = _convertFunctionCall(node, namespace)
        except BadExpression as ex:
            reader.error("%s", ex, location=ex.locations)
        # Skip no-effect check: if a function does nothing, it likely either
        # does so on purpose or a warning will already have been issued there.
        return

    else:
        # Evaluate statement for its side effects.
        try:
            buildExpression(node, namespace)
        except BadExpression as ex:
            reader.error(
                "bad expression in statement in %s: %s",
                whereDesc,
                ex,
                location=ex.locations,
            )
            return

    stateChanged = False
    for execNode in builder.nodes[numNodesBefore:]:
        if isinstance(execNode, Load):
            stateChanged |= execNode.storage.canLoadHaveSideEffect()
        elif isinstance(execNode, Store):
            stateChanged = True
    if not stateChanged:
        reader.warning(
            "statement in %s has no effect", whereDesc, location=node.treeLocation
        )


def emitCodeFromStatements(
    reader: LineReader,
    whereDesc: str,
    namespace: LocalNamespace,
    statements: Iterable[ParseNode],
    retType: None | IntType | ReferenceType,
) -> None:
    """
    Emits a code block from the given statements.
    Errors and warnings are logged on the given reader, using whereDesc as the
    description of the statement's origin.
    """
    for node in statements:
        if isinstance(node, DefinitionNode):
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
                assert nameNode.name == "ret", nameNode.name
                if not isinstance(retType, ReferenceType):
                    desc = "nothing" if retType is None else "value"
                    reader.error(
                        f'"ret" defined as reference in function that returns {desc}',
                        location=decl.location,
                    )
                    continue
                typ: IntType | ReferenceType = retType
            else:
                # Determine type.
                try:
                    typ = parse_type_decl(typeNode.name)
                except ValueError as ex:
                    raise BadExpression(
                        f"bad type name in definition: {ex}", typeNode.location
                    ) from ex
            # Evaluate value.
            name = nameNode.name
            try:
                ref = convertDefinition(kind, name, typ, node.value, namespace)
            except BadExpression as ex:
                reader.error(str(ex), location=ex.locations)
                ref = badReference(typ)
            # Add definition to namespace.
            try:
                namespace.define(name, ref, nameNode.location)
            except NameExistsError as ex:
                reader.error(
                    'failed to define %s "%s %s": %s',
                    kind.name,
                    typ,
                    name,
                    ex,
                    location=ex.locations,
                )

        elif isinstance(node, DeclarationNode):
            # Variable declaration.
            try:
                declareVariable(node, namespace)
            except BadExpression as ex:
                reader.error(str(ex), location=ex.locations)

        elif isinstance(node, BranchNode):
            # Conditional branch.
            # We don't have actual branching support yet, but we can force
            # the condition to be computed.
            value = Negation(buildExpression(node.cond, namespace))
            ref = Reference(SingleStorage(Keeper(1)), IntType.u(1))
            ref.emitStore(namespace.builder, value, node.cond.treeLocation)

        elif isinstance(node, LabelNode):
            # TODO: Add support.
            pass

        else:
            buildStatementEval(reader, whereDesc, namespace, node)
