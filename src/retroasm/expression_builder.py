from __future__ import annotations

from collections.abc import Iterable, Sequence
from typing import cast

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
from .expression_nodes import (
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
    create_io_reference,
)
from .reference import (
    ConcatenatedBits,
    FixedValue,
    FixedValueReference,
    Reference,
    SingleStorage,
    SlicedBits,
    bad_reference,
    int_reference,
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

    def __init__(self, name: str, msg: str, location: InputLocation | None):
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
        return namespace.add_variable(name, typ, nameNode.location)
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
            raise BadExpression.with_text(
                f"{ref.width}-bit value does not match " f'declared type "{typ.type}"',
                value.tree_location,
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
    match value:
        case Function():
            raise BadExpression(f'function "{name}" is not called', node.location)
        case IOChannel() | Reference():
            return value
        case value:
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
        match decl:
            case ReferenceType():
                # For reference arguments, we demand the passed width to match the
                # argument width, so truncation is never required.
                if ref.width != decl.type.width:
                    raise BadExpression.with_text(
                        f"{ref.width}-bit reference passed for "
                        f'reference argument "{decl} {name}"',
                        argNode.tree_location,
                    )
                bits = ref.bits
            case _:
                # Value arguments must be evaluated and truncated when passed.
                value = ref.emit_load(builder, argNode.tree_location)
                argWidth = decl.width
                if width_for_mask(value.mask) > argWidth:
                    value = truncate(value, argWidth)
                bits = FixedValue(value, argWidth)
        argMap[name] = bits

    # Inline function call.
    retBits = builder.inline_function_call(func, argMap, callNode.tree_location)
    if retBits is None:
        return None
    else:
        match func.retType:
            case None:
                # If retType is None, retBits would be None too.
                # assert False, func
                return None
            case ReferenceType(type=retType) | retType:
                return Reference(retBits, retType)


def _convertArithmetic(node: OperatorNode, namespace: BuilderNamespace) -> Expression:
    exprs: Sequence[Expression] = tuple(
        buildExpression(cast(ParseNode, node), namespace) for node in node.operands
    )
    match node.operator:
        case Operator.bitwise_and:
            return AndOperator(*exprs)
        case Operator.bitwise_or:
            return OrOperator(*exprs)
        case Operator.bitwise_xor:
            return XorOperator(*exprs)
        case Operator.shift_left:
            return LVShift(*exprs)
        case Operator.shift_right:
            return RVShift(*exprs)
        case Operator.add:
            return AddOperator(*exprs)
        case Operator.sub:
            expr1, expr2 = exprs
            return AddOperator(expr1, Complement(expr2))
        case Operator.complement:
            return Complement(*exprs)
        case Operator.bitwise_complement:
            return XorOperator(IntLiteral(-1), *exprs)
        case Operator.negation:
            return Negation(*exprs)
        case Operator.equal:
            return Negation(XorOperator(*exprs))
        case Operator.unequal:
            return Negation(Negation(XorOperator(*exprs)))
        case Operator.lesser:
            expr1, expr2 = exprs
            return SignTest(AddOperator(expr1, Complement(expr2)))
        case Operator.greater:
            expr1, expr2 = exprs
            return SignTest(AddOperator(expr2, Complement(expr1)))
        case Operator.lesser_equal:
            expr1, expr2 = exprs
            return Negation(SignTest(AddOperator(expr2, Complement(expr1))))
        case Operator.greater_equal:
            expr1, expr2 = exprs
            return Negation(SignTest(AddOperator(expr1, Complement(expr2))))
        case operator:
            assert False, operator


def _convertExpressionOperator(
    node: OperatorNode, namespace: BuilderNamespace
) -> Expression:
    match node.operator:
        case Operator.call:
            ref = _convertFunctionCall(node, namespace)
            if ref is None:
                raise BadExpression(
                    "function does not return anything; expected value",
                    node.tree_location,
                )
            else:
                return ref.emit_load(namespace.builder, node.tree_location)
        case Operator.lookup:
            return _convertReferenceLookup(node, namespace).emit_load(
                namespace.builder, node.tree_location
            )
        case Operator.slice:
            return _convertReferenceSlice(node, namespace).emit_load(
                namespace.builder, node.tree_location
            )
        case Operator.concatenation:
            return _convertReferenceConcat(node, namespace).emit_load(
                namespace.builder, node.tree_location
            )
        case _:
            return _convertArithmetic(node, namespace)


def buildExpression(node: ParseNode, namespace: BuilderNamespace) -> Expression:
    match node:
        case NumberNode(value=value):
            return IntLiteral(value)
        case IdentifierNode() as ident_node:
            ident = _convertIdentifier(ident_node, namespace)
            match ident:
                case IOChannel():
                    raise BadExpression(
                        f'I/O channel "{ident_node.name}" can only be used for lookup',
                        ident_node.location,
                    )
                case Reference() as ref:
                    return ref.emit_load(namespace.builder, node.location)
                case ident:
                    bad_type(ident)
        case OperatorNode():
            return _convertExpressionOperator(node, namespace)
        case DeclarationNode(tree_location=location):
            raise BadExpression("variable declaration is not allowed here", location)
        case DefinitionNode(tree_location=location):
            raise BadExpression("definition must be only statement on a line", location)
        case MultiMatchNode(tree_location=location):
            raise BadExpression(
                "multi-match can only be used as a standalone encoding item", location
            )
        case node:
            raise TypeError(type(node).__name__)


def _convertReferenceLookup(
    node: OperatorNode, namespace: BuilderNamespace
) -> Reference:
    exprNode, indexNode = node.operands
    assert indexNode is not None, node
    if isinstance(exprNode, IdentifierNode):
        match _convertIdentifier(exprNode, namespace):
            case IOChannel() as channel:
                index = buildExpression(indexNode, namespace)
                return create_io_reference(channel, index)
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
            match simplifyExpression(widthExpr):
                case IntLiteral(value=value):
                    width = value
                case _:
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
        raise BadExpression.with_text(
            "only the first concatenation operand is allowed to have "
            "unlimited width",
            nonFirstNode.tree_location,
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
                node.tree_location,
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
    match node:
        case NumberNode(value=value, width=width):
            return int_reference(value, IntType(width, width is unlimited))
        case DeclarationNode() as decl:
            return declareVariable(decl, namespace)
        case DefinitionNode(tree_location=location):
            raise BadExpression("definition must be only statement on a line", location)
        case IdentifierNode() as ident_node:
            ident = _convertIdentifier(ident_node, namespace)
            match ident:
                case IOChannel():
                    raise BadExpression(
                        f'I/O channel "{ident_node.name}" can only be used for lookup',
                        ident_node.location,
                    )
                case ident:
                    return ident
        case MultiMatchNode(tree_location=location):
            raise BadExpression(
                "multi-match can only be used as a standalone encoding item",
                location,
            )
        case OperatorNode() as operator:
            return _convertReferenceOperator(operator, namespace)
        case node:
            raise TypeError(type(node).__name__)


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

    match node:
        case AssignmentNode():
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

            lhs.emit_store(builder, rhs, node.lhs.tree_location)

        case EmptyNode():
            # Empty statement (NOP).
            # This is supposed to have no effect, so skip no-effect check.
            return

        case OperatorNode(operator=Operator.call):
            # Function call.
            try:
                ref_ = _convertFunctionCall(node, namespace)
            except BadExpression as ex:
                reader.error("%s", ex, location=ex.locations)
            # Skip no-effect check: if a function does nothing, it likely either
            # does so on purpose or a warning will already have been issued there.
            return

        case expr:
            # Evaluate statement for its side effects.
            try:
                buildExpression(expr, namespace)
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
        match execNode:
            case Load(storage=storage):
                stateChanged |= storage.can_load_have_side_effect()
            case Store():
                stateChanged = True
    if not stateChanged:
        reader.warning(
            "statement in %s has no effect", whereDesc, location=node.tree_location
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
        match node:
            case DefinitionNode(decl=decl, value=value):
                # Constant/reference definition.
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
                        reader.error(
                            '"ret" defined as reference in function that returns %s',
                            "nothing" if retType is None else "value",
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
                    ref = convertDefinition(kind, name, typ, value, namespace)
                except BadExpression as ex:
                    reader.error(str(ex), location=ex.locations)
                    ref = bad_reference(typ)
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

            case DeclarationNode() as decl:
                # Variable declaration.
                try:
                    declareVariable(decl, namespace)
                except BadExpression as ex:
                    reader.error(str(ex), location=ex.locations)

            case BranchNode(cond=cond):
                # Conditional branch.
                # We don't have actual branching support yet, but we can force
                # the condition to be computed.
                cond_value = Negation(buildExpression(cond, namespace))
                ref = Reference(SingleStorage(Keeper(1)), IntType.u(1))
                ref.emit_store(namespace.builder, cond_value, cond.tree_location)

            case LabelNode():
                # TODO: Add support.
                pass

            case stmt:
                buildStatementEval(reader, whereDesc, namespace, stmt)
