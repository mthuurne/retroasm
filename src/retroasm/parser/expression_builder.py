from __future__ import annotations

from collections.abc import Iterable, Sequence
from typing import cast

from ..codeblock import Load, Store
from ..expression import (
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
from ..expression_simplifier import simplify_expression
from ..function import Function
from ..namespace import (
    BuilderNamespace,
    LocalNamespace,
    NameExistsError,
    create_io_reference,
)
from ..reference import (
    ConcatenatedBits,
    FixedValue,
    FixedValueReference,
    Reference,
    SingleStorage,
    SlicedBits,
    bad_reference,
    int_reference,
)
from ..storage import IOChannel, Keeper
from ..types import (
    IntType,
    ReferenceType,
    Width,
    parse_type,
    parse_type_decl,
    unlimited,
    width_for_mask,
)
from ..utils import bad_type
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
from .linereader import BadInput, InputLocation, LineReader


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


def declare_variable(node: DeclarationNode, namespace: BuilderNamespace) -> Reference:
    assert node.kind is DeclarationKind.variable, node.kind

    # Determine type.
    # Note: The type node is None for 'ret' declarations, but those are
    #       reference declarations, not variable declarations.
    type_node = node.type
    assert type_node is not None, node
    try:
        typ = parse_type(type_node.name)
    except ValueError as ex:
        raise BadExpression(
            f"bad type name in definition: {ex}", type_node.location
        ) from ex

    # Get name.
    name_node = node.name
    name = name_node.name

    # Add declaration to namespace.
    try:
        return namespace.add_variable(name, typ, name_node.location)
    except NameExistsError as ex:
        raise BadExpression(
            f'failed to declare variable "{typ} {name}": {ex}', *ex.locations
        ) from ex


def convert_definition(
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
            expr = build_expression(value, namespace)
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
            ref = build_reference(value, namespace)
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


def _convert_identifier(
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


def _convert_function_call(
    call_node: OperatorNode, namespace: BuilderNamespace
) -> Reference | None:
    name_node, *arg_nodes = call_node.operands
    builder = namespace.builder

    # Get function object.
    assert isinstance(name_node, IdentifierNode), name_node
    func_name = name_node.name
    try:
        func = namespace[func_name]
    except KeyError:
        raise UnknownNameError(
            func_name, f'no function named "{func_name}"', name_node.location
        ) from None
    if not isinstance(func, Function):
        raise BadExpression(f'"{func_name}" is not a function', name_node.location)

    # Fill argument map.
    if len(arg_nodes) != len(func.args):
        raise BadExpression(
            f'argument count mismatch: function "{func_name}" '
            f"takes {len(func.args):d} argument(s), while call "
            f"provides {len(arg_nodes):d} argument(s)",
            call_node.location,
        )
    arg_map = {}
    for (name, decl), arg_node in zip(func.args.items(), arg_nodes):
        assert arg_node is not None, call_node
        try:
            ref = build_reference(arg_node, namespace)
        except BadExpression as ex:
            raise BadExpression(
                f'in call to function "{func_name}", argument "{name}": {ex}',
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
                        arg_node.tree_location,
                    )
                bits = ref.bits
            case _:
                # Value arguments must be evaluated and truncated when passed.
                value = ref.emit_load(builder, arg_node.tree_location)
                arg_width = decl.width
                if width_for_mask(value.mask) > arg_width:
                    value = truncate(value, arg_width)
                bits = FixedValue(value, arg_width)
        arg_map[name] = bits

    # Inline function call.
    ret_bits = builder.inline_function_call(func, arg_map, call_node.tree_location)
    if ret_bits is None:
        return None
    else:
        match func.ret_type:
            case None:
                # If retType is None, retBits would be None too.
                # assert False, func
                return None
            case ReferenceType(type=retType) | retType:
                return Reference(ret_bits, retType)


def _convert_arithmetic(node: OperatorNode, namespace: BuilderNamespace) -> Expression:
    exprs: Sequence[Expression] = tuple(
        build_expression(cast(ParseNode, node), namespace) for node in node.operands
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


def _convert_expression_operator(
    node: OperatorNode, namespace: BuilderNamespace
) -> Expression:
    match node.operator:
        case Operator.call:
            ref = _convert_function_call(node, namespace)
            if ref is None:
                raise BadExpression(
                    "function does not return anything; expected value",
                    node.tree_location,
                )
            else:
                return ref.emit_load(namespace.builder, node.tree_location)
        case Operator.lookup:
            return _convert_reference_lookup(node, namespace).emit_load(
                namespace.builder, node.tree_location
            )
        case Operator.slice:
            return _convert_reference_slice(node, namespace).emit_load(
                namespace.builder, node.tree_location
            )
        case Operator.concatenation:
            return _convert_reference_concat(node, namespace).emit_load(
                namespace.builder, node.tree_location
            )
        case _:
            return _convert_arithmetic(node, namespace)


def build_expression(node: ParseNode, namespace: BuilderNamespace) -> Expression:
    match node:
        case NumberNode(value=value):
            return IntLiteral(value)
        case IdentifierNode() as ident_node:
            ident = _convert_identifier(ident_node, namespace)
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
            return _convert_expression_operator(node, namespace)
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


def _convert_reference_lookup(
    node: OperatorNode, namespace: BuilderNamespace
) -> Reference:
    expr_node, index_node = node.operands
    assert index_node is not None, node
    if isinstance(expr_node, IdentifierNode):
        match _convert_identifier(expr_node, namespace):
            case IOChannel() as channel:
                index = build_expression(index_node, namespace)
                return create_io_reference(channel, index)
    else:
        assert expr_node is not None, node

    ref = build_reference(expr_node, namespace)
    index = build_expression(index_node, namespace)
    try:
        bits = SlicedBits(ref.bits, index, 1)
    except ValueError as ex:
        raise BadExpression(f"invalid bitwise lookup: {ex}", node.location) from ex
    else:
        return Reference(bits, IntType.u(1))


def _convert_reference_slice(
    node: OperatorNode, namespace: BuilderNamespace
) -> Reference:
    expr_node, start_node, end_node = node.operands
    assert expr_node is not None, node
    ref = build_reference(expr_node, namespace)
    start_expr = (
        IntLiteral(0) if start_node is None else build_expression(start_node, namespace)
    )
    if end_node is None:
        ref_width = ref.width
        if ref_width is unlimited:
            end_expr: Expression | None = None
        else:
            end_expr = IntLiteral(ref_width)
    else:
        end_expr = build_expression(end_node, namespace)
    width_expr: Expression | None = (
        end_expr
        if start_node is None or end_expr is None
        else AddOperator(end_expr, Complement(start_expr))
    )
    try:
        if width_expr is None:
            width: Width = unlimited
        else:
            match simplify_expression(width_expr):
                case IntLiteral(value=value):
                    width = value
                case _:
                    raise ValueError("slice width cannot be determined")
        bits = SlicedBits(ref.bits, start_expr, width)
    except ValueError as ex:
        raise BadExpression(f"invalid slice: {ex}", node.location) from ex
    else:
        typ = IntType(width, width is unlimited)
        return Reference(bits, typ)


def _convert_reference_concat(
    node: OperatorNode, namespace: BuilderNamespace
) -> Reference:
    expr_node1, expr_node2 = node.operands
    assert expr_node1 is not None, node
    assert expr_node2 is not None, node
    ref1 = build_reference(expr_node1, namespace)
    ref2 = build_reference(expr_node2, namespace)
    if ref2.width is unlimited:
        non_first_node = expr_node2
        while (
            isinstance(non_first_node, OperatorNode)
            and non_first_node.operator is Operator.concatenation
        ):
            assert non_first_node.operands[0] is not None, node
            non_first_node = non_first_node.operands[0]
        raise BadExpression.with_text(
            "only the first concatenation operand is allowed to have "
            "unlimited width",
            non_first_node.tree_location,
        )
    bits = ConcatenatedBits(ref2.bits, ref1.bits)
    width = bits.width
    typ = IntType(width, width != 0 and ref1.type.signed)
    return Reference(bits, typ)


comparison_operators = (
    Operator.negation,
    Operator.equal,
    Operator.unequal,
    Operator.lesser,
    Operator.lesser_equal,
    Operator.greater,
    Operator.greater_equal,
)


def _convert_reference_operator(
    node: OperatorNode, namespace: BuilderNamespace
) -> Reference:
    operator = node.operator
    if operator is Operator.call:
        ref = _convert_function_call(node, namespace)
        if ref is None:
            raise BadExpression(
                "function does not return anything; expected reference",
                node.tree_location,
            )
        else:
            return ref
    elif operator is Operator.lookup:
        return _convert_reference_lookup(node, namespace)
    elif operator is Operator.slice:
        return _convert_reference_slice(node, namespace)
    elif operator is Operator.concatenation:
        return _convert_reference_concat(node, namespace)
    else:
        expr = _convert_arithmetic(node, namespace)
        typ = IntType.u(1) if operator in comparison_operators else IntType.int
        return FixedValueReference(expr, typ)


def build_reference(node: ParseNode, namespace: BuilderNamespace) -> Reference:
    match node:
        case NumberNode(value=value, width=width):
            return int_reference(value, IntType(width, width is unlimited))
        case DeclarationNode() as decl:
            return declare_variable(decl, namespace)
        case DefinitionNode(tree_location=location):
            raise BadExpression("definition must be only statement on a line", location)
        case IdentifierNode() as ident_node:
            ident = _convert_identifier(ident_node, namespace)
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
            return _convert_reference_operator(operator, namespace)
        case node:
            raise TypeError(type(node).__name__)


def build_statement_eval(
    reader: LineReader, where_desc: str, namespace: LocalNamespace, node: ParseNode
) -> None:
    """
    Emits loads and stores on the given namespace that produce the (side)
    effects of evaluating the given node.
    Errors and warnings are logged on the given reader, using whereDesc as the
    description of the statement's origin.
    """
    builder = namespace.builder
    num_nodes_before = len(builder.nodes)

    match node:
        case AssignmentNode():
            try:
                lhs = build_reference(node.lhs, namespace)
            except BadExpression as ex:
                reader.error(
                    "bad expression on left hand side of assignment in %s: %s",
                    where_desc,
                    ex,
                    location=ex.locations,
                )
                return

            try:
                rhs = build_expression(node.rhs, namespace)
            except BadExpression as ex:
                reader.error(
                    "bad expression on right hand side of assignment in %s: %s",
                    where_desc,
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
                ref_ = _convert_function_call(node, namespace)
            except BadExpression as ex:
                reader.error("%s", ex, location=ex.locations)
            # Skip no-effect check: if a function does nothing, it likely either
            # does so on purpose or a warning will already have been issued there.
            return

        case expr:
            # Evaluate statement for its side effects.
            try:
                build_expression(expr, namespace)
            except BadExpression as ex:
                reader.error(
                    "bad expression in statement in %s: %s",
                    where_desc,
                    ex,
                    location=ex.locations,
                )
                return

    state_changed = False
    for exec_node in builder.nodes[num_nodes_before:]:
        match exec_node:
            case Load(storage=storage):
                state_changed |= storage.can_load_have_side_effect()
            case Store():
                state_changed = True
    if not state_changed:
        reader.warning(
            "statement in %s has no effect", where_desc, location=node.tree_location
        )


def emit_code_from_statements(
    reader: LineReader,
    where_desc: str,
    namespace: LocalNamespace,
    statements: Iterable[ParseNode],
    ret_type: None | IntType | ReferenceType,
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
                name_node = decl.name
                type_node = decl.type
                if type_node is None:
                    # For a function returning a reference, the reference type
                    # is declared in the function header rather than on the
                    # definition line.
                    assert kind == DeclarationKind.reference, kind
                    assert name_node.name == "ret", name_node.name
                    if not isinstance(ret_type, ReferenceType):
                        reader.error(
                            '"ret" defined as reference in function that returns %s',
                            "nothing" if ret_type is None else "value",
                            location=decl.location,
                        )
                        continue
                    typ: IntType | ReferenceType = ret_type
                else:
                    # Determine type.
                    try:
                        typ = parse_type_decl(type_node.name)
                    except ValueError as ex:
                        raise BadExpression(
                            f"bad type name in definition: {ex}", type_node.location
                        ) from ex
                # Evaluate value.
                name = name_node.name
                try:
                    ref = convert_definition(kind, name, typ, value, namespace)
                except BadExpression as ex:
                    reader.error(str(ex), location=ex.locations)
                    ref = bad_reference(typ)
                # Add definition to namespace.
                try:
                    namespace.define(name, ref, name_node.location)
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
                    declare_variable(decl, namespace)
                except BadExpression as ex:
                    reader.error(str(ex), location=ex.locations)

            case BranchNode(cond=cond):
                # Conditional branch.
                # We don't have actual branching support yet, but we can force
                # the condition to be computed.
                cond_value = Negation(build_expression(cond, namespace))
                ref = Reference(SingleStorage(Keeper(1)), IntType.u(1))
                ref.emit_store(namespace.builder, cond_value, cond.tree_location)

            case LabelNode():
                # TODO: Add support.
                pass

            case stmt:
                build_statement_eval(reader, where_desc, namespace, stmt)
