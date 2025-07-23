from __future__ import annotations

from collections.abc import Mapping

from ..codeblock_builder import SemanticsCodeBlockBuilder, decompose_store
from ..expression import Expression, truncate
from ..expression_simplifier import simplify_expression
from ..input import BadInput, ErrorCollector, InputLocation
from ..mode import Mode, ModeMatchReference
from ..namespace import ContextNamespace, GlobalNamespace, NameExistsError
from ..reference import FixedValue, FixedValueReference, decode_int
from ..symbol import CurrentAddress, ImmediateValue
from ..types import ReferenceType, parse_type_decl
from ..utils import bad_type
from .expression_builder import build_expression
from .expression_nodes import ConstRefDeclarationNode, DefinitionNode
from .expression_parser import parse_context


def parse_placeholders(
    ctx_loc: InputLocation,
    modes: Mapping[str, Mode],
    global_namespace: GlobalNamespace,
    collector: ErrorCollector,
) -> ContextNamespace:
    """Yield the placeholders defined in the given context."""

    pc = global_namespace.program_counter
    pc_fixated = {} if pc is None else decompose_store(pc, CurrentAddress())
    ctx_namespace = ContextNamespace(global_namespace)

    for node in parse_context(ctx_loc):
        # Retrieve info from AST.
        match node:
            case ConstRefDeclarationNode() as decl:
                pass
            case DefinitionNode(decl=decl):
                pass
            case node:
                bad_type(node)
        decl_type = decl.type
        type_name = decl_type.name
        name = decl.name.name

        if (mode := modes.get(type_name)) is not None:
            # Found a mode by this name.
            if isinstance(node, DefinitionNode):
                collector.error(
                    "mode match placeholder cannot have a defined value",
                    location=InputLocation.merge_span(node.location, node.value.tree_location),
                )

            # Mode placeholders are added to the namespace and then rejected
            # when fetched, to avoid a confusing "unknown name" error.
            val_ref: FixedValueReference = ModeMatchReference(name, mode)
        else:
            # Since it's not a mode, it must be a value type.
            try:
                val_type = parse_type_decl(type_name)
            except ValueError:
                collector.error(
                    f'there is no type or mode named "{type_name}"', location=decl_type.location
                )
                continue

            if isinstance(val_type, ReferenceType):
                collector.error(
                    "value placeholder cannot be a reference", location=decl_type.location
                )
                continue

            val_expr: Expression | None = None
            if isinstance(node, DefinitionNode):
                # Compute placeholder value.
                builder = SemanticsCodeBlockBuilder.with_stored_values(pc_fixated)
                try:
                    built_expr = build_expression(node.value, ctx_namespace, builder)
                except BadInput as ex:
                    collector.error(
                        f'in placeholder "{name}" value: {ex}', location=ex.locations
                    )
                else:
                    val_width = val_type.width
                    val_bits = FixedValue(truncate(built_expr, val_width), val_width)
                    code = builder.create_code_block((val_bits,))
                    for access in code.nodes:
                        collector.error(
                            f"state cannot be accessed from the context: {access}",
                            location=access.location,
                        )
                    # We put in a single FixedValue, we get out a single FixedValue.
                    (ret_bits,) = code.returned
                    assert isinstance(ret_bits, FixedValue), ret_bits
                    val_expr = simplify_expression(decode_int(ret_bits.expr, val_type))

            if val_expr is None:
                val_expr = ImmediateValue(name, val_type)

            val_ref = FixedValueReference(val_expr, val_type)

        try:
            ctx_namespace.define(name, val_ref, decl.name.location)
        except NameExistsError as ex:
            collector.error(f"failed to define placeholder: {ex}", location=ex.locations)

    return ctx_namespace
