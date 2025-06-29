from __future__ import annotations

from collections.abc import Iterator, Mapping
from dataclasses import dataclass
from typing import override

from ..codeblock_builder import SemanticsCodeBlockBuilder, returned_bits
from ..codeblock_simplifier import simplify_block
from ..expression import Expression
from ..expression_simplifier import simplify_expression
from ..input import ErrorCollector, InputLocation
from ..mode import MatchPlaceholder, Mode, Placeholder, ValuePlaceholder
from ..namespace import (
    ContextNamespace,
    GlobalNamespace,
    LocalNamespace,
    NameExistsError,
)
from ..reference import BitString, FixedValue, FixedValueReference, decode_int
from ..symbol import CurrentAddress, ImmediateValue
from ..types import IntType, ReferenceType, parse_type_decl
from ..utils import bad_type
from .expression_builder import BadExpression, convert_definition
from .expression_nodes import ConstRefDeclarationNode, DefinitionNode, ParseNode
from .expression_parser import parse_context


@dataclass(frozen=True)
class _ValuePlaceholderSpec:
    decl: ConstRefDeclarationNode
    type: IntType
    value: ParseNode | None

    @property
    def name(self) -> str:
        return self.decl.name.name

    @override
    def __str__(self) -> str:
        return f"{{{self.type} {self.name}}}"


@dataclass(frozen=True)
class _MatchPlaceholderSpec:
    decl: ConstRefDeclarationNode
    mode: Mode

    @property
    def name(self) -> str:
        return self.decl.name.name

    @property
    def type(self) -> IntType | ReferenceType:
        return self.mode.semantics_type

    @override
    def __str__(self) -> str:
        return f"{{{self.mode.name} {self.name}}}"


def _parse_context(
    ctx_loc: InputLocation,
    modes: Mapping[str, Mode],
    collector: ErrorCollector,
) -> Iterator[_MatchPlaceholderSpec | _ValuePlaceholderSpec]:
    for node in parse_context(ctx_loc):
        match node:
            case ConstRefDeclarationNode() as decl:
                pass
            case DefinitionNode(decl=decl):
                pass
            case node:
                bad_type(node)

        decl_type = decl.type
        type_name = decl_type.name

        if (mode := modes.get(type_name)) is not None:
            if isinstance(node, DefinitionNode):
                collector.error(
                    "mode placeholders cannot be assigned values",
                    location=InputLocation.merge_span(
                        node.location, node.value.tree_location
                    ),
                )
            yield _MatchPlaceholderSpec(decl, mode)
            continue

        try:
            typ = parse_type_decl(type_name)
        except ValueError:
            collector.error(
                f'there is no type or mode named "{type_name}"',
                location=decl_type.location,
            )
            continue

        if isinstance(typ, ReferenceType):
            collector.error(
                "value placeholders cannot be references",
                location=decl_type.location,
            )
            continue

        value = node.value if isinstance(node, DefinitionNode) else None
        yield _ValuePlaceholderSpec(decl, typ, value)


def _reject_mode_match_arg(name: str) -> BitString:
    raise ValueError("mode placeholder cannot be used in context value")


def parse_placeholders(
    ctx_loc: InputLocation,
    modes: Mapping[str, Mode],
    global_namespace: GlobalNamespace,
    collector: ErrorCollector,
) -> Iterator[Placeholder]:
    """Yield the placeholders defined in the given context."""

    pc = global_namespace.program_counter
    ctx_namespace = ContextNamespace(global_namespace)
    for spec in _parse_context(ctx_loc, modes, collector):
        decl = spec.decl
        location = decl.tree_location
        name = spec.name

        match spec:
            case _ValuePlaceholderSpec(value=val_node):
                val_type = spec.type
                val_expr: Expression | None = None
                if val_node is not None:
                    # Compute placeholder value.
                    builder = SemanticsCodeBlockBuilder()
                    placeholder_namespace = LocalNamespace(ctx_namespace)
                    try:
                        value_ref = convert_definition(
                            decl,
                            val_type,
                            val_node,
                            placeholder_namespace,
                            builder,
                        )
                    except BadExpression as ex:
                        collector.error(f"{ex}", location=ex.locations)
                    else:
                        code = builder.create_code_block(returned_bits(value_ref))
                        builder = SemanticsCodeBlockBuilder()
                        pc.emit_store(builder, CurrentAddress(), location)
                        returned = builder.inline_block(code, _reject_mode_match_arg)
                        simplify_block(builder.nodes, returned)
                        (val_bits,) = returned
                        assert isinstance(val_bits, FixedValue), val_bits
                        val_expr = simplify_expression(
                            decode_int(val_bits.expr, val_type)
                        )

                if val_expr is None:
                    val_expr = ImmediateValue(name, val_type)

                # Add value placeholder to namespace.
                val_ref = FixedValueReference(val_expr, val_type)
                try:
                    ctx_namespace.define(name, val_ref, decl.name.location)
                except NameExistsError as ex:
                    collector.error(
                        f"failed to define value placeholder: {ex}",
                        location=ex.locations,
                    )
                    continue

                yield ValuePlaceholder(name, val_type, val_expr, location)

            case _MatchPlaceholderSpec(mode=mode):
                # Mode placeholders are added to the namespace and then rejected
                # in fetch_arg(), to avoid a confusing "unknown name" error.
                try:
                    # The type doesn't matter, as the fetch will be rejected,
                    # but we must provide something.
                    ctx_namespace.add_argument(name, IntType.int, decl.name.location)
                except NameExistsError as ex:
                    collector.error(
                        f"failed to define mode match placeholder: {ex}",
                        location=ex.locations,
                    )
                    continue

                yield MatchPlaceholder(name, mode, location)

            case spec:
                bad_type(spec)
