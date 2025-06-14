from __future__ import annotations

from collections.abc import Iterator, Mapping
from dataclasses import dataclass
from typing import override

from ..codeblock_builder import SemanticsCodeBlockBuilder
from ..codeblock_simplifier import simplify_block
from ..expression import Expression
from ..expression_simplifier import simplify_expression
from ..input import ErrorCollector, InputLocation
from ..mode import MatchPlaceholder, Mode, ValuePlaceholder
from ..namespace import ContextNamespace, GlobalNamespace, LocalNamespace
from ..reference import BitString, FixedValue, decode_int
from ..symbol import CurrentAddress, ImmediateValue
from ..types import IntType, ReferenceType, Width, parse_type_decl
from ..utils import bad_type
from .expression_builder import BadExpression, convert_definition
from .expression_nodes import DeclarationNode, DefinitionNode, ParseNode
from .expression_parser import parse_context

type PlaceholderSpec = MatchPlaceholderSpec | ValuePlaceholderSpec


@dataclass(frozen=True)
class ValuePlaceholderSpec:
    decl: DeclarationNode
    type: IntType
    value: ParseNode | None

    @property
    def name(self) -> str:
        return self.decl.name.name

    @property
    def encoding_width(self) -> Width:
        return self.type.width

    @override
    def __str__(self) -> str:
        return f"{{{self.type} {self.name}}}"


@dataclass(frozen=True)
class MatchPlaceholderSpec:
    decl: DeclarationNode
    mode: Mode

    @property
    def name(self) -> str:
        return self.decl.name.name

    @property
    def encoding_width(self) -> Width | None:
        return self.mode.encoding_width

    @property
    def type(self) -> IntType | ReferenceType:
        return self.mode.semantics_type

    @property
    def value(self) -> None:
        return None

    @override
    def __str__(self) -> str:
        return f"{{{self.mode.name} {self.name}}}"


def parse_mode_context(
    ctx_loc: InputLocation,
    modes: Mapping[str, Mode],
    collector: ErrorCollector,
) -> Mapping[str, PlaceholderSpec]:
    placeholder_specs = {}
    for node in parse_context(ctx_loc):
        match node:
            case (DeclarationNode() as decl) | DefinitionNode(decl=decl):
                name = decl.name.name
                decl_type = decl.type
                assert decl_type is not None
                type_name = decl_type.name

                # Figure out whether the name is a mode or type.
                mode = modes.get(type_name)
                placeholder: PlaceholderSpec
                if mode is not None:
                    placeholder = MatchPlaceholderSpec(decl, mode)
                    if isinstance(node, DefinitionNode):
                        collector.error(
                            "filter values for mode placeholders are not supported yet",
                            location=InputLocation.merge_span(
                                node.location, node.value.tree_location
                            ),
                        )
                else:
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
                            "placeholders cannot be references",
                            location=decl_type.location,
                        )
                        continue
                    value = node.value if isinstance(node, DefinitionNode) else None
                    placeholder = ValuePlaceholderSpec(decl, typ, value)
                if name in placeholder_specs:
                    collector.error(
                        f'multiple placeholders named "{name}"',
                        location=decl.name.location,
                    )
                else:
                    placeholder_specs[name] = placeholder
            case node:
                bad_type(node)

    return placeholder_specs


def build_placeholders(
    placeholder_specs: Mapping[str, PlaceholderSpec],
    global_namespace: GlobalNamespace,
    collector: ErrorCollector,
) -> Iterator[MatchPlaceholder | ValuePlaceholder]:
    """Create placeholders from a spec."""

    ctx_namespace = ContextNamespace(global_namespace)
    # Populate namespace with an argument for each placeholder.
    # This avoids a confusing "unknown name" error when an invalid placeholder is
    # looked up and enables fetch_arg() to produce more specific error messages.
    for name, spec in placeholder_specs.items():
        val_type = spec.type
        if val_type is not None:
            if isinstance(val_type, ReferenceType):
                val_type = val_type.type
            ctx_namespace.add_argument(name, val_type, spec.decl.name.location)

    pc = global_namespace.program_counter

    values: dict[str, FixedValue] = {}

    def fetch_arg(name: str) -> BitString:
        try:
            return values[name]
        except KeyError:
            match placeholder_specs[name]:
                case MatchPlaceholderSpec():
                    raise ValueError(
                        "mode match cannot be used in context value"
                    ) from None
                case ValuePlaceholderSpec():
                    raise ValueError("value placeholder is declared later") from None
                case _ as spec:
                    # Note: mypy 1.16.0 doesn't narrow 'spec' to Never
                    assert False, spec  # bad_type(spec)

    for name, spec in placeholder_specs.items():
        decl = spec.decl
        location = decl.tree_location
        val_type = spec.type
        value_node = spec.value

        code = None
        if val_type is not None and value_node is not None:
            builder = SemanticsCodeBlockBuilder()
            placeholder_namespace = LocalNamespace(ctx_namespace, builder)
            try:
                value_ref = convert_definition(
                    decl.kind,
                    decl.name.name,
                    val_type,
                    value_node,
                    placeholder_namespace,
                )
            except BadExpression as ex:
                collector.error(f"{ex}", location=ex.locations)
            else:
                code = placeholder_namespace.create_code_block(value_ref)

        match spec:
            case ValuePlaceholderSpec():
                val_type = spec.type  # narrow Python type
                val_expr: Expression
                if code is None:
                    val_expr = ImmediateValue(name, val_type)
                else:
                    # Compute placeholder value.
                    builder = SemanticsCodeBlockBuilder()
                    pc.emit_store(builder, CurrentAddress(), location)
                    returned = builder.inline_block(code, fetch_arg)
                    simplify_block(builder.nodes, returned)
                    (val_bits,) = returned
                    assert isinstance(val_bits, FixedValue), val_bits
                    val_expr = simplify_expression(decode_int(val_bits.expr, val_type))

                placeholder = ValuePlaceholder(name, val_type, val_expr, location)
                values[name] = placeholder.bits
                yield placeholder
            case MatchPlaceholderSpec(mode=mode):
                yield MatchPlaceholder(name, mode, location)
            case spec:
                bad_type(spec)
