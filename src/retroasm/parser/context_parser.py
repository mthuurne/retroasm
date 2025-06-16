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
from ..namespace import ContextNamespace, GlobalNamespace, LocalNamespace
from ..reference import BitString, FixedValue, decode_int
from ..symbol import CurrentAddress, ImmediateValue
from ..types import IntType, ReferenceType, parse_type_decl
from ..utils import bad_type
from .expression_builder import BadExpression, convert_definition
from .expression_nodes import DeclarationNode, DefinitionNode, ParseNode
from .expression_parser import parse_context


@dataclass(frozen=True)
class _ValuePlaceholderSpec:
    decl: DeclarationNode
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
    decl: DeclarationNode
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
) -> Mapping[str, _MatchPlaceholderSpec | _ValuePlaceholderSpec]:
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
                placeholder: _MatchPlaceholderSpec | _ValuePlaceholderSpec
                if mode is not None:
                    placeholder = _MatchPlaceholderSpec(decl, mode)
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
                    placeholder = _ValuePlaceholderSpec(decl, typ, value)
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


def parse_placeholders(
    ctx_loc: InputLocation,
    modes: Mapping[str, Mode],
    global_namespace: GlobalNamespace,
    collector: ErrorCollector,
) -> Iterator[Placeholder]:
    """Create placeholders from a spec."""

    placeholder_specs = _parse_context(ctx_loc, modes, collector)

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
            # Note: Mypy won't narrow the default case to Never without the variable.
            match spec := placeholder_specs[name]:
                case _MatchPlaceholderSpec():
                    raise ValueError(
                        "mode match cannot be used in context value"
                    ) from None
                case _ValuePlaceholderSpec():
                    raise ValueError("value placeholder is declared later") from None
                case _:
                    bad_type(spec)

    for name, spec in placeholder_specs.items():
        decl = spec.decl
        location = decl.tree_location

        match spec:
            case _ValuePlaceholderSpec(type=val_type, value=val_node):
                code = None
                if val_node is not None:
                    builder = SemanticsCodeBlockBuilder()
                    placeholder_namespace = LocalNamespace(ctx_namespace)
                    try:
                        value_ref = convert_definition(
                            decl.kind,
                            decl.name.name,
                            val_type,
                            val_node,
                            placeholder_namespace,
                            builder,
                        )
                    except BadExpression as ex:
                        collector.error(f"{ex}", location=ex.locations)
                    else:
                        code = builder.create_code_block(returned_bits(value_ref))

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

            case _MatchPlaceholderSpec(mode=mode):
                yield MatchPlaceholder(name, mode, location)

            case spec:
                bad_type(spec)
