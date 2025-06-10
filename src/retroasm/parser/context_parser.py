from __future__ import annotations

from collections.abc import Iterator, Mapping
from dataclasses import dataclass
from typing import override

from ..codeblock_builder import SemanticsCodeBlockBuilder
from ..input import ErrorCollector, InputLocation
from ..mode import ComputedPlaceholder, MatchPlaceholder, Mode, ValuePlaceholder
from ..namespace import (
    ContextNamespace,
    GlobalNamespace,
    LocalNamespace,
    NameExistsError,
)
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
    def type(self) -> None | IntType | ReferenceType:
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
    sem_namespace = ContextNamespace(global_namespace)

    for name, spec in placeholder_specs.items():
        decl = spec.decl
        sem_type = spec.type
        value = spec.value

        code = None
        if sem_type is not None and value is not None:
            placeholder_namespace = LocalNamespace(
                sem_namespace, SemanticsCodeBlockBuilder()
            )
            try:
                ref = convert_definition(
                    decl.kind, decl.name.name, sem_type, value, placeholder_namespace
                )
            except BadExpression as ex:
                collector.error(f"{ex}", location=ex.locations)
            else:
                code = placeholder_namespace.create_code_block(ref)

        if sem_type is not None:
            match sem_type:
                case ReferenceType(type=argType) | (IntType() as argType):
                    try:
                        sem_namespace.add_argument(name, argType, decl.name.location)
                    except NameExistsError as ex:
                        collector.error(f"{ex}", location=ex.locations)
                        continue
                case typ:
                    bad_type(typ)

        match spec:
            case ValuePlaceholderSpec():
                sem_type = spec.type  # narrow Python type
                if code is None:
                    yield ValuePlaceholder(name, sem_type)
                else:
                    yield ComputedPlaceholder(name, sem_type, code)
            case MatchPlaceholderSpec(mode=mode):
                yield MatchPlaceholder(name, mode)
            case spec:
                bad_type(spec)
