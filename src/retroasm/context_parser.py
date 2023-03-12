from __future__ import annotations

from dataclasses import dataclass
from typing import TypeAlias, Union

from .expression_nodes import DeclarationNode, ParseNode
from .mode import Mode
from .types import IntType, ReferenceType, Width

PlaceholderSpec: TypeAlias = Union["MatchPlaceholderSpec", "ValuePlaceholderSpec"]


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

    def __str__(self) -> str:
        return f"{{{self.mode.name} {self.name}}}"
