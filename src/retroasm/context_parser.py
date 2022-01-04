from __future__ import annotations

from dataclasses import dataclass
from typing import Union

from .expression_nodes import DeclarationNode, ParseNode
from .mode import Mode
from .types import IntType, ReferenceType, Width

PlaceholderSpec = Union["MatchPlaceholderSpec", "ValuePlaceholderSpec"]


@dataclass(frozen=True)
class ValuePlaceholderSpec:
    decl: DeclarationNode
    type: IntType
    value: ParseNode | None

    @property
    def name(self) -> str:
        return self.decl.name.name

    @property
    def encodingWidth(self) -> Width:
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
    def encodingWidth(self) -> int | None:
        return self.mode.encodingWidth

    @property
    def type(self) -> None | IntType | ReferenceType:
        return self.mode.semanticsType

    @property
    def value(self) -> None:
        return None

    def __str__(self) -> str:
        return f"{{{self.mode.name} {self.name}}}"
