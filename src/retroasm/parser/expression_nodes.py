from __future__ import annotations

from collections.abc import Iterator
from dataclasses import dataclass
from enum import Enum, auto
from typing import override

from ..input import InputLocation
from ..types import Width, unlimited


@dataclass(frozen=True, slots=True, kw_only=True)
class ParseNode:
    location: InputLocation | None = None
    """Location information for this node itself."""

    @property
    def tree_location(self) -> InputLocation | None:
        """Location information for the tree rooted at this node."""
        return self.location

    def __iter__(self) -> Iterator[ParseNode]:
        yield self


@dataclass(frozen=True, slots=True)
class EmptyNode(ParseNode):
    pass


@dataclass(frozen=True, slots=True)
class LabelNode(ParseNode):
    name: str


@dataclass(frozen=True, slots=True)
class FlagTestNode(ParseNode):
    name: str


@dataclass(frozen=True, slots=True)
class BranchNode(ParseNode):
    cond: ParseNode
    target: LabelNode

    @property
    @override
    def tree_location(self) -> InputLocation | None:
        return InputLocation.merge_span(self.location, self.target.tree_location)


@dataclass(frozen=True, slots=True)
class AssignmentNode(ParseNode):
    lhs: ParseNode
    rhs: ParseNode

    @property
    @override
    def tree_location(self) -> InputLocation | None:
        return InputLocation.merge_span(self.lhs.tree_location, self.rhs.tree_location)

    @override
    def __iter__(self) -> Iterator[ParseNode]:
        yield self
        yield from self.lhs
        yield from self.rhs


class Operator(Enum):
    bitwise_and = auto()
    bitwise_or = auto()
    bitwise_xor = auto()
    add = auto()
    sub = auto()
    multiply = auto()
    divide = auto()
    modulo = auto()
    complement = auto()
    bitwise_complement = auto()
    concatenation = auto()
    lookup = auto()
    negation = auto()
    slice = auto()
    shift_left = auto()
    shift_right = auto()
    equal = auto()
    unequal = auto()
    lesser = auto()
    lesser_equal = auto()
    greater = auto()
    greater_equal = auto()
    call = auto()


@dataclass(frozen=True, slots=True)
class OperatorNode(ParseNode):
    operator: Operator
    operands: tuple[ParseNode | None, ...]

    @property
    @override
    def tree_location(self) -> InputLocation | None:
        return InputLocation.merge_span(
            self.location,
            *(
                operand.tree_location
                for operand in self.operands
                if operand is not None
            ),
        )

    @override
    def __iter__(self) -> Iterator[ParseNode]:
        yield self
        for operand in self.operands:
            if operand is not None:
                yield from operand

    @override
    def __str__(self) -> str:
        return f"{self.operator.name}({', '.join(str(op) for op in self.operands)})"


@dataclass(frozen=True, slots=True)
class IdentifierNode(ParseNode):
    name: str

    @override
    def __str__(self) -> str:
        return self.name


@dataclass(frozen=True, slots=True)
class MultiMatchNode(ParseNode):
    name: str


@dataclass(frozen=True, slots=True)
class _BaseDeclarationNode(ParseNode):
    type: IdentifierNode
    name: IdentifierNode

    @property
    @override
    def tree_location(self) -> InputLocation | None:
        return InputLocation.merge_span(self.location, self.name.tree_location)


@dataclass(frozen=True, slots=True)
class VariableDeclarationNode(_BaseDeclarationNode):
    # TODO: In Python 3.13, we could make this a Final ClassVar instead.
    @property
    def description(self) -> str:
        return "variable"


@dataclass(frozen=True, slots=True)
class ConstantDeclarationNode(_BaseDeclarationNode):
    # TODO: In Python 3.13, we could make this a Final ClassVar instead.
    @property
    def description(self) -> str:
        return "constant"


@dataclass(frozen=True, slots=True)
class ReferenceDeclarationNode(_BaseDeclarationNode):
    # TODO: In Python 3.13, we could make this a Final ClassVar instead.
    @property
    def description(self) -> str:
        return "reference"


type DeclarationNode = (
    VariableDeclarationNode | ConstantDeclarationNode | ReferenceDeclarationNode
)


@dataclass(frozen=True, slots=True)
class DefinitionNode(ParseNode):
    decl: DeclarationNode
    value: ParseNode

    @property
    @override
    def tree_location(self) -> InputLocation | None:
        return InputLocation.merge_span(
            self.decl.tree_location, self.value.tree_location
        )

    @override
    def __iter__(self) -> Iterator[ParseNode]:
        yield self
        yield from self.decl
        yield from self.value


@dataclass(frozen=True, slots=True)
class NumberNode(ParseNode):
    value: int
    width: Width

    @override
    def __str__(self) -> str:
        width = self.width
        if width is unlimited:
            return f"{self.value:d}"
        else:
            assert isinstance(width, int)
            return f"${{:0{(width + 3) // 4:d}x}}".format(self.value)


def parse_int(value_str: str) -> tuple[int, Width]:
    """
    Parse the given string as a binary, decimal or hexadecimal integer.
    Returns a pair containing the value and the width of the literal in bits.
    Raises ValueError if the given string does not represent an integer.
    """
    if value_str[0] == "$":
        return parse_digits(value_str[1:], 16), (len(value_str) - 1) * 4
    elif value_str[0] == "%":
        return parse_digits(value_str[1:], 2), len(value_str) - 1
    elif value_str[0] == "0" and len(value_str) != 1:
        raise ValueError(f"leading zeroes not allowed on decimal number: {value_str}")
    else:
        return parse_digits(value_str, 10), unlimited


def parse_digits(digits: str, base: int) -> int:
    """
    Wrapper around the "int" constructor that generates a slightly more
    detailed ValueError message if the given string contains characters that
    are not valid as digits in the given base.
    """
    try:
        return int(digits, base)
    except ValueError:
        base_desc = {2: "binary", 10: "decimal", 16: "hexadecimal"}
        raise ValueError(f"bad {base_desc[base]} number: {digits}") from None
