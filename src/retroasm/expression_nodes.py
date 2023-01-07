from __future__ import annotations

from collections.abc import Iterator
from dataclasses import dataclass
from enum import Enum, auto

from .linereader import BadInput, InputLocation
from .types import Width, unlimited


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
    def tree_location(self) -> InputLocation | None:
        return InputLocation.merge_span(self.location, self.target.tree_location)


@dataclass(frozen=True, slots=True)
class AssignmentNode(ParseNode):
    lhs: ParseNode
    rhs: ParseNode

    @property
    def tree_location(self) -> InputLocation | None:
        return InputLocation.merge_span(self.lhs.tree_location, self.rhs.tree_location)

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
    def tree_location(self) -> InputLocation | None:
        return InputLocation.merge_span(
            self.location,
            *(
                operand.tree_location
                for operand in self.operands
                if operand is not None
            ),
        )

    def __iter__(self) -> Iterator[ParseNode]:
        yield self
        for operand in self.operands:
            if operand is not None:
                yield from operand

    def __str__(self) -> str:
        return f"{self.operator.name}({', '.join(str(op) for op in self.operands)})"


@dataclass(frozen=True, slots=True)
class IdentifierNode(ParseNode):
    name: str

    def __str__(self) -> str:
        return self.name


@dataclass(frozen=True, slots=True)
class MultiMatchNode(ParseNode):
    name: str


class DeclarationKind(Enum):
    variable = auto()
    constant = auto()
    reference = auto()


@dataclass(frozen=True, slots=True)
class DeclarationNode(ParseNode):
    kind: DeclarationKind
    type: IdentifierNode | None
    name: IdentifierNode

    @property
    def tree_location(self) -> InputLocation | None:
        return InputLocation.merge_span(self.location, self.name.tree_location)


@dataclass(frozen=True, slots=True)
class DefinitionNode(ParseNode):
    decl: DeclarationNode
    value: ParseNode

    @property
    def tree_location(self) -> InputLocation | None:
        return InputLocation.merge_span(
            self.decl.tree_location, self.value.tree_location
        )

    def __iter__(self) -> Iterator[ParseNode]:
        yield self
        yield from self.decl
        yield from self.value


@dataclass(frozen=True, slots=True)
class NumberNode(ParseNode):
    value: int
    width: Width

    def __str__(self) -> str:
        width = self.width
        if width is unlimited:
            return f"{self.value:d}"
        else:
            assert isinstance(width, int)
            return f"${{:0{(width + 3) // 4:d}x}}".format(self.value)


class ParseError(BadInput):
    """Raised when the input text cannot be parsed into an expression."""


def parseInt(valueStr: str) -> tuple[int, Width]:
    """
    Parse the given string as a binary, decimal or hexadecimal integer.
    Returns a pair containing the value and the width of the literal in bits.
    Raises ValueError if the given string does not represent an integer.
    """
    if valueStr[0] == "$":
        return parseDigits(valueStr[1:], 16), (len(valueStr) - 1) * 4
    elif valueStr[0] == "%":
        return parseDigits(valueStr[1:], 2), len(valueStr) - 1
    elif valueStr[0] == "0" and len(valueStr) != 1:
        raise ValueError(f"leading zeroes not allowed on decimal number: {valueStr}")
    else:
        return parseDigits(valueStr, 10), unlimited


def parseDigits(digits: str, base: int) -> int:
    """
    Wrapper around the "int" constructor that generates a slightly more
    detailed ValueError message if the given string contains characters that
    are not valid as digits in the given base.
    """
    try:
        return int(digits, base)
    except ValueError:
        baseDesc = {2: "binary", 10: "decimal", 16: "hexadecimal"}
        raise ValueError(f"bad {baseDesc[base]} number: {digits}") from None
