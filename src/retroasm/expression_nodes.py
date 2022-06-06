from __future__ import annotations

from collections.abc import Iterable, Iterator, Sequence
from enum import Enum, auto
from typing import Union, cast

from .linereader import BadInput, InputLocation, mergeSpan
from .types import Width, unlimited


class ParseNode:
    __slots__ = ("location", "tree_location")

    def __init__(self, location: InputLocation):
        self.location = location
        self.tree_location = location
        """
        Location information, where the span includes to the entire tree
        under this node.
        """

    def __repr__(self) -> str:
        attrStr = ", ".join(
            f"{slot}={getattr(self, slot)}"
            for cls in cast(
                Iterable[type[ParseNode]],
                self.__class__.__mro__[:-2],  # drop ParseNode, object
            )
            for slot in cls.__slots__
        )
        return f"{self.__class__.__name__}({attrStr})"

    def __iter__(self) -> Iterator[ParseNode]:
        yield self


class EmptyNode(ParseNode):
    __slots__ = ()


class LabelNode(ParseNode):
    __slots__ = ("name",)

    def __init__(self, name: str, location: InputLocation):
        ParseNode.__init__(self, location)
        self.name = name


class FlagTestNode(ParseNode):
    __slots__ = ("name",)

    def __init__(self, name: str, location: InputLocation):
        ParseNode.__init__(self, location)
        self.name = name


class BranchNode(ParseNode):
    __slots__ = ("cond", "target")

    def __init__(self, cond: ParseNode, target: LabelNode, location: InputLocation):
        ParseNode.__init__(self, location)
        self.cond = cond
        self.target = target
        self.tree_location = mergeSpan(location, target.tree_location)


class AssignmentNode(ParseNode):
    __slots__ = ("lhs", "rhs")

    def __init__(self, lhs: ParseNode, rhs: ParseNode, location: InputLocation):
        ParseNode.__init__(self, location)
        self.lhs = lhs
        self.rhs = rhs
        self.tree_location = mergeSpan(lhs.tree_location, rhs.tree_location)

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


class OperatorNode(ParseNode):
    __slots__ = ("operator", "operands")

    def __init__(
        self,
        operator: Operator,
        operands: Iterable[ParseNode | None],
        location: InputLocation,
    ):
        ParseNode.__init__(self, location)
        self.operator = operator
        self.operands: Sequence[ParseNode | None] = tuple(operands)
        self.tree_location = self._tree_location()

    def __iter__(self) -> Iterator[ParseNode]:
        yield self
        for operand in self.operands:
            if operand is not None:
                yield from operand

    def _tree_location(self) -> InputLocation:
        location = self.location
        baseLocation = location.updateSpan((0, 0))
        treeStart, treeEnd = location.span
        for operand in self.operands:
            if operand is None:
                continue
            location = operand.tree_location
            assert location.updateSpan((0, 0)) == baseLocation
            start, end = location.span
            treeStart = min(treeStart, start)
            treeEnd = max(treeEnd, end)
        return baseLocation.updateSpan((treeStart, treeEnd))


class IdentifierNode(ParseNode):
    __slots__ = ("name",)

    def __init__(self, name: str, location: InputLocation):
        ParseNode.__init__(self, location)
        self.name = name


class MultiMatchNode(ParseNode):
    __slots__ = ("name",)

    def __init__(self, name: str, location: InputLocation):
        ParseNode.__init__(self, location)
        self.name = name


class DeclarationKind(Enum):
    variable = auto()
    constant = auto()
    reference = auto()


class DeclarationNode(ParseNode):
    __slots__ = ("kind", "type", "name")

    def __init__(
        self,
        kind: DeclarationKind,
        typ: IdentifierNode | None,
        name: IdentifierNode,
        location: InputLocation,
    ):
        ParseNode.__init__(self, location)
        self.kind = kind
        self.type = typ
        self.name = name
        self.tree_location = mergeSpan(location, name.tree_location)


class DefinitionNode(ParseNode):
    __slots__ = ("decl", "value")

    def __init__(
        self, decl: DeclarationNode, value: ParseNode, location: InputLocation
    ):
        ParseNode.__init__(self, location)
        self.decl = decl
        self.value = value
        self.tree_location = mergeSpan(decl.tree_location, value.tree_location)

    def __iter__(self) -> Iterator[ParseNode]:
        yield self
        yield from self.decl
        yield from self.value


class NumberNode(ParseNode):
    __slots__ = ("value", "width")

    def __init__(self, value: int, width: Width, location: InputLocation):
        ParseNode.__init__(self, location)
        self.value = value
        self.width = width


DefDeclNode = Union[DeclarationNode, DefinitionNode]
ContextNode = Union[DeclarationNode, DefinitionNode, FlagTestNode]


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
