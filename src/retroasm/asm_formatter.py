from __future__ import annotations

from typing import AbstractSet, Iterable, Iterator, Mapping

from .expression import IntLiteral
from .mode import PlaceholderRole
from .reference import FixedValue, Reference
from .types import IntType, Width, unlimited


class Formatter:

    margin = 20
    operationWidth = 8

    def __init__(self) -> None:
        # pylint: disable=consider-using-f-string
        self._lineFormat = "{:%d}{:%d}{}" % (self.margin, self.operationWidth)

    def value(self, value: int, typ: IntType) -> str:
        if value < 16 or typ.signed:
            return str(value)
        else:
            width = typ.width
            if width is unlimited:
                # The type doesn't tell us how wide the presentation
                # should be, so look at the value instead.
                width = value.bit_length()
            else:
                assert isinstance(width, int)
            return f"${{:0{(width + 3) // 4:d}x}}".format(value)

    def _operands(self, operands: Iterable[str]) -> str:
        prevWord = False
        parts = []
        for operand in operands:
            isWord = operand[0].isalnum() or operand[0] in "_$%"
            if prevWord and isWord:
                parts.append(" ")
            parts.append(operand)
            prevWord = isWord
        return "".join(parts)

    def comment(self, comment: str) -> str:
        return f"# {comment}"

    def label(self, label: str) -> str:
        return label + ":"

    def mnemonic(
        self,
        # TODO: Use the Mnemonic class instead?
        mnemonic: Iterable[str | Reference],
        labels: Mapping[int, str],
    ) -> str:
        parts = []
        for mnemElem in mnemonic:
            if isinstance(mnemElem, str):
                parts.append(mnemElem)
            elif isinstance(mnemElem, Reference):
                # TODO: Are these asserts always true? If so, try to change
                #       the argument types to refrect that. If not, make
                #       this method more versatile.
                bits = mnemElem.bits
                assert isinstance(bits, FixedValue)
                expr = bits.expr
                assert isinstance(expr, IntLiteral)
                value = expr.value
                # TODO: Role detection needs to be re-implemented.
                roles: AbstractSet[PlaceholderRole] = frozenset()
                label = (
                    labels.get(value)
                    if PlaceholderRole.code_addr in roles
                    or PlaceholderRole.data_addr in roles
                    else None
                )
                if label is None:
                    parts.append(self.value(value, mnemElem.type))
                else:
                    parts.append(label)
            else:
                assert False, mnemElem

        localLabel = ""
        return self._lineFormat.format(
            localLabel, parts[0], self._operands(parts[1:])
        ).rstrip()

    dataDirectives: Mapping[Width, str] = {8: "db", 16: "dw", 32: "dd", 64: "dq"}

    def data(self, ref: Reference) -> str:
        directive = self.dataDirectives[ref.width]
        return self.mnemonic((directive, ref), {})

    def raw(self, data: bytes) -> Iterator[str]:
        """Format data with no known structure using data directives."""
        directive = self.dataDirectives[8]
        chunkSize = 16
        for offset in range(0, len(data), chunkSize):
            yield self._lineFormat.format(
                "",
                directive,
                ", ".join(f"${byte:02x}" for byte in data[offset : offset + chunkSize]),
            )
