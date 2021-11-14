from __future__ import annotations

from typing import Iterable, Iterator, Mapping, cast

from .asm_directives import DataDirective, OriginDirective, StringDirective
from .expression import Expression, IntLiteral
from .reference import FixedValue, Reference
from .symbol import SymbolValue
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
            return self.hexValue(value, width)

    def hexValue(self, value: int, width: int) -> str:
        return f"${{:0{(width + 3) // 4:d}x}}".format(value)

    def hexRange(self, start: int, end: Width, width: int) -> str:
        startStr = self.hexValue(start, width)
        endStr = "" if end is unlimited else self.hexValue(cast(int, end), width)
        return f"{startStr}-{endStr}"

    def _stringLiteral(self, string: bytes) -> Iterator[str]:
        idx = 0
        end = len(string)
        while idx < end:
            start = idx
            while idx < end and 32 <= string[idx] < 127 and string[idx] != ord('"'):
                idx += 1
            if idx != start:
                yield f'"{string[start:idx].decode()}"'
            if idx < end:
                value = string[idx]
                yield "0" if value == 0 else self.hexValue(value, 8)
                idx += 1

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
        return f"; {comment}"

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
                # TODO: Is 'bits' always a FixedValue?
                #       If so, try to change the types to reflect that.
                #       If not, make this method more versatile.
                bits = mnemElem.bits
                assert isinstance(bits, FixedValue)
                parts.append(self.expression(bits.expr, mnemElem.type, labels))
                # Shorten "X+-N" to just "X-N".
                if len(parts) >= 2 and parts[-1].startswith("-") and parts[-2] == "+":
                    del parts[-2]
            else:
                assert False, mnemElem

        localLabel = ""
        return self._lineFormat.format(
            localLabel, parts[0], self._operands(parts[1:])
        ).rstrip()

    def expression(
        self, expr: Expression, exprType: IntType, labels: Mapping[int, str]
    ) -> str:
        """
        Format the given expression.
        TODO: Use a recursive expression pretty printer.
              Once we start formatting actual sources instead of
              only disassembly, we will encounter operators as well.
        """
        if isinstance(expr, SymbolValue):
            return expr.name
        elif isinstance(expr, IntLiteral):
            value = expr.value
            width = exprType.width
            if exprType.signed:
                if width != 0 and width is not unlimited:
                    assert isinstance(width, int)
                    if value & 1 << (width - 1):
                        value -= 1 << width
            return self.value(value, exprType)
        else:
            assert False, expr

    orgKeyword = "org"

    def origin(self, directive: OriginDirective) -> str:
        return self.mnemonic((self.orgKeyword, directive.addr), {})

    dataKeywords: Mapping[Width, str] = {8: "db", 16: "dw", 32: "dd", 64: "dq"}

    def data(self, directive: DataDirective | StringDirective) -> str:
        keyword = self.dataKeywords[
            directive.width if isinstance(directive, DataDirective) else 8
        ]
        words: list[str | Reference] = [keyword]
        for data in directive.data:
            items: Iterable[str | Reference]
            if isinstance(data, bytes):
                items = self._stringLiteral(data)
            else:
                items = (data,)
            for item in items:
                if len(words) > 1:
                    words.append(", ")
                words.append(item)
        return self.mnemonic(words, {})

    def raw(self, data: bytes) -> Iterator[str]:
        """Format data with no known structure using data directives."""
        directive = self.dataKeywords[8]
        chunkSize = 16
        for offset in range(0, len(data), chunkSize):
            yield self._lineFormat.format(
                "",
                directive,
                ", ".join(
                    self.hexValue(byte, 8) for byte in data[offset : offset + chunkSize]
                ),
            )
