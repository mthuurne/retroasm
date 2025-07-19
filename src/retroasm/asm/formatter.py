from __future__ import annotations

from collections.abc import Iterable, Iterator, Mapping

from ..expression import IntLiteral
from ..parser.expression_nodes import ParseNode
from ..reference import FixedValueReference
from ..symbol import CurrentAddress, SymbolValue
from ..types import IntType, Width, unlimited
from ..utils import bad_type
from .directives import DataDirective, OriginDirective, StringDirective


class Formatter:
    margin = 20
    operation_width = 8

    def __init__(self) -> None:
        self._line_format = "{:%d}{:%d}{}" % (self.margin, self.operation_width)

    def value(self, value: int, typ: IntType) -> str:
        if typ.signed:
            return str(value)
        else:
            width = typ.width
            if width is unlimited:
                # The type doesn't tell us how wide the presentation
                # should be, so look at the value instead.
                if value < 16:
                    return str(value)
                width = value.bit_length()
            else:
                assert isinstance(width, int)
            return self.hex_value(value, width)

    def hex_value(self, value: int, width: int) -> str:
        return f"${{:0{(width + 3) // 4:d}x}}".format(value)

    def hex_range(self, start: int, end: Width, width: int) -> str:
        start_str = self.hex_value(start, width)
        end_str = "" if end is unlimited else self.hex_value(end, width)
        return f"{start_str}-{end_str}"

    def _string_literal(self, string: bytes) -> Iterator[str]:
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
                yield "0" if value == 0 else self.hex_value(value, 8)
                idx += 1

    def _operands(self, operands: Iterable[str]) -> str:
        prev_word = False
        parts = []
        for operand in operands:
            is_word = operand[0].isalnum() or operand[0] in "_$%"
            if prev_word and is_word:
                parts.append(" ")
            parts.append(operand)
            prev_word = is_word
        return "".join(parts)

    def comment(self, comment: str) -> str:
        return f"; {comment}"

    def label(self, label: str) -> str:
        return label + ":"

    def mnemonic(self, mnemonic: Iterable[str | FixedValueReference | ParseNode]) -> str:
        parts = []
        for mnem_elem in mnemonic:
            match mnem_elem:
                case str() as text:
                    parts.append(text)
                case FixedValueReference() as ref:
                    parts.append(self.expression(ref))
                    # Shorten "X+-N" to just "X-N".
                    if len(parts) >= 2 and parts[-1].startswith("-") and parts[-2] == "+":
                        del parts[-2]
                case ParseNode() as node:
                    # TODO: Proper formatting.
                    parts.append(str(node))
                case elem:
                    bad_type(elem)

        local_label = ""
        return self._line_format.format(
            local_label, parts[0], self._operands(parts[1:])
        ).rstrip()

    def expression(self, ref: FixedValueReference) -> str:
        """
        Format the given expression.
        """
        match ref.expr:
            case SymbolValue(name=name):
                return name
            case CurrentAddress():
                return "$"
            case IntLiteral(value=value):
                return self.value(value, ref.type)
            case expr:
                # TODO: Use a recursive expression pretty printer.
                #       Once we start formatting actual sources instead of only
                #       disassembly, we will encounter operators as well.
                raise NotImplementedError(expr)

    org_keyword = "org"

    def origin(self, directive: OriginDirective) -> str:
        return self.mnemonic((self.org_keyword, directive.addr))

    data_keywords: Mapping[Width, str] = {8: "db", 16: "dw", 32: "dd", 64: "dq"}

    def data(self, directive: DataDirective | StringDirective) -> str:
        keyword = self.data_keywords[
            directive.width if isinstance(directive, DataDirective) else 8
        ]
        words: list[str | ParseNode] = [keyword]
        for data in directive.data:
            items: Iterable[str | ParseNode]
            if isinstance(data, bytes):
                items = self._string_literal(data)
            else:
                items = (data,)
            for item in items:
                if len(words) > 1:
                    words.append(", ")
                words.append(item)
        return self.mnemonic(words)

    def raw(self, data: bytes) -> Iterator[str]:
        """Format data with no known structure using data directives."""
        directive = self.data_keywords[8]
        chunk_size = 16
        for offset in range(0, len(data), chunk_size):
            yield self._line_format.format(
                "",
                directive,
                ", ".join(
                    self.hex_value(byte, 8) for byte in data[offset : offset + chunk_size]
                ),
            )
