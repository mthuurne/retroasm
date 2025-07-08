"""
Parser for mnemonic column in instruction set definition.
"""

from __future__ import annotations

from collections.abc import Iterator

from ..input import ErrorCollector, InputLocation
from ..mode import MatchPlaceholder, MnemItem, ValuePlaceholder
from ..reference import int_reference
from ..types import IntType
from ..utils import bad_type
from .expression_nodes import parse_int
from .tokens import TokenEnum, Tokenizer


class MnemonicToken(TokenEnum):
    identifier = r"[A-Za-z_][A-Za-z0-9_]*'?"
    number = r"[%$0-9]\w*"
    operator = r"\\?[+\-*/&|^=<>@#]+"
    bracket = r"[\[\](){}]"
    separator = r","
    other = r".+?"


class MnemonicTokenizer(Tokenizer[MnemonicToken]):
    pass


def parse_mnemonic(
    tokens: MnemonicTokenizer,
    placeholders: dict[str, MatchPlaceholder | ValuePlaceholder],
    collector: ErrorCollector,
) -> Iterator[MnemItem]:
    seen_placeholders: dict[str, InputLocation] = {}

    int_literal_counter = 0
    for token_type, token_loc in tokens:
        text = token_loc.text
        match token_type:
            case MnemonicToken.identifier:
                placeholder = placeholders.get(text)
                if placeholder is None:
                    yield text
                elif text in seen_placeholders:
                    # In theory we could support repeated placeholders, but the only
                    # meaning that would make sense is that they would all match the
                    # same mode entry or expression and I don't know of any situation
                    # in which that would be a useful feature.
                    collector.error(
                        f'placeholder "{text}" occurs multiple times in mnemonic',
                        location=(token_loc, seen_placeholders[text]),
                    )
                else:
                    yield placeholder
                    seen_placeholders[text] = token_loc
            case MnemonicToken.number:
                try:
                    value, width = parse_int(text)
                except ValueError as ex:
                    collector.error(f"{ex}", location=token_loc)
                else:
                    # Create a synthetic placeholder for integer literal.
                    name = f"#{int_literal_counter}"
                    int_literal_counter += 1
                    placeholder = ValuePlaceholder(
                        name, int_reference(value, IntType.u(width)), token_loc
                    )
                    placeholders[name] = placeholder
                    yield placeholder
            case MnemonicToken.operator:
                yield text.lstrip("\\")
            case MnemonicToken.bracket:
                yield text
            case MnemonicToken.separator:
                yield text
            case MnemonicToken.other:
                collector.error(f"invalid token: {text}", location=token_loc)
            case _:
                bad_type(token_type)
