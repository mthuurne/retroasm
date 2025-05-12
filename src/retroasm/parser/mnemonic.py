"""
Parser for mnemonic column in instruction set definition.
"""

from __future__ import annotations

import re
from collections.abc import Iterable, Iterator

from ..input import (
    ErrorCollector,
    InputLocation,
)
from ..mode import (
    MatchPlaceholder,
    MnemItem,
    ValuePlaceholder,
)
from ..reference import int_reference
from ..types import IntType
from .expression_nodes import parse_int

_re_mnemonic = re.compile(r"\w+'?|[$%]\w+|[^\w\s]")


def parse_mnemonic(
    mnem_loc: InputLocation,
    placeholders: Iterable[MatchPlaceholder | ValuePlaceholder],
    collector: ErrorCollector,
) -> Iterator[MnemItem]:
    placeholder_map = {p.name: p for p in placeholders}
    seen_placeholders: dict[str, InputLocation] = {}
    for mnem_elem in mnem_loc.find_locations(_re_mnemonic):
        text = mnem_elem.text
        placeholder = placeholder_map.get(text)
        if placeholder is None:
            if "0" <= text[0] <= "9" or text[0] in "$%":
                try:
                    value, width = parse_int(text)
                except ValueError as ex:
                    collector.error(f"{ex}", location=mnem_elem)
                else:
                    yield int_reference(value, IntType.u(width))
            else:
                yield text
        elif text in seen_placeholders:
            # In theory we could support repeated placeholders, but the only
            # meaning that would make sense is that they would all match the
            # same mode entry or expression and I don't know of any situation
            # in which that would be a useful feature.
            collector.error(
                f'placeholder "{text}" occurs multiple times in mnemonic',
                location=(mnem_elem, seen_placeholders[text]),
            )
        else:
            yield placeholder
            seen_placeholders[text] = mnem_elem
