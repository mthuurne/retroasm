from __future__ import annotations

from collections.abc import Iterator
from inspect import cleandoc


def parse_docstring(docstring: str) -> Iterator[tuple[str, str]]:
    """
    Iterate through the codeblocks in the given clean docstring.

    Yields pairs containing the language and the block body.
    """

    lines = docstring.split("\n")
    num_lines = len(lines)

    def find_block(search_start: int) -> tuple[int, str, str] | None:
        prefix = ".. code-block::"
        for idx in range(search_start, num_lines):
            if (line := lines[idx]).startswith(prefix):
                language = line[len(prefix) :].strip()
                block_start = idx + 1
                break
        else:
            return None

        for idx in range(block_start, num_lines):
            if (line := lines[idx]) and not line[0].isspace():
                block_end = idx
                break
        else:
            block_end = num_lines

        body = cleandoc("\n".join(lines[block_start:block_end]))
        return block_end, language, body

    idx = 0
    while block := find_block(idx):
        idx, language, body = block
        yield language, body
