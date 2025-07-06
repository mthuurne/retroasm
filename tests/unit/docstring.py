from __future__ import annotations

from collections.abc import Iterator
from inspect import cleandoc
from itertools import chain, repeat


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


def unpack_docstring(docstring: str, *req_langs: str, opt: str | None = None) -> Iterator[str]:
    """
    Parse a docstring and unpack the code blocks according to their language.

    Positional arguments define required languages:
    these must exist in the given order from the start of the docstring.
    The `opt_lang` keyword argument defines an optional language:
    blocks of this language may exist in any amount after the required languages.

    Yields required blocks in the specified order, followed by any optional blocks.
    Raises `ValueError` if the blocks found don't match the specified languages.
    """

    parsed = parse_docstring(docstring)
    langspec = chain(req_langs, repeat(opt))
    idx = 0
    for idx, ((lang, body), exp_lang) in enumerate(zip(parsed, langspec), start=1):
        if lang == exp_lang:
            yield body
        else:
            raise ValueError(f'block {idx:d} has language "{lang}", expected "{exp_lang}"')
    if idx < len(req_langs):
        raise ValueError(f"only {idx:d} of {len(req_langs)} required blocks found")
