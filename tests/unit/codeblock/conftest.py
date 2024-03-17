from __future__ import annotations

from collections.abc import Sequence
from dataclasses import dataclass
from inspect import cleandoc
from io import StringIO
from logging import Logger, getLevelNamesMapping, getLogger

import pytest

from retroasm.input import InputLogger
from retroasm.parser.instrset_parser import InstructionSetParser
from retroasm.parser.linereader import DefLineReader


class TestParser(InstructionSetParser):
    def __init__(self, path: str, logger: Logger) -> None:
        super().__init__()
        self.path = path
        self.logger = logger

    def parse_text(self, text: str) -> None:
        reader = DefLineReader(self.path, StringIO(text))
        logger = InputLogger(self.path, self.logger)
        self.parse(reader, logger)


@pytest.fixture
def parser() -> TestParser:
    """
    An instruction set parser with a few registers and an I/O channel predefined.
    """

    logger = getLogger("test")
    parser = TestParser("test.instr", logger)
    parser.parse_text(
        """
reg
u32 a, b
u1 f
u32 sp, pc

io
u32 mem[u32]
"""
    )
    return parser


def _parse_docstring(docstring: str) -> tuple[str, str]:
    """Parse a code block and list of logging messages from the given docstring."""

    lines = cleandoc(docstring).split("\n")
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

    blocks = {}
    idx = 0
    while block := find_block(idx):
        idx, language, body = block
        if language in blocks:
            raise ValueError(f'Multiple blocks with language "{language}"')
        blocks[language] = body

    try:
        code = blocks["instr"]
    except KeyError:
        raise ValueError("No code block") from None

    logging = blocks.get("inputlog", "")

    return code, logging


@dataclass
class DocstringTester:
    parser: TestParser
    expected_log: Sequence[tuple[str, int, str]]
    actual_log: Sequence[tuple[str, int, str]]

    def check(self) -> None:
        assert self.actual_log == self.expected_log


@pytest.fixture
def docstring_tester(
    parser: TestParser, request: pytest.FixtureRequest, caplog: pytest.LogCaptureFixture
) -> DocstringTester:
    """
    Fixture that parses a code block and list of logging messages from the requesting
    test function's docstring.

    The fixture will parse the code block and check whether the actual log messages
    match the ones listed in the docstring.
    """

    func_node: pytest.Function = request.node
    docstring = func_node.function.__doc__
    if docstring is None:
        raise pytest.FixtureLookupError(
            request.fixturename, request, "missing docstring"
        )
    try:
        code, logging = _parse_docstring(docstring)
    except ValueError as ex:
        raise pytest.FixtureLookupError(
            request.fixturename, request, f"error parsing docstring: {ex}"
        ) from ex

    print("> code:")
    print(code)
    parser.parse_text(code)

    expected_log = []
    if logging:
        print("> logging:")
        print(logging)
        levels_by_name = getLevelNamesMapping()
        logger_name = parser.logger.name
        for line in logging.split("\n"):
            level_name, message = line.split(":", 1)
            level = levels_by_name[level_name.strip().upper()]
            expected_log.append((logger_name, level, message.strip()))

    return DocstringTester(parser, expected_log, caplog.record_tuples)
