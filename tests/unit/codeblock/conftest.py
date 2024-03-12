from __future__ import annotations

from collections.abc import Sequence
from dataclasses import dataclass
from inspect import cleandoc
from io import StringIO
from logging import Logger, getLevelNamesMapping, getLogger

import pytest

from retroasm.parser.instrset_parser import InstructionSetParser
from retroasm.parser.linereader import DefLineReader


class TestParser(InstructionSetParser):
    def __init__(self, path: str, logger: Logger) -> None:
        super().__init__()
        self.path = path
        self.logger = logger

    def parse_text(self, text: str) -> None:
        reader = DefLineReader(self.path, StringIO(text), self.logger)
        self.parse(reader)


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


def _parse_docstring(docstring: str) -> tuple[str, Sequence[str]]:
    """Parse a code block and list of logging messages from the given docstring."""

    lines = cleandoc(docstring).split("\n")

    code_start = lines.index(".. code-block:: instr") + 1
    for code_end in range(code_start, len(lines)):
        if (line := lines[code_end]) and not line[0].isspace():
            break
    else:
        code_end = len(lines)
    code = cleandoc("\n".join(lines[code_start:code_end]))

    logging = []
    for idx in range(code_end, len(lines)):
        line = lines[idx]
        if not line:
            continue
        elif line[0] == "-":
            logging.append(line[1:].strip())
        elif line[0].isspace() and logging:
            logging[-1] += " " + line.strip()

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

    print(code)
    parser.parse_text(code)

    levels_by_name = getLevelNamesMapping()
    logger_name = parser.logger.name
    expected_log = []
    for message in logging:
        level = levels_by_name[message[: message.index(":")].upper()]
        expected_log.append((logger_name, level, message))

    return DocstringTester(parser, expected_log, caplog.record_tuples)
