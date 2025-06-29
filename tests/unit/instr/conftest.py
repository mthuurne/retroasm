from __future__ import annotations

from dataclasses import dataclass
from io import StringIO
from logging import Logger, getLogger

import pytest

from retroasm.input import ErrorCollector, LocationFormatter
from retroasm.parser.instrset_parser import InstructionSetParser
from retroasm.parser.linereader import DefLineReader

from ..docstring import unpack_docstring


class TestParser(InstructionSetParser):
    def __init__(self, path: str, logger: Logger) -> None:
        super().__init__()
        self.path = path
        self.logger = logger

    def parse_text(self, text: str) -> None:
        reader = DefLineReader(self.path, StringIO(text))
        logger = ErrorCollector(self.logger)
        self.parse(reader, logger)


_default_regs_definition = """
reg
u32 a, b
u1 f
u32 sp, pc

io
u32 mem[u32]
"""


@pytest.fixture
def parser(request: pytest.FixtureRequest) -> TestParser:
    """
    An instruction set parser with optionally a few registers and an I/O channel
    predefined.
    """

    logger = getLogger("test")
    parser = TestParser("test.instr", logger)

    # Include default register definitions?
    if marker := request.node.get_closest_marker("default_regs"):
        (default_regs,) = marker.args
    else:
        default_regs = True
    if default_regs:
        parser.parse_text(_default_regs_definition)

    return parser


@dataclass
class InstructionSetDocstringTester:
    parser: TestParser
    expected_log: str
    actual_log: str

    def check(self) -> None:
        assert self.actual_log == self.expected_log


@pytest.fixture
def instr_tester(
    parser: TestParser,
    docstring: str,
    request: pytest.FixtureRequest,
    caplog: pytest.LogCaptureFixture,
) -> InstructionSetDocstringTester:
    """
    Fixture that parses a code block and list of logging messages from the requesting
    test function's docstring.

    The fixture will parse the code block and check whether the actual log messages
    match the ones listed in the docstring.
    """

    try:
        code, *logs = unpack_docstring(docstring, "instr", opt="inputlog")
    except ValueError as ex:
        raise pytest.FixtureLookupError(
            request.fixturename, request, f"error unpacking docstring: {ex}"
        ) from ex

    if logs:
        (logging,) = logs
    else:
        logging = ""

    caplog.handler.setFormatter(LocationFormatter())

    print("> code:")
    print(code)
    parser.parse_text(code)

    return InstructionSetDocstringTester(parser, logging, caplog.text.rstrip())
