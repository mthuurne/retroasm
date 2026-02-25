from __future__ import annotations

from collections.abc import Iterator
from dataclasses import dataclass
from io import StringIO
from logging import Logger, getLogger
from textwrap import dedent
from typing import Literal, overload

import pytest

from retroasm.function import Function
from retroasm.input import ErrorCollector, LocationFormatter
from retroasm.instrset import InstructionSet
from retroasm.parser.instrset_parser import InstructionSetParser
from retroasm.parser.linereader import DefLineReader

from ..docstring import unpack_docstring
from ..utils_pytest import get_flag_marker


class TestParser(InstructionSetParser):
    def __init__(self, path: str, logger: Logger) -> None:
        super().__init__()
        self.path = path
        self.logger = logger

    def parse_text(self, text: str) -> None:
        reader = DefLineReader(self.path, StringIO(text))
        collector = ErrorCollector(self.logger)
        self.parse(reader, collector)


@pytest.fixture
def parser() -> TestParser:
    """
    An instruction set parser.
    """

    logger = getLogger("test")
    return TestParser("test.instr", logger)


@dataclass
class InstructionSetDocstringTester:
    parser: TestParser
    expected_log: str
    actual_log: str

    def check(self) -> None:
        assert self.actual_log == self.expected_log

    @overload
    def create_instruction_set(self, expect_fail: Literal[False] = False) -> InstructionSet: ...

    @overload
    def create_instruction_set(self, expect_fail: Literal[True]) -> None: ...

    def create_instruction_set(self, expect_fail: bool = False) -> InstructionSet | None:
        parser = self.parser
        collector = ErrorCollector(parser.logger)
        instruction_set = parser.finalize(collector, None)
        assert (instruction_set is None) == expect_fail
        return instruction_set


_default_regs_definition = """
reg
u32 a, b
u8 h, l
u16& hl = h;l
u1 f
u32 sp, pc

mode u32& reg32
%00                 . a
%01                 . b
%10                 . sp
%11                 . pc
"""

_default_io_definition = """
io
u32 mem[u32]
"""

_default_instr_definition = """
instr nop
$00000000 . nop . nop
"""


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
    print("> code:")
    print(code)

    if logs:
        (logging,) = logs
    else:
        logging = ""

    caplog.handler.setFormatter(LocationFormatter())

    # Include default register definitions?
    use_default_regs = get_flag_marker(request, "default_regs", None)
    use_default_io = get_flag_marker(request, "default_io", None)
    use_default_instr = get_flag_marker(request, "default_instr", None)

    if use_default_regs is None or use_default_io is None or use_default_instr is None:
        blocks = set(_iter_block_names(code))
        if use_default_regs is None:
            use_default_regs = "reg" not in blocks
        if use_default_io is None:
            use_default_io = "io" not in blocks
        if use_default_instr is None:
            use_default_instr = "instr" not in blocks

    if use_default_regs:
        parser.parse_text(_default_regs_definition)
    if use_default_io:
        parser.parse_text(_default_io_definition)
    if use_default_instr:
        parser.parse_text(_default_instr_definition)

    parser.parse_text(code)

    return InstructionSetDocstringTester(parser, logging, caplog.text)


def _iter_block_names(text: str) -> Iterator[str]:
    """Yield the names of the blocks in the given definition text."""
    reader = DefLineReader("<dummy>", StringIO(text))
    for line in reader:
        yield line.text.split()[0]
        reader.skip_block()


@dataclass
class CodeBlockDocstringTester:
    parser: TestParser
    actual_dump: str
    expected_dump: str

    def check(self) -> None:
        assert self.actual_dump == self.expected_dump


@pytest.fixture
def codeblock_tester(
    parser: TestParser,
    docstring: str,
    request: pytest.FixtureRequest,
) -> CodeBlockDocstringTester:
    """
    Fixture that parses a source code fragment and a code block dump from the requesting
    test function's docstring.

    The fixture will parse the source code and look up a function named "test".
    It will then check whether the dump of that function's code block matches the dump
    from the docstring.
    """

    try:
        code, expected_dump = unpack_docstring(docstring, "instr", "dump")
    except ValueError as ex:
        raise pytest.FixtureLookupError(
            request.fixturename, request, f"error unpacking docstring: {ex}"
        ) from ex

    parser.parse_text(_default_regs_definition)
    parser.parse_text(_default_io_definition)
    parser.parse_text(code)

    try:
        func = parser.global_namespace["test"]
    except KeyError as ex:
        raise pytest.FixtureLookupError(
            request.fixturename, request, 'no function named "test" defined in docstring'
        ) from ex
    if not isinstance(func, Function):
        raise pytest.FixtureLookupError(
            request.fixturename, request, '"test" defined in docstring is not a function'
        )

    if func.code is None:
        raise pytest.FixtureLookupError(
            request.fixturename, request, 'error in "test" function; check log'
        )

    buffer = StringIO()
    func.code.dump(file=buffer)
    actual_dump = dedent(buffer.getvalue())

    return CodeBlockDocstringTester(parser, actual_dump, expected_dump)
