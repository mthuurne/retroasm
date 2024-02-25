from __future__ import annotations

from io import StringIO
from logging import ERROR, Logger, getLogger

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


def test_variable_undefined_basic_compute(
    parser: TestParser, caplog: pytest.LogCaptureFixture
) -> None:
    """
    An error is reported when a variable's undefined value is used in a computation.
    """

    parser.parse_text(
        """
func compute_undef()
    var int UNDEF
    a := a + UNDEF
"""
    )

    assert caplog.record_tuples == [
        (
            "test",
            ERROR,
            'ERROR: error in body of function "compute_undef": '
            'variable "UNDEF" is used before it is initialized',
        ),
    ]


def test_variable_undefined_basic_ioindex(
    parser: TestParser, caplog: pytest.LogCaptureFixture
) -> None:
    """
    An error is reported when a variable's undefined value is used as an I/O index.
    """

    parser.parse_text(
        """
func ioindex_undef()
    var int UNDEF
    a := mem[UNDEF]
"""
    )

    assert caplog.record_tuples == [
        (
            "test",
            ERROR,
            'ERROR: error in body of function "ioindex_undef": '
            'variable "UNDEF" is used before it is initialized',
        ),
    ]


def test_variable_undefined_basic_return(
    parser: TestParser, caplog: pytest.LogCaptureFixture
) -> None:
    """
    An error is reported when a variable's undefined value is returned.
    """

    parser.parse_text(
        """
func int return_undef()
    var int UNDEF
    a := mem[UNDEF]
"""
    )

    assert caplog.record_tuples == [
        (
            "test",
            ERROR,
            'ERROR: error in body of function "return_undef": '
            'variable "UNDEF" is used before it is initialized',
        ),
    ]
