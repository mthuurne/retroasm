from __future__ import annotations

from collections.abc import Iterator
from logging import ERROR, INFO, getLogger
from pathlib import Path

import pytest

from retroasm.instr import load_instruction_set


@pytest.fixture
def empty_file(tmp_path: Path) -> Iterator[Path]:
    path = tmp_path / "empty.instr"
    with path.open("w"):
        pass
    yield path


minimal_instr = r"""
reg
int pc

instr
$00 . nop
"""


@pytest.fixture
def minimal_file(tmp_path: Path) -> Iterator[Path]:
    path = tmp_path / "minimal.instr"
    with path.open("w") as out:
        out.write(minimal_instr)
    yield path


def test_load_instr_nofile(tmp_path: Path, caplog: pytest.LogCaptureFixture) -> None:
    """
    Attempting to load a non-existing instruction set file fails gracefully
    and logs the error.
    """
    path = tmp_path / "nosuchfile.instr"
    logger = getLogger("test")
    instr = load_instruction_set(path, logger)
    assert caplog.record_tuples == [
        (
            "test",
            ERROR,
            f"{path}: Failed to read instruction set: No such file or directory",
        ),
    ]
    assert instr is None


def test_load_instr_empty(empty_file: Path, caplog: pytest.LogCaptureFixture) -> None:
    """
    Attempting to load an empty instruction set file fails gracefully
    and logs the error.
    """
    logger = getLogger("test")
    instr = load_instruction_set(empty_file, logger)
    assert caplog.record_tuples == [
        (
            "test.parser",
            ERROR,
            "ERROR: no program counter defined: "
            'a register or alias named "pc" is required',
        ),
        (
            "test.parser",
            ERROR,
            "ERROR: no instruction encodings defined",
        ),
        (
            "test.parser",
            ERROR,
            f"{empty_file}: 2 errors and 0 warnings",
        ),
    ]
    assert instr is None


def test_load_instr_minimal(
    minimal_file: Path, caplog: pytest.LogCaptureFixture
) -> None:
    """
    Loading a minimal instruction set file succeeds and logs nothing at INFO level.
    """
    logger = getLogger("test")
    logger.setLevel(INFO)
    instr = load_instruction_set(minimal_file, logger)
    assert caplog.record_tuples == []
    assert instr is not None
