from __future__ import annotations

import re
from collections.abc import Iterator
from contextlib import contextmanager
from importlib.resources.abc import Traversable
from typing import IO, Self

from retroasm.input import InputLocation


class LineReader:
    """
    Iterates through the lines of a text file.
    The lines will not contain a trailing newline character.
    """

    @classmethod
    @contextmanager
    def open(cls, path: Traversable) -> Iterator[Self]:
        with path.open() as lines:
            yield cls(str(path), lines)

    def __init__(self, path: str, lines: IO[str]):
        self._path = path
        self._lines = lines
        self._lastline: str | None = None
        self._lineno = 0

    def __iter__(self) -> Iterator[InputLocation]:
        return self

    def _next_line(self) -> str:
        try:
            line = next(self._lines).rstrip("\n")
        except StopIteration:
            self._lastline = None
            raise
        else:
            self._lastline = line
            self._lineno += 1
            return line

    def __next__(self) -> InputLocation:
        self._next_line()
        return self.location

    @property
    def location(self) -> InputLocation:
        """
        Returns an InputLocation object describing the current line in
        the input file.
        """
        lastline = self._lastline
        if lastline is None:
            lastline = "[end of file]"
            span = (0, 0)
        else:
            span = (0, len(lastline))
        return InputLocation(self._path, self._lineno, lastline, span)


_RE_COMMENT = re.compile(r"(?<!\\)#")


class DefLineReader(LineReader):
    """
    Iterates through lines of a block-structured definition file.
    Trailing whitespace is stripped, comments are removed.
    If a block header is bad, it is possible to skip the parsing of that
    block without aborting parsing altogether, in an attempt to catch
    multiple errors in a single pass.
    """

    def __next__(self) -> InputLocation:
        while True:
            line = self._next_line().rstrip()
            match = _RE_COMMENT.search(line)
            if match is None:
                span = (0, len(line))
            else:
                end = match.start()
                while end > 0 and line[end - 1].isspace():
                    end -= 1
                if end == 0:
                    # Comment lines are ignored rather than returned as empty
                    # lines, such that they don't terminate blocks.
                    continue
                span = (0, end)
            return InputLocation(self._path, self._lineno, line, span)

    def iter_block(self) -> Iterator[InputLocation]:
        """Iterates through the lines of the current block."""
        while True:
            try:
                line = next(self)
            except StopIteration:
                break
            if len(line) == 0:
                break
            yield line

    def skip_block(self) -> None:
        """Skips the remainder of the current block."""
        for _ in self.iter_block():
            pass
