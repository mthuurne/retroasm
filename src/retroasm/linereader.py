from __future__ import annotations

from collections.abc import Iterable, Iterator, Sequence
from contextlib import contextmanager
from dataclasses import dataclass
from importlib.abc import Traversable
from logging import DEBUG, ERROR, INFO, WARNING, Formatter, LogRecord, Logger
from re import Match, Pattern
from typing import IO, Any, TypeVar, Union, cast
import re


@dataclass(frozen=True, slots=True)
class InputLocation:
    """
    Describes a particular location in an input text file.
    This can be used to provide context in error reporting, by passing it as
    the value of the 'location' argument to the log methods of LineReader.
    """

    path: Traversable
    """
    The path to the input text file.

    This path might not exist on the file system, for example when reading from
    a resource.
    """

    lineno: int
    """
    The number of this location's line in the input file,
    where line 1 is the first line.
    """

    line: str
    """The contents of the input line that this location describes."""

    span: tuple[int, int]
    """
    The column span information of this location.

    This acts as a typical Python slice: first column index is zero, start index is
    inclusive start and end index is exclusive.
    """

    def __len__(self) -> int:
        start, end = self.span
        return end - start

    def update_span(self, span: tuple[int, int]) -> InputLocation:
        """
        Adds or updates the column span information of a location.
        Returns an updated location object; the original is unmodified.
        """
        return InputLocation(self.path, self.lineno, self.line, span)

    @property
    def end_location(self) -> InputLocation:
        """A zero-length location marking the end point of this location."""
        end = self.span[1]
        return self.update_span((end, end))

    @property
    def text(self) -> str:
        """The text described by this location: the spanned substring."""
        return self.line[slice(*self.span)]

    def match(self, pattern: Pattern[str]) -> InputMatch | None:
        """
        Matches the text in this location to the given compiled regular
        expression pattern.
        Returns an InputMatch object, or None if no match was found.
        """
        match = pattern.match(self.line, *self.span)
        return None if match is None else InputMatch(self, match)

    def find_locations(self, pattern: Pattern[str]) -> Iterator[InputLocation]:
        """
        Searches the text in this location for the given compiled regular
        expression pattern.
        Returns an iterator that yields an InputLocation object for each
        match.
        """
        for match in pattern.finditer(self.line, *self.span):
            yield self.update_span(match.span(0))

    def find_matches(self, pattern: Pattern[str]) -> Iterator[InputMatch]:
        """
        Searches the text in this location for the given compiled regular
        expression pattern.
        Returns an iterator that yields an InputMatch object for each
        match.
        """
        for match in pattern.finditer(self.line, *self.span):
            yield InputMatch(self, match)

    def split(self, pattern: Pattern[str]) -> Iterator[InputLocation]:
        """
        Splits the text in this location using the given pattern as a
        separator.
        Returns an iterator yielding InputLocations representing the text
        between the separators.
        """
        search_start, search_end = self.span
        curr = search_start
        for match in pattern.finditer(self.line, search_start, search_end):
            sep_start, sep_end = match.span(0)
            yield self.update_span((curr, sep_start))
            curr = sep_end
        yield self.update_span((curr, search_end))


class InputMatch:
    """
    The result of a regular expression operation on an InputLocation.
    The interface is inspired by, but not equal to, that of match objects from
    the "re" module of the standard library.
    """

    __slots__ = ("_location", "_match")

    def __init__(self, location: InputLocation, match: Match[str]):
        self._location = location
        self._match = match

    def has_group(self, index: int | str) -> bool:
        """
        Returns `True` iff a group matched at the given index,
        which can be name or a numeric index with the first group being 1.
        """
        return self._match.span(index) != (-1, -1)

    def group(self, index: int | str) -> InputLocation:
        """
        Returns an InputLocation for the group matched at the given index,
        which can be name or a numeric index with the first group being 1.
        If 0 as passed as the index, an InputLocation for the entire matched
        string is returned.
        If the group did not participate in the match, ValueError is raised.
        """
        span = self._match.span(index)
        if span == (-1, -1):
            name = f"{index}" if isinstance(index, int) else f'"{index}"'
            raise ValueError(f"group {name} was not part of the match")
        else:
            return self._location.update_span(span)

    @property
    def groups(self) -> Iterator[InputLocation]:
        """
        Iterates through the InputLocations of each of the groups matched.
        If a group did not participate in the match, ValueError is raised.
        """
        for i in range(self._match.re.groups):
            yield self.group(i + 1)

    @property
    def group_name(self) -> str | None:
        """
        The name of the last matched group, or None if last matched group
        was nameless or no groups were matched.
        """
        return self._match.lastgroup


def merge_span(
    from_location: InputLocation, to_location: InputLocation
) -> InputLocation:
    """
    Returns a new location of which the span starts at the start of the
    given 'from' location and ends at the end of the given 'to' location.
    Both given locations must be on the same line.
    """
    merged_span = (from_location.span[0], to_location.span[1])
    merged_location = from_location.update_span(merged_span)
    assert merged_location == to_location.update_span(merged_span), (
        from_location,
        to_location,
    )
    return merged_location


class DelayedError(Exception):
    """
    Raised when one or more errors were encountered when processing input.
    Since we want to report as many errors as possible in each processing,
    errors are logged and processing continues. However, it usually doesn't
    make sense to continue with later processing steps, since the incomplete
    output caused by earlier errors would trigger new errors. Therefore
    at the end of a processing step DelayedError can be raised to abort
    processing.
    """


class BadInput(Exception):
    """
    An exception which contains information about the part of the input
    file which is considered to violate a rule.
    The 'location' attribute contains an InputLocation object describing
    the location in the input file that triggered the exception, or None if
    this information is not available.
    """

    @classmethod
    def with_text(cls, msg: str, location: InputLocation) -> BadInput:
        """
        Returns an instance of the BadInput (sub)class it is called on,
        with the input text in the location's span appended after the error
        message.
        """
        return cls(f"{msg}: {location.text}", location)

    def __init__(self, msg: str, *locations: InputLocation):
        Exception.__init__(self, msg)
        self.locations = locations


LineReaderT = TypeVar("LineReaderT", bound="LineReader")


class LineReader:
    """
    Iterates through the lines of a text file.
    The lines will not contain a trailing newline character.
    Log methods on the reader can be used to produce log records with context
    information: path name and the number and contents of the current line.
    Errors and warnings reported in this way are counted.
    The companion class LineReaderFormatter can be used to incorporate the
    context information in the logging.
    """

    @classmethod
    @contextmanager
    def open(
        cls: type[LineReaderT], path: Traversable, logger: Logger
    ) -> Iterator[LineReaderT]:
        with path.open() as lines:
            reader = cls(path, lines, logger)
            reader.debug("start reading")
            yield reader
            reader.debug("done reading")

    def __init__(self, path: Traversable, lines: IO[str], logger: Logger):
        self._path = path
        self._lines = lines
        self.logger = logger

        self._lastline: str | None = None
        self._lineno = 0
        self.warnings = 0
        self.errors = 0

    def __iter__(self) -> Iterator[InputLocation]:
        return self

    def _nextLine(self) -> str:
        self._lastline = None  # in case next() raises StopIteration
        line = next(self._lines).rstrip("\n")
        self._lastline = line
        self._lineno += 1
        return line

    def __next__(self) -> InputLocation:
        self._nextLine()
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

    def debug(self, msg: str, *args: object, **kwargs: object) -> None:
        """Log a message at the DEBUG level."""
        self.__log(DEBUG, msg, *args, **kwargs)

    def info(self, msg: str, *args: object, **kwargs: object) -> None:
        """Log a message at the INFO level."""
        self.__log(INFO, msg, *args, **kwargs)

    def warning(self, msg: str, *args: object, **kwargs: object) -> None:
        """Log a message at the WARNING level and increase the warning count."""
        self.warnings += 1
        self.__log(WARNING, "warning: " + msg, *args, **kwargs)

    def error(self, msg: str, *args: object, **kwargs: object) -> None:
        """Log a message at the ERROR level and increase the error count."""
        self.errors += 1
        self.__log(ERROR, "ERROR: " + msg, *args, **kwargs)

    @contextmanager
    def checkErrors(self) -> Iterator[LineReader]:
        """
        Returns a context manager that raises DelayedError on context close
        if any errors were logged since the context was opened.
        """
        errorsBefore = self.errors
        yield self
        numErrors = self.errors - errorsBefore
        if numErrors != 0:
            raise DelayedError(f"{numErrors:d} errors were logged")

    def summarize(self) -> None:
        """Log a message containing the error and warning counts."""
        level = ERROR if self.errors > 0 else (WARNING if self.warnings > 0 else INFO)
        msg = (
            f"{_pluralize(self.errors, 'error')} and "
            f"{_pluralize(self.warnings, 'warning')}"
        )
        self.__log(level, msg, location=None)

    def __log(self, level: int, msg: str, *args: Any, **kwargs: Any) -> None:
        logger = self.logger
        if logger.isEnabledFor(level):
            try:
                location = cast(
                    Union[None, InputLocation, Sequence[InputLocation]],
                    kwargs.pop("location"),
                )
            except KeyError:
                location = self.location
            extra = dict(location=location)
            logger.log(level, msg, *args, extra=extra, **kwargs)


_reComment = re.compile(r"(?<!\\)#")


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
            line = self._nextLine().rstrip()
            match = _reComment.search(line)
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

    def iterBlock(self) -> Iterator[InputLocation]:
        """Iterates through the lines of the current block."""
        while True:
            try:
                line = next(self)
            except StopIteration:
                break
            if len(line) == 0:
                break
            yield line

    def skipBlock(self) -> None:
        """Skips the remainder of the current block."""
        for _ in self.iterBlock():
            pass


def _pluralize(count: int, verb: str) -> str:
    return f"{count:d} {verb}{'' if count == 1 else 's'}"


class LineReaderFormatter(Formatter):
    def format(self, record: LogRecord) -> str:
        msg = super().format(record)
        location = cast(
            Union[None, InputLocation, Sequence[InputLocation]],
            getattr(record, "location", None),
        )
        return "\n".join(_formatParts(_iterParts(msg, location)))


def _formatParts(
    parts: Iterable[
        tuple[
            str | None, Traversable | None, int, str | None, Sequence[tuple[int, int]]
        ]
    ],
) -> Iterator[str]:
    for msg, path, lineno, line, spans in parts:
        if path is None:
            assert msg is not None
            yield msg
        elif msg is None:
            yield f"{path}:{lineno:d}:"
        else:
            yield f"{path}:{lineno:d}: {msg}"

        if line is not None:
            yield line

            length = len(line) + 1
            spanLine = " " * length
            last = len(spans) - 1
            for i, span in enumerate(reversed(spans)):
                start, end = span
                start = min(start, length)
                end = min(end, length)
                if start > end:
                    continue
                elif start == end:
                    # Highlight empty span using single character.
                    end = start + 1
                highlight = ("^" if i == last else "~") * (end - start)
                spanLine = spanLine[:start] + highlight + spanLine[end:]
            spanLine = spanLine.rstrip()
            if spanLine:
                yield spanLine


def _iterParts(
    msg: str, location: None | InputLocation | Sequence[InputLocation]
) -> Iterator[
    tuple[str | None, Traversable | None, int, str | None, Sequence[tuple[int, int]]]
]:
    if location is None:
        yield msg, None, -1, None, []
    elif isinstance(location, InputLocation):
        loc = location
        yield msg, loc.path, loc.lineno, loc.line, [loc.span]
    else:
        multiMsg: str | None = msg
        i = 0
        while i < len(location):
            # Merge spans of following locations on the same line.
            loc = location[i]
            lineno = loc.lineno
            spans = [loc.span]
            i += 1
            while (
                i < len(location)
                and location[i].lineno == lineno
                and location[i].path == loc.path
            ):
                spans.append(location[i].span)
                i += 1
            yield multiMsg, loc.path, lineno, loc.line, spans
            multiMsg = None
