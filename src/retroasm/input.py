from __future__ import annotations

from collections.abc import Iterable, Iterator, Sequence
from contextlib import contextmanager
from dataclasses import dataclass
from logging import DEBUG, ERROR, INFO, WARNING, Formatter, Logger, LogRecord
from re import Match, Pattern
from typing import Any, Self, overload


@dataclass(frozen=True, slots=True)
class InputLocation:
    """
    Describes a particular location in an input text file.
    This can be used to provide context in error reporting, by passing it as
    the value of the 'location' argument to the log methods of LineReader.
    """

    path: str
    """
    The path to the input text file.

    This path is only used for logging; it does not need to refer to any real file.
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

    def __getitem__(self, item: int | slice) -> InputLocation:
        span_start, span_end = self.span
        span_len = span_end - span_start

        if isinstance(item, int):
            index = item + span_len if item < 0 else item
            if 0 <= index < span_len:
                new_span_start = span_start + index
                new_span_end = new_span_start + 1
            else:
                raise IndexError(f"index {item} out of range for length {span_len}")
        else:
            start, stop, step = item.indices(span_len)
            if step != 1:
                raise IndexError("step sizes other than 1 are not supported")
            new_span_start = span_start + start
            new_span_end = max(span_start + stop, new_span_start)

        return self.update_span((new_span_start, new_span_end))

    def __len__(self) -> int:
        start, end = self.span
        return end - start

    def update_span(self, span: tuple[int, int]) -> InputLocation:
        """
        Adds or updates the column span information of a location.
        Returns an updated location object; the original is unmodified.
        """
        return InputLocation(self.path, self.lineno, self.line, span)

    @overload
    @staticmethod
    def merge_span(*locations: InputLocation) -> InputLocation:
        ...

    @overload
    @staticmethod
    def merge_span(*locations: InputLocation | None) -> InputLocation | None:
        ...

    @staticmethod
    def merge_span(*locations: InputLocation | None) -> InputLocation | None:
        """
        Return a new location with a minimal span that includes all of the given
        locations.
        All given locations must be on the same line.
        """
        base_location: InputLocation | None = None
        for location in locations:
            if location is None:
                continue
            if base_location is None:
                base_location = location
                merged_start, merged_end = base_location.span
            else:
                assert location.path == base_location.path, locations
                assert location.lineno == base_location.lineno, locations
                start, end = location.span
                merged_start = min(merged_start, start)
                merged_end = max(merged_end, end)
        if base_location is None:
            return None
        else:
            return base_location.update_span((merged_start, merged_end))

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


class BadInput(Exception):
    """
    An exception which contains information about the part of the input
    file which is considered to violate a rule.
    The `locations` attribute can contain `InputLocation` objects describing
    the location(s) in the input file that triggered the exception.
    """

    @classmethod
    def with_text(cls, msg: str, location: InputLocation | None) -> BadInput:
        """
        Returns an instance of the BadInput (sub)class it is called on,
        with the input text in the location's span appended after the error
        message.
        """
        if location is None:
            return cls(msg)
        else:
            return cls(f"{msg}: {location.text}", location)

    def __init__(self, msg: str, *locations: InputLocation | None):
        Exception.__init__(self, msg)
        self.locations = tuple(loc for loc in locations if loc is not None)


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


class InputLogger:
    """
    A logger with support for locations.
    """

    def __init__(self, path: str, logger: Logger):
        self._path = path
        self.logger = logger
        self.problem_counter = ProblemCounter()

    def debug(self, msg: str, *args: object) -> None:
        """Log a message at the DEBUG level."""
        self.__log(DEBUG, msg, *args)

    def info(
        self,
        msg: str,
        *args: object,
        location: Sequence[InputLocation | None] | InputLocation | None,
    ) -> None:
        """Log a message at the INFO level."""
        self.__log(INFO, msg, *args, location=location)

    def warning(
        self,
        msg: str,
        *args: object,
        location: Sequence[InputLocation | None] | InputLocation | None,
    ) -> None:
        """Log a message at the WARNING level and increase the warning count."""
        self.problem_counter.num_warnings += 1
        self.__log(WARNING, "warning: " + msg, *args, location=location)

    def error(
        self,
        msg: str,
        *args: object,
        location: Sequence[InputLocation | None] | InputLocation | None,
    ) -> None:
        """Log a message at the ERROR level and increase the error count."""
        self.problem_counter.num_errors += 1
        self.__log(ERROR, "ERROR: " + msg, *args, location=location)

    @contextmanager
    def check_errors(self) -> Iterator[Self]:
        """
        Returns a context manager that raises DelayedError on context close
        if any errors were logged since the context was opened.
        """
        num_errors_before = self.problem_counter.num_errors
        yield self
        num_errors = self.problem_counter.num_errors - num_errors_before
        if num_errors != 0:
            raise DelayedError(f"{num_errors:d} errors were logged")

    def summarize(self) -> None:
        """Log a message containing the error and warning counts."""
        problem_counter = self.problem_counter
        self.logger.log(problem_counter.level, "%s: %s", self._path, problem_counter)

    def __log(self, level: int, msg: str, *args: Any, **kwargs: Any) -> None:
        logger = self.logger
        if logger.isEnabledFor(level):
            location: None | InputLocation | Sequence[InputLocation] = kwargs.pop(
                "location", None
            )
            logger.log(level, msg, *args, extra={"location": location}, **kwargs)


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


@dataclass(slots=True)
class ProblemCounter:
    """Error and warning counts."""

    num_errors: int = 0
    num_warnings: int = 0

    @property
    def level(self) -> int:
        """Logging level corresponding to the problems counted."""
        if self.num_errors > 0:
            return ERROR
        elif self.num_warnings > 0:
            return WARNING
        else:
            return INFO

    def __str__(self) -> str:
        return (
            f"{_pluralize(self.num_errors, 'error')} and "
            f"{_pluralize(self.num_warnings, 'warning')}"
        )

    def __add__(self, other: ProblemCounter) -> ProblemCounter:
        return ProblemCounter(
            self.num_errors + other.num_errors, self.num_warnings + other.num_warnings
        )

    def __iadd__(self, other: ProblemCounter) -> ProblemCounter:
        self.num_errors += other.num_errors
        self.num_warnings += other.num_warnings
        return self


def _pluralize(count: int, verb: str) -> str:
    return f"{count:d} {verb}{'' if count == 1 else 's'}"


class LineReaderFormatter(Formatter):
    def format(self, record: LogRecord) -> str:
        msg = super().format(record)
        location: None | InputLocation | Sequence[InputLocation] = getattr(
            record, "location", None
        )
        return "\n".join(_format_parts(_iter_parts(msg, location)))


def _iter_parts(
    msg: str, location: None | InputLocation | Sequence[InputLocation]
) -> Iterator[
    tuple[str | None, str | None, int, str | None, Sequence[tuple[int, int]]]
]:
    if location is None:
        yield msg, None, -1, None, []
    elif isinstance(location, InputLocation):
        loc = location
        yield msg, loc.path, loc.lineno, loc.line, [loc.span]
    else:
        multi_msg: str | None = msg
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
            yield multi_msg, loc.path, lineno, loc.line, spans
            multi_msg = None


def _format_parts(
    parts: Iterable[
        tuple[str | None, str | None, int, str | None, Sequence[tuple[int, int]]]
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
            span_line = " " * length
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
                span_line = span_line[:start] + highlight + span_line[end:]
            span_line = span_line.rstrip()
            if span_line:
                yield span_line
