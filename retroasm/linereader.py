from contextlib import contextmanager
from logging import DEBUG, INFO, WARNING, ERROR, Formatter
import re

class LineReader:
    '''Iterates through the lines of a text file.
    The lines will not contain a trailing newline character.
    Log methods on the reader can be used to produce log records with context
    information: path name and the number and contents of the current line.
    Errors and warnings reported in this way are counted.
    The companion class LineReaderFormatter can be used to incorporate the
    context information in the logging.
    '''

    @classmethod
    @contextmanager
    def open(cls, pathname, logger):
        with open(pathname, 'r') as lines:
            reader = cls(pathname, lines, logger)
            reader.debug('start reading')
            yield reader
            reader.debug('done reading')

    def __init__(self, pathname, lines, logger):
        self._pathname = pathname
        self._lines = lines
        self.logger = logger

        self._lastline = None
        self._lineno = 0
        self.warnings = 0
        self.errors = 0

    def __iter__(self):
        return self

    def _nextLine(self):
        self._lastline = None # in case next() raises StopIteration
        line = next(self._lines).rstrip('\n')
        self._lastline = line
        self._lineno += 1

    def __next__(self):
        self._nextLine()
        return self.location

    @property
    def location(self):
        '''Returns an InputLocation object describing the current line in
        the input file.
        '''
        return InputLocation(self._pathname, self._lineno, self._lastline, None)

    def debug(self, msg, *args, **kwargs):
        '''Log a message at the DEBUG level.
        '''
        self.__log(DEBUG, msg, *args, **kwargs)

    def info(self, msg, *args, **kwargs):
        '''Log a message at the INFO level.
        '''
        self.__log(INFO, msg, *args, **kwargs)

    def warning(self, msg, *args, **kwargs):
        '''Log a message at the WARNING level and increase the warning count.
        '''
        self.warnings += 1
        self.__log(WARNING, 'warning: ' + msg, *args, **kwargs)

    def error(self, msg, *args, **kwargs):
        '''Log a message at the ERROR level and increase the error count.
        '''
        self.errors += 1
        self.__log(ERROR, 'ERROR: ' + msg, *args, **kwargs)

    @contextmanager
    def checkErrors(self):
        '''Returns a context manager that raises DelayedError on context close
        if any errors were logged since the context was opened.
        '''
        errorsBefore = self.errors
        yield self
        numErrors = self.errors - errorsBefore
        if numErrors != 0:
            raise DelayedError('%d errors were logged' % numErrors)

    def summarize(self):
        '''Log a message containing the error and warning counts.
        '''
        level = ERROR if self.errors > 0 else (
            WARNING if self.warnings > 0 else INFO
            )
        msg = '%d error%s and %d warning%s' % (
            self.errors, '' if self.errors == 1 else 's',
            self.warnings, '' if self.warnings == 1 else 's'
            )
        self.__log(level, msg)

    def __log(self, level, msg, *args, location=None, **kwargs):
        if self.logger.isEnabledFor(level):
            if location is None:
                location = self.location
            extra = dict(location=location)
            self.logger.log(level, msg, *args, extra=extra, **kwargs)

class InputLocation:
    '''Describes a particular location in an input text file.
    This can be used to provide context in error reporting, by passing it as
    the value of the 'location' argument to the log methods of LineReader.
    '''
    __slots__ = ('_pathname', '_lineno', '_line', '_span')

    pathname = property(lambda self: self._pathname)
    '''The file system path to the input text file.'''

    line = property(lambda self: self._line)
    '''The contents of the input line that this location describes.'''

    lineno = property(lambda self: self._lineno)
    '''The number of this location's line in the input file, where line 1 is
    the first line.'''

    span = property(lambda self: self._span)
    '''The column span information of this location, or None if no column span
    information is available.'''

    @property
    def effectiveSpan(self):
        '''The column span information of this location, or the full line if
        no column span information is available.
        '''
        span = self._span
        return (0, len(self._line)) if span is None else span

    def __init__(self, pathname, lineno, line, span=None):
        self._pathname = pathname
        self._lineno = lineno
        self._line = line
        self._span = span

    def __repr__(self):
        return 'InputLocation(%r, %r, %r, %r)' % (
            self._pathname, self._lineno, self._line, self._span
            )

    def __eq__(self, other):
        return ( # pylint: disable=protected-access
            isinstance(other, InputLocation)
            and self._pathname == other._pathname
            and self._lineno == other._lineno
            and self._line == other._line
            and self._span == other._span
            )

    def __ne__(self, other):
        return not self.__eq__(other)

    def __len__(self):
        span = self._span
        return len(self._line) if span is None else span[1] - span[0]

    def updateSpan(self, span):
        '''Adds or updates the column span information of a location.
        Returns an updated location object; the original is unmodified.
        '''
        return InputLocation(self._pathname, self._lineno, self._line, span)

    @property
    def endLocation(self):
        '''A zero-length location marking the end point of this location.
        '''
        end = self.effectiveSpan[1]
        return self.updateSpan((end, end))

    @property
    def text(self):
        '''Returns the text described by this location: the spanned substring
        if span information is available, otherwise the full line.
        '''
        line = self._line
        span = self._span
        return line if span is None else line[slice(*span)]

    def match(self, pattern):
        '''Matches the text in this location to the given compiled regular
        expression pattern.
        Returns an InputMatch object, or None if no match was found.
        '''
        match = pattern.match(self._line, *self.effectiveSpan)
        return None if match is None else InputMatch(self, match)

    def findLocations(self, pattern):
        '''Searches the text in this location for the given compiled regular
        expression pattern.
        Returns an iterator that yields an InputLocation object for each
        match.
        '''
        for match in pattern.finditer(self._line, *self.effectiveSpan):
            yield self.updateSpan(match.span(0))

    def findMatches(self, pattern):
        '''Searches the text in this location for the given compiled regular
        expression pattern.
        Returns an iterator that yields an InputMatch object for each
        match.
        '''
        for match in pattern.finditer(self._line, *self.effectiveSpan):
            yield InputMatch(self, match)

    def split(self, pattern):
        '''Splits the text in this location using the given pattern as a
        separator.
        Returns an iterator yielding InputLocations representing the text
        between the separators.
        '''
        searchStart, searchEnd = self.effectiveSpan
        curr = searchStart
        for match in pattern.finditer(self._line, searchStart, searchEnd):
            sepStart, sepEnd = match.span(0)
            yield self.updateSpan((curr, sepStart))
            curr = sepEnd
        yield self.updateSpan((curr, searchEnd))

class InputMatch:
    '''The result of a regular expression operation on an InputLocation.
    The interface is inspired by, but not equal to, that of match objects from
    the "re" module of the standard library.
    '''
    __slots__ = ('_location', '_match')

    def __init__(self, location, match):
        self._location = location
        self._match = match

    def group(self, index):
        '''Returns an InputLocation for the group matched at the given index,
        with the first group being 1. If 0 as passed as the index, an
        InputLocation for the entire matched string is returned.
        If the group did not participate in the match, None is returned.
        '''
        span = self._match.span(index)
        if span == (-1, -1):
            return None
        else:
            return self._location.updateSpan(span)

    @property
    def groups(self):
        '''A tuple containing an InputLocation for each of the groups matched
        and None for those groups that were not part of the match.
        '''
        return tuple(self.group(i) for i in range(1, self._match.re.groups + 1))

    @property
    def groupName(self):
        '''The name of the last matched group, or None if last matched group
        was nameless or no groups were matched.
        '''
        return self._match.lastgroup

def mergeSpan(fromLocation, toLocation):
    '''Returns a new location of which the span starts at the start of the
    given 'from' location and ends at the end of the given 'to' location.
    Both given locations must be on the same line.
    '''
    mergedSpan = (fromLocation.span[0], toLocation.span[1])
    mergedLocation = fromLocation.updateSpan(mergedSpan)
    assert mergedLocation == toLocation.updateSpan(mergedSpan), \
            (fromLocation, toLocation)
    return mergedLocation

class DelayedError(Exception):
    '''Raised when one or more errors were encountered when processing input.
    Since we want to report as many errors as possible in each processing,
    errors are logged and processing continues. However, it usually doesn't
    make sense to continue with later processing steps, since the incomplete
    output caused by earlier errors would trigger new errors. Therefore
    at the end of a processing step DelayedError can be raised to abort
    processing.
    '''

class BadInput(Exception):
    '''An exception which contains information about the part of the input
    file which is considered to violate a rule.
    The 'location' attribute contains an InputLocation object describing
    the location in the input file that triggered the exception, or None if
    this information is not available.
    '''

    @classmethod
    def withText(cls, msg, location):
        '''Returns an instance of the BadInput (sub)class it is called on,
        with the input text in the location's span appended after the error
        message.
        '''
        return cls('%s: %s' % (msg, location.text), location)

    def __init__(self, msg, location=None):
        Exception.__init__(self, msg)
        self.location = location

_reComment = re.compile(r'(?<!\\)#')

class DefLineReader(LineReader):
    '''Iterates through lines of a block-structured definition file.
    Trailing whitespace is stripped, comments are removed.
    If a block header is bad, it is possible to skip the parsing of that
    block without aborting parsing altogether, in an attempt to catch
    multiple errors in a single pass.
    '''

    def __next__(self):
        while True:
            self._nextLine()
            line = self._lastline.rstrip()
            match = _reComment.search(line)
            if match is None:
                span = None
            else:
                end = match.start()
                while end > 0 and line[end - 1].isspace():
                    end -= 1
                if end == 0:
                    # Comment lines are ignored rather than returned as empty
                    # lines, such that they don't terminate blocks.
                    continue
                span = (0, end)
            return InputLocation(self._pathname, self._lineno, line, span)

    def iterBlock(self):
        '''Iterates through the lines of the current block.
        '''
        while True:
            line = next(self)
            if len(line) == 0:
                break
            yield line

    def skipBlock(self):
        '''Skips the remainder of the current block.
        '''
        for _ in self.iterBlock():
            pass

class LineReaderFormatter(Formatter):

    def format(self, record):
        msg = super().format(record)
        location = getattr(record, 'location', None)
        return '\n'.join(self._formatParts(self._iterParts(msg, location)))

    def _formatParts(self, parts):
        for msg, pathname, lineno, line, spans in parts:
            if pathname is None:
                assert msg is not None
                yield msg
            elif msg is None:
                yield '%s:%d:' % (pathname, lineno)
            else:
                yield '%s:%d: %s' % (pathname, lineno, msg)

            if line is not None:
                yield line

                length = len(line) + 1
                spanLine = ' ' * length
                last = len(spans) - 1
                for i, span in enumerate(reversed(spans)):
                    if span is None:
                        continue
                    start, end = span
                    start = min(start, length)
                    end = min(end, length)
                    if start > end:
                        continue
                    elif start == end:
                        # Highlight empty span using single character.
                        end = start + 1
                    highlight = ('^' if i == last else '~') * (end - start)
                    spanLine = spanLine[:start] + highlight + spanLine[end:]
                spanLine = spanLine.rstrip()
                if spanLine:
                    yield spanLine

    def _iterParts(self, msg, location):
        if location is None:
            yield msg, None, None, None, []
        elif isinstance(location, InputLocation):
            loc = location
            yield msg, loc.pathname, loc.lineno, loc.line, [loc.span]
        else:
            i = 0
            while i < len(location):
                # Merge spans of following locations on the same line.
                loc = location[i]
                lineno = loc.lineno
                spans = [loc.span]
                i += 1
                while i < len(location) and \
                        location[i].lineno == lineno and \
                        location[i].pathname == loc.pathname:
                    spans.append(location[i].span)
                    i += 1
                yield msg, loc.pathname, lineno, loc.line, spans
                msg = None
