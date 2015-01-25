from contextlib import contextmanager
from logging import DEBUG, INFO, WARNING, ERROR, CRITICAL, Formatter
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
        self.pathname = pathname
        self.lines = lines
        self.logger = logger

        self.lastline = None
        self.lineno = 0
        self.warnings = 0
        self.errors = 0

    def __iter__(self):
        return self

    def __next__(self):
        self.lastline = None # in case next() raises StopIteration
        line = next(self.lines).rstrip('\n')
        self.lastline = line
        self.lineno += 1
        return line

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

    def critical(self, msg, *args, **kwargs):
        '''Log a message at the CRITICAL level, increase the error count and
        stop the line iteration.
        '''
        self.errors += 1
        self.__log(CRITICAL, 'FATAL: ' + msg, *args, **kwargs)
        self.lines = iter(())

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
            extra = self.getLocation() if location is None else location
            self.logger.log(level, msg, *args, extra=extra, **kwargs)

    def getLocation(self, span=None):
        '''Returns a dictionary describing the current location in the input
        stream. This can be passed as the "location" argument to the log methods
        when a message should be logged for a line other than the current one.
        If the 'span' argument is provided, it must be a tuple of the start
        and end index of the area of interest within the current line.
        '''
        return dict(
            readerPathname=self.pathname,
            readerLineno=self.lineno,
            readerLastline=self.lastline,
            readerColSpan=span,
            )

def getSpan(location):
    '''Returns the column span information of the given location,
    or None if the given location has no column span information.
    '''
    return location.get('readerColSpan')

def getText(location):
    '''Returns the text described by the given location: the spanned substring
    if span information is available, otherwise the full line.
    '''
    line = location['readerLastline']
    try:
        span = location['readerColSpan']
    except KeyError:
        return line
    else:
        return line[slice(*span)]

def updateSpan(location, span):
    '''Adds or updates the column span information of a location.
    Returns an updated location object; the original is unmodified.
    '''
    return dict(location, readerColSpan=span)

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
    The 'location' attribute contains the location in the instruction set
    definition file that triggered the exception, as a metadata dictionary that
    can be used with LineReader, or None if this information is not available.
    '''

    @classmethod
    def withText(cls, msg, location):
        '''Returns an instance of the BadInput (sub)class it is called on,
        with the input text in the location's span appended after the error
        message.
        '''
        return cls('%s: %s' % (msg, getText(location)), location)

    def __init__(self, msg, location=None):
        Exception.__init__(self, msg)
        self.location = location

reComment = re.compile(r'(?<!\\)#')

class DefLineReader(LineReader):
    '''Iterates through lines of a block-structured definition file.
    Trailing whitespace is stripped, comments are removed.
    If a block header is bad, it is possible to skip the parsing of that
    block without aborting parsing altogether, in an attempt to catch
    multiple errors in a single pass.
    '''

    def __next__(self):
        while True:
            line = super().__next__().rstrip()
            match = reComment.search(line)
            if match:
                line = line[ : match.start()].rstrip()
                if not line:
                    # Comment lines are ignored rather than returned as empty
                    # lines, such that they don't terminate blocks.
                    continue
            return line

    def iterBlock(self):
        '''Iterates through the lines of the current block.
        '''
        while True:
            line = next(self)
            if line:
                yield line
            else:
                break

    def skipBlock(self):
        '''Skips the remainder of the current block.
        '''
        for _ in self.iterBlock():
            pass

class LineReaderFormatter(Formatter):

    def format(self, record):
        readerPathname = getattr(record, 'readerPathname', '-')
        readerLineno = getattr(record, 'readerLineno', None)
        readerLastline = getattr(record, 'readerLastline', None)
        readerColSpan = getattr(record, 'readerColSpan', None)
        msg = super().format(record)
        if readerLineno is not None:
            msg = '%s:%s: %s' % (readerPathname, readerLineno, msg)
        if readerLastline is not None:
            msg += '\n  ' + readerLastline
        if readerColSpan is not None:
            fromIdx, toIdx = readerColSpan
            if fromIdx == toIdx:
                toIdx += 1
            msg += '\n  ' + ' ' * fromIdx + '^' * (toIdx - fromIdx)
        return msg
