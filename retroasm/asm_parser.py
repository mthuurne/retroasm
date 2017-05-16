from .linereader import LineReader

from collections import namedtuple
from enum import Enum
from logging import getLogger
import re

logger = getLogger('parse-asm')

Token = namedtuple('Token', ('kind', 'value', 'location'))

TokenKind = Enum('TokenKind', ( # pylint: disable=invalid-name
    'word', 'symbol', 'string', 'comment', 'whitespace', 'end'
    ))

_tokenPattern = re.compile('|'.join(
    '(?P<%s>%s)' % (token.name, regex) for token, regex in (
        # pylint: disable=bad-whitespace
        (TokenKind.word,        r'\w+'),
        (TokenKind.string,      r'"[^"]*"|\'[^\']*\''),
        (TokenKind.comment,     r';.*$'),
        (TokenKind.whitespace,  r'\s+'),
        (TokenKind.symbol,      r'.'),
        )
    ))

def tokenizeLine(line, location):
    '''Iterates through the Tokens in a line of assembly.
    '''
    for match in _tokenPattern.finditer(line):
        kind = getattr(TokenKind, match.lastgroup)
        if kind is not TokenKind.whitespace:
            group = kind.name
            value = match.group(group)
            span = match.span(group)
            yield Token(kind, value, location.updateSpan(span))

def parseAsm(reader):
    for line in reader:
        # Tokenize entire line.
        tokens = list(tokenizeLine(line, reader.getLocation()))

        # Strip comment.
        if tokens and tokens[-1].kind is TokenKind.comment:
            del tokens[-1]

        def check(idx, kind, value=None, tokens=tokens):
            if idx < len(tokens):
                token = tokens[idx]
                if token.kind is kind:
                    if value is None or token.value.lower() == value:
                        return True
            return False

        # Look for a label.
        label = None
        if check(0, TokenKind.word):
            if check(1, TokenKind.symbol, ':'):
                label = tokens[0].value
                del tokens[:2]
            elif check(1, TokenKind.word, 'equ'):
                label = tokens[0].value
                del tokens[0]
        if label is not None:
            reader.info('label: %s', label)

        # Look for a directive or instruction.
        if not tokens:
            continue
        if not check(0, TokenKind.word):
            reader.error(
                'expected directive or instruction, got %s',
                tokens[0].kind.name,
                location=tokens[0].location
                )
            continue
        instr = tokens[0].value

        reader.info('%s', instr, location=tokens[0].location)

def readSource(path):
    with LineReader.open(path, logger) as reader:
        with reader.checkErrors():
            parseAsm(reader)
            reader.summarize()
