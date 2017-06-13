from .expression_parser import parseDigits
from .linereader import DelayedError, LineReader

from collections import namedtuple
from enum import Enum
from logging import getLogger
import re

logger = getLogger('parse-asm')

Token = namedtuple('Token', ('kind', 'value', 'location'))

TokenKind = Enum('TokenKind', ( # pylint: disable=invalid-name
    'number', 'word', 'symbol', 'string', 'comment', 'whitespace', 'end'
    ))

_tokenPattern = re.compile('|'.join(
    '(?P<%s>%s)' % (token.name, regex) for token, regex in (
        # pylint: disable=bad-whitespace
        (TokenKind.number,      r'\$\w+|%\w+|\d\w*'),
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
        group = kind.name
        value = match.group(group)
        span = match.span(group)
        yield Token(kind, value, location.updateSpan(span))

def parseNumber(value):
    if value[0] == '$':
        return parseDigits(value[1:], 16)
    elif value[0] == '%':
        return parseDigits(value[1:], 2)
    elif value[0] == '0' and len(value) >= 2 and value[1] in 'xXbB':
        return parseDigits(value[2:], 16 if value[1] in 'xX' else 2)
    elif value[-1].isdigit():
        return parseDigits(value, 10)
    else:
        try:
            base = {'b': 2, 'h': 16}[value[-1].lower()]
            value = value[:-1]
        except KeyError:
            raise ValueError('bad number suffix "%s"' % value[-1])
        else:
            return parseDigits(value, base)

def createMatchSequence(tokens, reader):
    '''Convert tokens to a match sequence.
    '''
    for token in tokens:
        kind = token.kind
        if kind is TokenKind.number:
            yield int
        elif kind is TokenKind.word or kind is TokenKind.symbol:
            yield token.value
        else:
            assert False, token

def parseInstruction(tokens, reader):
    try:
        with reader.checkErrors():
            # Arbitrary strings are not allowed as instruction operands, but
            # single characters should be replaced by their character numbers.
            for idx in range(len(tokens)):
                token = tokens[idx]
                if token.kind is TokenKind.string:
                    value = token.value
                    assert len(value) >= 2, value
                    assert value[0] == value[-1], value
                    if len(value) == 2:
                        reader.error(
                            'empty string in instruction operand',
                            location=token.location
                            )
                    elif len(value) == 3:
                        tokens[idx] = Token(
                            TokenKind.number, ord(value[1]), token.location
                            )
                    else:
                        reader.error(
                            'multi-character string in instruction operand',
                            location=token.location
                            )
    except DelayedError:
        return None

    matchSeq = tuple(createMatchSequence(tokens, reader))

    reader.info(
        'instruction %s', ' '.join(str(elem) for elem in matchSeq),
        location=tokens[0].location
        )

def parseDirective(tokens, reader):
    reader.info(
        'directive: %s', ' '.join(str(token.value) for token in tokens),
        location=tokens[0].location
        )

def parseAsm(reader, instrSet):
    instrSet.dumpMnemonicTree()
    instructionNames = instrSet.instructionNames

    for line in reader:
        # Tokenize entire line.
        tokens = []
        try:
            with reader.checkErrors():
                for token in tokenizeLine(line, reader.getLocation()):
                    kind = token.kind
                    if kind is TokenKind.whitespace:
                        # Skip whitespace.
                        continue
                    elif kind is TokenKind.comment:
                        # Strip comment.
                        break
                    elif kind is TokenKind.number:
                        # Convert to int.
                        try:
                            value = parseNumber(token.value)
                        except ValueError as ex:
                            reader.error('%s', ex, location=token.location)
                            continue
                        else:
                            token = Token(kind, value, token.location)
                    tokens.append(token)
        except DelayedError:
            continue

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
        firstWord = tokens[0].value
        if firstWord in instructionNames:
            parseInstruction(tokens, reader)
        else:
            parseDirective(tokens, reader)

def readSource(path, instrSet):
    with LineReader.open(path, logger) as reader:
        with reader.checkErrors():
            parseAsm(reader, instrSet)
            reader.summarize()
