from enum import Enum, auto
from logging import getLogger
from typing import (
    Iterable, Iterator, List, NamedTuple, Optional, Sequence, Type, Union
)
import re

from .expression_parser import NumberNode, parseDigits
from .instrset import InstructionSet
from .linereader import DelayedError, InputLocation, LineReader
from .types import unlimited

logger = getLogger('parse-asm')

class TokenKind(Enum):
    number = auto()
    word = auto()
    symbol = auto()
    string = auto()
    comment = auto()
    whitespace = auto()
    end = auto()

class Token(NamedTuple):
    kind: TokenKind
    value: str
    location: InputLocation

    def check(self, kind: TokenKind, value: Optional[str] = None) -> bool:
        """Check whether this token is of a particular kind
        and optionally check the value as well.
        Return True for a match, False otherwise.
        """
        if self.kind is kind:
            if value is None or self.value.casefold() == value:
                return True
        return False

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

def tokenizeLine(line: InputLocation) -> Iterator[Token]:
    '''Iterates through the Tokens in a line of assembly.
    '''
    for match in line.findMatches(_tokenPattern):
        name = match.groupName
        assert name is not None
        kind = TokenKind[name]
        location = match.group(name)
        assert location is not None
        yield Token(kind, location.text, location)

def parseNumber(token: Token) -> NumberNode:
    """Parse a token of kind `TokenKind.number`.
    Raise `ValueError` if the token does not contain a valid numeric literal.
    """

    value = token.value
    if value[0] == '$':
        digits = value[1:]
        digitWidth = 4
    elif value[0] == '%':
        digits = value[1:]
        digitWidth = 1
    elif value[0] == '0' and len(value) >= 2 and value[1] in 'xXbB':
        digits = value[2:]
        digitWidth = 4 if value[1] in 'xX' else 1
    elif value[-1].isdigit():
        # Decimal numbers have no integer per-digit width.
        return NumberNode(parseDigits(value, 10), unlimited, token.location)
    else:
        digits = value[:-1]
        try:
            digitWidth = {'b': 1, 'h': 4}[value[-1].casefold()]
        except KeyError:
            raise ValueError('bad number suffix "%s"' % value[-1])

    return NumberNode(
        parseDigits(digits, 1 << digitWidth),
        len(digits) * digitWidth,
        token.location
        )

def createMatchSequence(tokens: Iterable[Token]
                        ) -> Iterator[Union[Type[int], int, str]]:
    '''Convert tokens to a match sequence.
    '''
    for token in tokens:
        kind = token.kind
        if kind is TokenKind.number or kind is TokenKind.string:
            yield int
        elif kind is TokenKind.word or kind is TokenKind.symbol:
            yield token.value
        else:
            assert kind is TokenKind.end, token

def parseInstruction(tokens: List[Token], reader: LineReader) -> None:
    try:
        with reader.checkErrors():
            for token in tokens:
                if token.kind is TokenKind.number:
                    # Convert to int.
                    try:
                        number = parseNumber(token)
                    except ValueError as ex:
                        reader.error('%s', ex, location=token.location)
                elif token.kind is TokenKind.string:
                    # Arbitrary strings are not allowed as instruction
                    # operands, but single characters should be replaced
                    # by their character numbers.
                    value = token.value
                    assert len(value) >= 2, value
                    assert value[0] == value[-1], value
                    if len(value) == 2:
                        reader.error(
                            'empty string in instruction operand',
                            location=token.location
                            )
                    elif len(value) == 3:
                        number = NumberNode(ord(value[1]), 8, token.location)
                    else:
                        reader.error(
                            'multi-character string in instruction operand',
                            location=token.location
                            )
    except DelayedError:
        return None

    matchSeq = tuple(createMatchSequence(tokens))

    reader.info(
        'instruction %s', ' '.join(str(elem) for elem in matchSeq),
        location=tokens[0].location
        )

def parseDirective(tokens: Sequence[Token], reader: LineReader) -> None:
    reader.info(
        'directive: %s', ' '.join(str(token.value) for token in tokens),
        location=tokens[0].location
        )

def parseAsm(reader: LineReader, instrSet: InstructionSet) -> None:
    instrSet.dumpMnemonicTree()
    instructionNames = instrSet.instructionNames

    for line in reader:
        # Tokenize entire line.
        tokens = [
            token
            for token in tokenizeLine(line)
            if token.kind not in (TokenKind.whitespace, TokenKind.comment)
            ]
        # Add sentinel.
        tokens.append(Token(TokenKind.end, '', line.endLocation))

        # Look for a label.
        label = None
        if tokens[0].check(TokenKind.word):
            if tokens[1].check(TokenKind.symbol, ':'):
                label = tokens[0].value
                del tokens[:2]
            elif tokens[1].check(TokenKind.word, 'equ'):
                label = tokens[0].value
                del tokens[0]
        if label is not None:
            reader.info('label: %s', label)

        # Look for a directive or instruction.
        if tokens[0].check(TokenKind.end):
            continue
        if not tokens[0].check(TokenKind.word):
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

def readSource(path: str, instrSet: InstructionSet) -> None:
    with LineReader.open(path, logger) as reader:
        with reader.checkErrors():
            parseAsm(reader, instrSet)
            reader.summarize()
