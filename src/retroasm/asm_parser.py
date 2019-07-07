from __future__ import annotations

from logging import getLogger
from typing import Iterable, Iterator, List, Sequence, Type, Union

from .expression_parser import NumberNode, parseDigits
from .instrset import InstructionSet
from .linereader import DelayedError, LineReader
from .tokens import Token, TokenEnum
from .types import unlimited

logger = getLogger('parse-asm')


class AsmToken(TokenEnum):
    # pylint: disable=bad-whitespace
    number      = r'\$\w+|%\w+|\d\w*'
    word        = r'[\w.]+'
    string      = r'"[^"]*"|\'[^\']*\''
    comment     = r';.*$'
    whitespace  = r'\s+'
    symbol      = r'.'
    end         = None

def parseNumber(token: Token[AsmToken]) -> NumberNode:
    """Parse a token of kind `AsmToken.number`.
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
        if kind is AsmToken.number or kind is AsmToken.string:
            yield int
        elif kind is AsmToken.word or kind is AsmToken.symbol:
            yield token.value
        else:
            assert kind is AsmToken.end, token

def parseInstruction(tokens: List[Token[AsmToken]], reader: LineReader) -> None:
    try:
        with reader.checkErrors():
            for token in tokens:
                if token.kind is AsmToken.number:
                    # Convert to int.
                    try:
                        number = parseNumber(token)
                    except ValueError as ex:
                        reader.error('%s', ex, location=token.location)
                elif token.kind is AsmToken.string:
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
            for token in AsmToken.scan(line)
            if token.kind not in (AsmToken.whitespace, AsmToken.comment)
            ]

        # Look for a label.
        label = None
        if tokens[0].check(AsmToken.word):
            if tokens[1].check(AsmToken.symbol, ':'):
                label = tokens[0].value
                del tokens[:2]
            elif tokens[1].check(AsmToken.word, 'equ'):
                label = tokens[0].value
                del tokens[0]
            elif tokens[0].value.startswith('.'):
                label = tokens[0].value
                del tokens[0]
        if label is not None:
            reader.info('label: %s', label)

        # Look for a directive or instruction.
        if tokens[0].check(AsmToken.end):
            continue
        if not tokens[0].check(AsmToken.word):
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
