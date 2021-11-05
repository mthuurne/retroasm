from __future__ import annotations

from typing import Iterable, Iterator, Mapping, cast

from .asm_directives import DataDirective
from .asm_formatter import Formatter
from .codeblock_builder import SemanticsCodeBlockBuilder
from .expression import IntLiteral
from .fetch import ImageFetcher
from .instrset import InstructionSet, flagsSetByCode
from .mode import EncodeMatch, ModeMatch
from .reference import Reference


def disassemble(
    instrSet: InstructionSet, fetcher: ImageFetcher, startAddr: int
) -> Iterator[tuple[int, DataDirective | ModeMatch]]:
    """
    Disassemble instructions from the given fetcher.
    The fetched data is assumed to be code for the given instruction set,
    to be executed at the given address.
    """
    pc = cast(Reference, instrSet.globalNamespace["pc"])
    numBytes = fetcher.numBytes

    addr = startAddr
    while fetcher[0] is not None:
        encodedLength, encMatch = decode(instrSet, fetcher)
        if encMatch is None:
            # Force at least one encoding item to be disassembled to a data directive,
            # otherwise we would not make any progress.
            encodedLength = max(encodedLength, 1)
            for idx in range(encodedLength):
                value = fetcher[idx]
                assert value is not None
                yield addr, DataDirective.literal(instrSet.encodingType, value)
                addr += numBytes
        else:
            # TODO: Handle situations where the re-encoding of the match does not
            #       result in the same encoded data as the input, for example when
            #       redundant prefixes are present.
            postAddr = addr + encodedLength * numBytes
            modeMatch = ModeMatch.fromEncodeMatch(encMatch)
            yield addr, modeMatch.substPC(pc, IntLiteral(postAddr))
            addr = postAddr
        fetcher = fetcher.advance(encodedLength)


def decode(
    instrSet: InstructionSet, fetcher: ImageFetcher
) -> tuple[int, EncodeMatch | None]:
    """
    Attempt to decode one instruction from the given fetcher.

    Return the number of encoding items decoded and the decoded instruction, if any.
    """

    # Decode prefixes.
    prefixes = []
    decodePrefix = instrSet.prefixDecodeFunc
    encodedLength = 0
    while (prefix := decodePrefix(fetcher)) is not None:
        prefixes.append(prefix)
        prefixEncLen = prefix.encoding.encodedLength
        assert prefixEncLen is not None, prefix
        fetcher = fetcher.advance(prefixEncLen)
        encodedLength += prefixEncLen

    # Compute prefix flags.
    if prefixes:
        prefixMapping = instrSet.prefixMapping
        prefixBuilder = SemanticsCodeBlockBuilder()
        prefixBuilder.inlineBlock(prefixMapping.initCode)
        for prefix in prefixes:
            prefixBuilder.inlineBlock(prefix.semantics)
        prefixCode = prefixBuilder.createCodeBlock(())
        flagForVar = prefixMapping.flagForVar
        flags = frozenset(flagForVar[storage] for storage in flagsSetByCode(prefixCode))
    else:
        flags = frozenset()

    # Decode instruction.
    decoder = instrSet.getDecoder(flags)
    encMatch = decoder.tryDecode(fetcher)
    if encMatch is not None:
        encodedLength += encMatch.encodedLength

    return encodedLength, encMatch


def formatAsm(
    formatter: Formatter,
    decoded: Iterable[tuple[int, DataDirective | ModeMatch]],
    labels: Mapping[int, str],
) -> None:
    for addr, match in decoded:
        label = labels.get(addr)
        if label is not None:
            print(formatter.label(label))
        if isinstance(match, DataDirective):
            print(formatter.data(match))
        else:
            print(formatter.mnemonic(match.mnemonic, labels))
