from __future__ import annotations

from typing import Iterable, Iterator, Mapping, Sequence, cast

from .asm_directives import DataDirective
from .asm_formatter import Formatter
from .codeblock_builder import SemanticsCodeBlockBuilder
from .expression import IntLiteral
from .fetch import ImageFetcher
from .instrset import InstructionSet, flagsSetByCode
from .mode import EncodeMatch, Encoding, ModeMatch
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
        prefixEncs, encMatch = decode(instrSet, fetcher)

        encodedLength = sum(cast(int, enc.encodedLength) for enc in prefixEncs)
        encodedLength += 1 if encMatch is None else encMatch.encodedLength
        postAddr = addr + encodedLength * numBytes

        if encMatch is None:
            for prefixEnc in prefixEncs:
                raise NotImplementedError  # TODO: Implement.
            value = fetcher[0]
            if value is None:
                break
            yield addr, DataDirective.literal(instrSet.encodingType, value)
        else:
            # TODO: Handle situations where the re-encoding of the match does not
            #       result in the same encoded data as the input, for example when
            #       redundant prefixes are present.
            modeMatch = ModeMatch.fromEncodeMatch(encMatch)
            yield addr, modeMatch.substPC(pc, IntLiteral(postAddr))
        fetcher = fetcher.advance(encodedLength)
        addr = postAddr


def decode(
    instrSet: InstructionSet, fetcher: ImageFetcher
) -> tuple[Sequence[Encoding], EncodeMatch | None]:
    """Attempt to decode one instruction from the given fetcher."""

    # Decode prefixes.
    prefixes = []
    decodePrefix = instrSet.prefixDecodeFunc
    while (prefix := decodePrefix(fetcher)) is not None:
        prefixes.append(prefix)
        prefixEncLen = prefix.encoding.encodedLength
        assert prefixEncLen is not None, prefix
        fetcher = fetcher.advance(prefixEncLen)

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

    return tuple(prefix.encoding for prefix in prefixes), encMatch


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
