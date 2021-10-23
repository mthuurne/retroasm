from __future__ import annotations

from typing import AbstractSet, Iterable, Iterator, Mapping, cast

from .asm_formatter import Formatter
from .codeblock_builder import SemanticsCodeBlockBuilder
from .expression import IntLiteral
from .fetch import ImageFetcher
from .instrset import InstructionSet, flagsSetByCode
from .mode import ModeMatch
from .reference import FixedValue, Reference
from .types import IntType, unlimited


def disassemble(
    instrSet: InstructionSet, fetcher: ImageFetcher, startAddr: int
) -> Iterator[tuple[int, Reference | ModeMatch]]:
    """
    Disassemble instructions from the given fetcher.
    The fetched data is assumed to be code for the given instruction set,
    to be executed at the given address.
    """
    pc = cast(Reference, instrSet.globalNamespace["pc"])
    decodePrefix = instrSet.prefixDecodeFunc
    prefixMapping = instrSet.prefixMapping
    prefixInitCode = prefixMapping.initCode
    flagForVar = prefixMapping.flagForVar
    numBytes = fetcher.numBytes
    encWidth = instrSet.encodingWidth
    assert encWidth is not None
    encType = IntType.int if encWidth is unlimited else IntType.u(encWidth)

    addr = startAddr
    while fetcher[0] is not None:
        # Decode prefixes.
        prefixBuilder = None
        while True:
            prefix = decodePrefix(fetcher)
            if prefix is None:
                break
            if prefixBuilder is None:
                prefixBuilder = SemanticsCodeBlockBuilder()
                prefixBuilder.inlineBlock(prefixInitCode)
            prefixBuilder.inlineBlock(prefix.semantics)
            encLen = prefix.encoding.encodedLength
            assert encLen is not None, prefix
            fetcher = fetcher.advance(encLen)
        if prefixBuilder is None:
            flags: AbstractSet[str] = frozenset()
        else:
            prefixCode = prefixBuilder.createCodeBlock(())
            flags = frozenset(
                flagForVar[storage] for storage in flagsSetByCode(prefixCode)
            )

        # Decode instruction.
        decoder = instrSet.getDecoder(flags)
        encMatch = decoder.tryDecode(fetcher)
        encodedLength = 1 if encMatch is None else encMatch.encodedLength
        postAddr = addr + encodedLength * numBytes
        if encMatch is None:
            value = fetcher[0]
            if value is None:
                break
            bits = FixedValue(IntLiteral(value), encWidth)
            yield addr, Reference(bits, encType)
        else:
            match = ModeMatch.fromEncodeMatch(encMatch)
            yield addr, match.substPC(pc, IntLiteral(postAddr))
        fetcher = fetcher.advance(encodedLength)
        addr = postAddr


def formatAsm(
    formatter: Formatter,
    decoded: Iterable[tuple[int, Reference | ModeMatch]],
    labels: Mapping[int, str],
) -> None:
    for addr, match in decoded:
        label = labels.get(addr)
        if label is not None:
            print(formatter.formatLabel(label))
        if isinstance(match, Reference):
            print(formatter.formatData(match))
        else:
            print(formatter.formatMnemonic(match.mnemonic, labels))
