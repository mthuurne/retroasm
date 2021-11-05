from __future__ import annotations

from typing import Iterable, Iterator, Mapping, cast

from .asm_directives import DataDirective
from .asm_formatter import Formatter
from .expression import IntLiteral
from .fetch import ImageFetcher
from .instrset import InstructionSet
from .mode import ModeMatch
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
        encodedLength, modeMatch = instrSet.decodeInstruction(fetcher)
        if modeMatch is None:
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
            yield addr, modeMatch.substPC(pc, IntLiteral(postAddr))
            addr = postAddr
        fetcher = fetcher.advance(encodedLength)


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
