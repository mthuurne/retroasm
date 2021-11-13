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
            unused = max(encodedLength, 1)
        else:
            # Verify that the opcodes produced when assembling are the same as
            # the ones we disassembled. This is not a given: there can be more
            # than one way to encode the same instruction.
            reencoded = tuple(instrSet.encodeInstruction(modeMatch))
            reencodedLen = len(reencoded)
            unused = encodedLength - reencodedLen
            if unused < 0 or any(
                fetcher[unused + idx] != enc for idx, enc in enumerate(reencoded)
            ):
                # Reject the match because it would break round trips.
                # TODO: Maybe this should be an option: there might be use cases where
                #       round trips are not important.
                modeMatch = None
                unused = encodedLength

        # Disassemble unused encoding items to data directives.
        for idx in range(unused):
            value = fetcher[idx]
            assert value is not None
            yield addr, DataDirective.literal(instrSet.encodingType, value)
            addr += numBytes
        fetcher = fetcher.advance(unused)

        # Disassemble instruction.
        if modeMatch is not None:
            postAddr = addr + reencodedLen * numBytes
            yield addr, modeMatch.substPC(pc, IntLiteral(postAddr))
            addr = postAddr
            fetcher = fetcher.advance(reencodedLen)


def formatAsm(
    formatter: Formatter,
    decoded: Iterable[tuple[int, DataDirective | ModeMatch]],
    labels: Mapping[int, str],
) -> Iterator[str]:
    for addr, match in decoded:
        label = labels.get(addr)
        if label is not None:
            yield formatter.label(label)
        if isinstance(match, DataDirective):
            yield formatter.data(match)
        else:
            yield formatter.mnemonic(match.mnemonic, labels)
