from __future__ import annotations

from typing import Iterable, Iterator, Mapping, Sequence, cast

from .asm_directives import DataDirective
from .asm_formatter import Formatter
from .expression import IntLiteral
from .fetch import AdvancingFetcher, ImageFetcher
from .instrset import InstructionSet
from .reference import FixedValueReference, Reference


class Instruction:
    @property
    def mnemonic(self) -> Sequence[str | FixedValueReference]:
        return self._mnemonic

    def __init__(self, mnemonic: Iterable[str | FixedValueReference]):
        self._mnemonic = tuple(mnemonic)


class DisasmFetcher(AdvancingFetcher):
    """Wraps an image fetcher and tracks how far it looks ahead."""

    __slots__ = ("_fetcher", "_delta_offset", "_max_offset")

    def __init__(self, fetcher: AdvancingFetcher, delta: int = 0):
        super().__init__()
        self._fetcher = fetcher
        self._delta_offset = delta
        self._max_offset = -1

    @property
    def looked_ahead(self) -> int:
        """
        Return the number of encoding units that was fetched ahead of the start
        position.

        This is used as the number of encoding units consumed by the instruction
        decoder if no instruction was matched.
        """
        return self._max_offset + 1

    def __getitem__(self, index: int, /) -> int | None:
        self._max_offset = max(self._max_offset, index)
        return self._fetcher[self._delta_offset + index]

    def advance(self, steps: int = 1) -> DisasmFetcher:
        return DisasmFetcher(self, steps)

    def update(self, steps: int) -> None:
        """Advance the wrapped fetcher and reset the look ahead tracking."""
        if steps != 0:
            self._fetcher = self._fetcher.advance(steps)
        assert self._delta_offset == 0
        self._max_offset = -1


def disassemble(
    instrSet: InstructionSet, imageFetcher: ImageFetcher, startAddr: int
) -> Iterator[tuple[int, DataDirective | Instruction]]:
    """
    Disassemble instructions from the given fetcher.
    The fetched data is assumed to be code for the given instruction set,
    to be executed at the given address.
    """
    pc = cast(Reference, instrSet.globalNamespace["pc"])
    numBytes = imageFetcher.numBytes
    fetcher = DisasmFetcher(imageFetcher)

    addr = startAddr
    while fetcher[0] is not None:
        encodedLength, modeMatch = instrSet.decodeInstruction(fetcher)

        if modeMatch is None:
            # Disassemble to data directives a number of encoding items equal to
            # how far the instruction decoder looked ahead.
            unused = fetcher.looked_ahead
            assert unused > 0
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
        unused_items = []
        for idx in range(unused):
            value = fetcher[idx]
            if value is None:
                break
            unused_items.append(value)
        if unused_items:
            yield addr, DataDirective.literal(instrSet.encodingType, *unused_items)
        addr += unused * numBytes
        fetcher.update(unused)

        # Disassemble instruction.
        if modeMatch is not None:
            postAddr = addr + reencodedLen * numBytes
            modeMatch = modeMatch.substPC(pc, IntLiteral(postAddr))
            yield addr, Instruction(modeMatch.mnemonic)
            addr = postAddr
            fetcher.update(reencodedLen)


def formatAsm(
    formatter: Formatter,
    decoded: Iterable[tuple[int, DataDirective | Instruction]],
    labels: Mapping[int, str],
) -> Iterator[str]:
    for addr, statement in decoded:
        label = labels.get(addr)
        if label is not None:
            yield formatter.label(label)
        if isinstance(statement, DataDirective):
            yield formatter.data(statement)
        else:
            yield formatter.mnemonic(statement.mnemonic)
