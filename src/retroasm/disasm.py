from __future__ import annotations

from collections.abc import Iterable, Iterator, Mapping, Sequence
from typing import cast

from .asm.directives import DataDirective
from .asm.formatter import Formatter
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

    __slots__ = ("_fetcher", "_addr", "_addr_step", "_delta_offset", "_max_offset")

    @classmethod
    def from_image_fetcher(cls, fetcher: ImageFetcher, addr: int) -> DisasmFetcher:
        return cls(fetcher, 0, addr, fetcher.num_bytes)

    def __init__(
        self, fetcher: AdvancingFetcher, delta: int, addr: int, addr_step: int
    ):
        super().__init__()
        self._fetcher = fetcher
        self._addr = addr
        self._addr_step = addr_step
        self._delta_offset = delta
        self._max_offset = -1

    @property
    def addr(self) -> int:
        return self._addr

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
        addr_step = self._addr_step
        new_addr = self._addr + steps * addr_step
        return DisasmFetcher(self, steps, new_addr, addr_step)

    def update(self, steps: int) -> None:
        """Advance the wrapped fetcher and reset the look ahead tracking."""
        if steps != 0:
            self._fetcher = self._fetcher.advance(steps)
            self._addr += steps * self._addr_step
        assert self._delta_offset == 0
        self._max_offset = -1


def disassemble(
    instr_set: InstructionSet, image_fetcher: ImageFetcher, start_addr: int
) -> Iterator[tuple[int, DataDirective | Instruction]]:
    """
    Disassemble instructions from the given fetcher.
    The fetched data is assumed to be code for the given instruction set,
    to be executed at the given address.
    """
    pc = cast(Reference, instr_set.global_namespace["pc"])
    fetcher = DisasmFetcher.from_image_fetcher(image_fetcher, start_addr)

    while fetcher[0] is not None:
        encoded_length, mode_match = instr_set.decode_instruction(fetcher)

        if mode_match is None:
            # Disassemble to data directives a number of encoding items equal to
            # how far the instruction decoder looked ahead.
            unused = fetcher.looked_ahead
            assert unused > 0
        else:
            # Verify that the opcodes produced when assembling are the same as
            # the ones we disassembled. This is not a given: there can be more
            # than one way to encode the same instruction.
            reencoded = tuple(instr_set.encode_instruction(mode_match))
            reencoded_len = len(reencoded)
            unused = encoded_length - reencoded_len
            if unused < 0 or any(
                fetcher[unused + idx] != enc for idx, enc in enumerate(reencoded)
            ):
                # Reject the match because it would break round trips.
                # TODO: Maybe this should be an option: there might be use cases where
                #       round trips are not important.
                mode_match = None
                unused = encoded_length

        # Disassemble unused encoding items to data directives.
        unused_items = []
        for idx in range(unused):
            value = fetcher[idx]
            if value is None:
                break
            unused_items.append(value)
        if unused_items:
            yield fetcher.addr, DataDirective.literal(
                instr_set.encoding_type.width, *unused_items
            )
        fetcher.update(unused)

        # Disassemble instruction.
        if mode_match is not None:
            pre_addr = fetcher.addr
            fetcher.update(reencoded_len)
            post_addr = fetcher.addr
            mode_match = mode_match.subst_pc(pc, IntLiteral(post_addr))
            yield pre_addr, Instruction(mode_match.mnemonic)


def format_asm(
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
