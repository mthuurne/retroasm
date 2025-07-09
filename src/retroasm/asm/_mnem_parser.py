from __future__ import annotations

from typing import TypeAlias, Union, override

from ..instrset import InstructionSet
from ..mode import MatchPlaceholder, Mode, ModeEntry
from ..reference import FixedValueReference
from ..utils import bad_type

MnemMatch: TypeAlias = Union[str, type[int], "Mode"]


class MnemTreeNode:
    """
    A node in a mnemonic match tree.

    A mnemonic match tree efficiently finds the matching mode entry for a given
    mnemonic sequence.
    """

    def __init__(self) -> None:
        self._children: dict[MnemMatch, MnemTreeNode] = {}
        self._leaves: list[ModeEntry] = []

    @override
    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self._children!r}, {self._leaves!r})"

    @staticmethod
    def _match_key(match: MnemMatch) -> tuple[int, str | None]:
        match match:
            case str() as text:
                return 0, text
            case Mode(name=name):
                return 1, name
            case type() as typ:
                assert typ is int
                return 2, None
            case match:
                bad_type(match)

    def dump(self, indent: str) -> None:
        for entry in self._leaves:
            tokens = " ".join(str(token) for token in entry.mnemonic)
            print(f"{indent}= {tokens}")
        children = self._children
        for match in sorted(children.keys(), key=self._match_key):
            print(f"{indent}+ {match}")
            children[match].dump(" " * len(indent) + "`---")

    def add_mode_entry(self, entry: ModeEntry) -> None:
        """Add the given mode entry to this tree."""
        node = self
        for token in entry.mnemonic:
            match: MnemMatch
            match token:
                case str() as text:
                    match = text
                case FixedValueReference():
                    match = int
                case MatchPlaceholder(mode=mode):
                    match = mode
                case token:
                    bad_type(token)
            try:
                node = node._children[match]
            except KeyError:
                node._children[match] = node = MnemTreeNode()
        node._leaves.append(entry)


def get_instruction_parser(instr_set: InstructionSet) -> MnemTreeNode:
    mnem_tree = MnemTreeNode()
    for entry in instr_set.entries:
        mnem_tree.add_mode_entry(entry)
    return mnem_tree
