from typing import Iterator, cast

from retroasm.asm_directives import DataDirective
from retroasm.asm_formatter import Formatter
from retroasm.binfmt import Image
from retroasm.disasm import disassemble
from retroasm.fetch import ImageFetcher
from retroasm.instr import InstructionSet, builtinInstructionSets
from retroasm.section import ByteOrder

z80 = cast(InstructionSet, builtinInstructionSets["z80"])
createZ80Fetcher = ImageFetcher.factory(8, ByteOrder.little)

formatter = Formatter()


def disassemble_image(image: Image, addr: int = 0x4000) -> Iterator[tuple[int, str]]:
    """Disassemble the whole image as Z80 code, yield the statements as addr + text."""
    length = len(image)
    fetcher = createZ80Fetcher(image, 0, length)
    for stmt_addr, statement in disassemble(z80, fetcher, addr):
        assert addr <= stmt_addr < addr + length
        if isinstance(statement, DataDirective):
            stmt_str = formatter.data(statement)
        else:
            stmt_str = formatter.mnemonic(statement.mnemonic)
        yield stmt_addr, " ".join(stmt_str.split())


def test_disasm_empty() -> None:
    """Disassemble an empty image."""
    disassembled = list(disassemble_image(bytes()))
    assert disassembled == []


def test_disasm_ldir() -> None:
    """Disassemble a simple code fragment."""
    image = b"\x21\x00\x80" b"\x11\x00\xa0" b"\x01\x00\x20" b"\xed\xb0"
    disassembled = list(disassemble_image(image))
    assert disassembled == [
        (0x4000, "ld hl,$8000"),
        (0x4003, "ld de,$a000"),
        (0x4006, "ld bc,$2000"),
        (0x4009, "ldir"),
    ]


def test_disasm_rst() -> None:
    """
    Disassemble an instruction with an immediate value embedded in the opcode.

    Unlike most immediates, the address for the Z80 RST instruction is encoded
    in the first and only opcode byte.
    """
    image = b"\xc7\xcf\xd7\xdf\xe7\xef\xf7\xff"
    disassembled = list(disassemble_image(image))
    assert disassembled == [
        (0x4000, "rst $00"),
        (0x4001, "rst $08"),
        (0x4002, "rst $10"),
        (0x4003, "rst $18"),
        (0x4004, "rst $20"),
        (0x4005, "rst $28"),
        (0x4006, "rst $30"),
        (0x4007, "rst $38"),
    ]


def test_disasm_fixed_numbers() -> None:
    """
    Disassemble instructions with fixed numbers.

    These numbers are implied by the instruction rather than explicitly encoded.
    While the values are fixed, the numbers are expressions rather than strings
    to support constructs like "OUT (C),$00" or "INT_MODE: EQU 2 ... IM INT_MODE".
    """
    image = b"\xed\x46\xed\x56\xed\x5e\xed\x71"
    disassembled = list(disassemble_image(image))
    assert disassembled == [
        (0x4000, "im 0"),
        (0x4002, "im 1"),
        (0x4004, "im 2"),
        (0x4006, "out (c),0"),
    ]


def test_disasm_push_ix_pop_iy() -> None:
    """Disassemble code using prefixes."""
    image = b"\xdd\xe5" b"\xfd\xe1"
    disassembled = list(disassemble_image(image))
    assert disassembled == [
        (0x4000, "push ix"),
        (0x4002, "pop iy"),
    ]


def test_disasm_bad_opcode() -> None:
    """Disassemble an illegal instruction."""
    image = b"\xed\x0c"
    disassembled = list(disassemble_image(image))
    assert disassembled == [
        (0x4000, "db $ed, $0c"),
    ]


def test_disasm_multiple_opcode() -> None:
    """Disassemble an instruction that has more than one possible opcode."""
    image = b"\x2a\x34\x12" b"\xed\x6b\x78\x56"
    disassembled = list(disassemble_image(image))
    assert disassembled == [
        (0x4000, "ld hl,($1234)"),
        # TODO: Using a DW for the address would be even better.
        (0x4003, "db $ed, $6b, $78, $56"),
    ]


def test_disasm_truncated_instr() -> None:
    """Disassemble a code fragment containing a partial instruction."""
    image = b"\xed"
    disassembled = list(disassemble_image(image))
    assert disassembled == [
        (0x4000, "db $ed"),
    ]


def test_disasm_truncated_prefix() -> None:
    """Disassemble a code fragment containing only prefixes."""
    image = b"\xdd\xfd"
    disassembled = list(disassemble_image(image))
    assert disassembled == [
        (0x4000, "db $dd"),
        (0x4001, "db $fd"),
    ]


def test_disasm_redundant_prefix() -> None:
    """Disassemble a code fragment containing a redundant prefix."""
    image = b"\xdd\xf5"
    disassembled = list(disassemble_image(image))
    assert disassembled == [
        (0x4000, "db $dd"),
        (0x4001, "push af"),
    ]


def test_disasm_repeated_prefix() -> None:
    """Disassemble a code fragment containing a repeated prefix."""
    image = b"\xfd\xfd\x21\xcd\xab"
    disassembled = list(disassemble_image(image))
    assert disassembled == [
        (0x4000, "db $fd"),
        (0x4001, "ld iy,$abcd"),
    ]


def test_disasm_overridden_prefix() -> None:
    """Disassemble a code fragment containing an overridden prefix."""
    image = b"\xfd\xdd\x21\xcd\xab"
    disassembled = list(disassemble_image(image))
    assert disassembled == [
        (0x4000, "db $fd"),
        (0x4001, "ld ix,$abcd"),
    ]


def test_disasm_negative_offset() -> None:
    """Disassemble a code fragment using a negative offset."""
    image = b"\xdd\x77\xfe"
    disassembled = list(disassemble_image(image))
    assert disassembled == [
        (0x4000, "ld (ix-2),a"),
    ]
