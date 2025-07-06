from __future__ import annotations

import pytest

from retroasm.codeblock_builder import decompose_store
from retroasm.expression import AndOperator, IntLiteral, RShift
from retroasm.namespace import ReadOnlyNamespace
from retroasm.reference import Reference, SingleStorage
from retroasm.storage import Register
from retroasm.symbol import CurrentAddress
from retroasm.types import IntType

from .conftest import InstructionSetDocstringTester


def test_reg_multiple(instr_tester: InstructionSetDocstringTester) -> None:
    """
    Multiple registers of the same type can be declared without repeating the type.

    .. code-block:: instr

        reg
        u8 a, b, c

        io

    """
    instr_tester.check()
    namespace = instr_tester.parser.global_namespace
    assert namespace.keys() == {"a", "b", "c"}
    for name in namespace:
        reg = namespace[name]
        assert isinstance(reg, Reference), reg
        assert reg.type is IntType.u(8)


def test_reg_assign(instr_tester: InstructionSetDocstringTester) -> None:
    """
    Explicit register initialization is unsupported.

    TODO: If there are processors where the initialization value is not zero,
          it would be worth supporting assignment in register definitions.

    .. code-block:: instr

        reg
        u32 x := $12345678

    .. code-block:: inputlog

        test.instr:2: ERROR: bad register definition line: found assignment ":=" in an unexpected place
        u32 x := $12345678
              ^^

    """
    instr_tester.check()


def test_reg_const(instr_tester: InstructionSetDocstringTester) -> None:
    """
    A register can have a constant value.

    .. code-block:: instr

        reg
        int answer = 42

    """
    instr_tester.check()
    reg = instr_tester.parser.global_namespace["answer"]
    assert isinstance(reg, Reference), reg
    assert reg.type is IntType.int
    assert reg.bits.int_value == 42


def test_reg_alias(instr_tester: InstructionSetDocstringTester) -> None:
    """
    A register can be an alias of another.

    .. code-block:: instr

        reg
        u16 ip
        u16& pc = ip

    """
    instr_tester.check()
    reg_ip = instr_tester.parser.global_namespace["ip"]
    reg_pc = instr_tester.parser.global_namespace["pc"]
    assert reg_ip is reg_pc


def test_reg_alias_no_ref(instr_tester: InstructionSetDocstringTester) -> None:
    """
    A register alias must have a reference type.

    .. code-block:: instr

        reg
        u16 ip
        u16 pc = ip

    .. code-block:: inputlog

        test.instr:3: ERROR: bad register alias: bad value for constant "u16 pc": attempt to read state: reg16 ip
        u16 pc = ip
                 ^^

    """
    instr_tester.check()


def test_reg_base_ref(instr_tester: InstructionSetDocstringTester) -> None:
    """
    Base registers (as opposed to aliases) cannot have a reference type.

    .. code-block:: instr

        reg
        u32& x

    .. code-block:: inputlog

        test.instr:2: ERROR: base register cannot have a reference type
        u32& x
        ^^^^

    """
    instr_tester.check()


def test_reg_shadow_arg(instr_tester: InstructionSetDocstringTester) -> None:
    """
    A function argument is not allowed to shadow a register name.
    More generally, it is not allowed to shadow names from a parent namespace.

    .. code-block:: instr

        reg
        u32 x

        func shadow(u32 x)
            nop

    .. code-block:: inputlog

        test.instr:4: ERROR: error in argument "x" of function "shadow": name "x" redefined
        func shadow(u32 x)
                        ^
        test.instr:2:
        u32 x
            ^

    """
    instr_tester.check()


def test_reg_shadow_var(instr_tester: InstructionSetDocstringTester) -> None:
    """
    A local variable is not allowed to shadow a register name.
    More generally, it is not allowed to shadow names from a parent namespace.

    .. code-block:: instr

        reg
        u32 x

        func shadow()
            var u32 x

    .. code-block:: inputlog

        test.instr:5: ERROR: failed to declare variable "u32 x": name "x" redefined
            var u32 x
                    ^
        test.instr:2:
        u32 x
            ^

    """
    instr_tester.check()


def _assert_register(namespace: ReadOnlyNamespace, name: str) -> Register:
    ref = namespace[name]
    assert isinstance(ref, Reference), ref
    bits = ref.bits
    assert isinstance(bits, SingleStorage), bits
    storage = bits.storage
    assert isinstance(storage, Register), storage
    return storage


@pytest.mark.default_regs(False)
def test_reg_pc_base(instr_tester: InstructionSetDocstringTester) -> None:
    """
    Fixate the program counter as a base register.

    .. code-block:: instr

        reg
        u16 pc

    """
    namespace = instr_tester.parser.global_namespace
    pc = namespace.program_counter
    assert pc is not None
    pc_reg = _assert_register(namespace, "pc")
    assert decompose_store(pc, CurrentAddress()) == {
        pc_reg: AndOperator(CurrentAddress(), IntLiteral(0xFFFF))
    }


@pytest.mark.default_regs(False)
def test_reg_pc_alias(instr_tester: InstructionSetDocstringTester) -> None:
    """
    Fixate the program counter as a register alias.

    .. code-block:: instr

        reg
        u16 ip
        u16& pc = ip

    """
    namespace = instr_tester.parser.global_namespace
    pc = namespace.program_counter
    assert pc is not None
    ip_reg = _assert_register(namespace, "ip")
    assert decompose_store(pc, CurrentAddress()) == {
        ip_reg: AndOperator(CurrentAddress(), IntLiteral(0xFFFF))
    }


@pytest.mark.default_regs(False)
def test_reg_pc_complex(instr_tester: InstructionSetDocstringTester) -> None:
    """
    Fixate the program counter as a composite of registers and constants.

    .. code-block:: instr

        reg
        u8 pch, pcl
        u18& pc = %1 ; pch ; pcl ; %0

    """
    namespace = instr_tester.parser.global_namespace
    pc = namespace.program_counter
    assert pc is not None
    pch_reg = _assert_register(namespace, "pch")
    pcl_reg = _assert_register(namespace, "pcl")
    assert decompose_store(pc, CurrentAddress()) == {
        pch_reg: AndOperator(RShift(CurrentAddress(), 9), IntLiteral(0xFF)),
        pcl_reg: AndOperator(RShift(CurrentAddress(), 1), IntLiteral(0xFF)),
    }
