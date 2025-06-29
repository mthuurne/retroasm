from __future__ import annotations

import pytest

from retroasm.reference import Reference
from retroasm.types import IntType

from .conftest import InstructionSetDocstringTester


@pytest.mark.default_regs(False)
def test_reg_multiple(instr_tester: InstructionSetDocstringTester) -> None:
    """
    Multiple registers of the same type can be declared without repeating the type.

    .. code-block:: instr

        reg
        u8 a, b, c

    """
    instr_tester.check()
    namespace = instr_tester.parser.global_namespace
    assert namespace.keys() == {"a", "b", "c"}
    for name in namespace:
        reg = namespace[name]
        assert isinstance(reg, Reference), reg
        assert reg.type is IntType.u(8)


@pytest.mark.default_regs(False)
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


@pytest.mark.default_regs(False)
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


@pytest.mark.default_regs(False)
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


@pytest.mark.default_regs(False)
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


@pytest.mark.default_regs(False)
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
