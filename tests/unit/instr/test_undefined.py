from __future__ import annotations

from .conftest import InstructionSetDocstringTester


def test_variable_undefined_compute(
    instr_tester: InstructionSetDocstringTester,
) -> None:
    """
    An error is reported when a variable's undefined value is used in a computation.

    .. code-block:: instr

        func compute_undef()
            var int UNDEF
            a := a + UNDEF

    .. code-block:: inputlog

        test.instr:3: ERROR: Undefined value of variable "UNDEF" is stored
            a := a + UNDEF
                     ^^^^^
    """
    instr_tester.check()


def test_variable_undefined_ioindex(
    instr_tester: InstructionSetDocstringTester,
) -> None:
    """
    An error is reported when a variable's undefined value is used as an I/O index.

    .. code-block:: instr

        func ioindex_undef()
            var int UNDEF
            a := mem[UNDEF]

    .. code-block:: inputlog

        test.instr:3: ERROR: Undefined value of variable "UNDEF" is used as an I/O index
            a := mem[UNDEF]
                     ^^^^^
    """
    instr_tester.check()


def test_variable_undefined_return_value(
    instr_tester: InstructionSetDocstringTester,
) -> None:
    """
    An error is reported when a variable's undefined value is returned as a value.

    .. code-block:: instr

        func return_undef()
            var int UNDEF
            def int ret = UNDEF

    .. code-block:: inputlog

        test.instr:3: ERROR: Undefined value of variable "UNDEF" is returned
            def int ret = UNDEF
                          ^^^^^
    """
    instr_tester.check()


def test_variable_undefined_return_compute(
    instr_tester: InstructionSetDocstringTester,
) -> None:
    """
    An error is reported when an expression that depends on variable's undefined value
    is returned as a value.

    .. code-block:: instr

        func return_undef()
            var int UNDEF
            def int ret = UNDEF + 1

    .. code-block:: inputlog

        test.instr:3: ERROR: Undefined value of variable "UNDEF" is returned
            def int ret = UNDEF + 1
                          ^^^^^
    """
    instr_tester.check()


def test_variable_undefined_return_eliminate(
    instr_tester: InstructionSetDocstringTester,
) -> None:
    """
    No error is reported when a variable's undefined value ultimately has no effect.

    .. code-block:: instr

        func return_undef()
            var int UNDEF
            def int ret = UNDEF & 0
    """
    instr_tester.check()


def test_variable_undefined_return_reference(
    instr_tester: InstructionSetDocstringTester,
) -> None:
    """
    An error is reported when a variable's undefined value is returned as a reference.

    .. code-block:: instr

        func return_undef()
            var int UNDEF
            def int& ret = UNDEF

    .. code-block:: inputlog

        test.instr:1: ERROR: Undefined value of variable "UNDEF" is returned
        func return_undef()
             ^^^^^^^^^^^^
    """
    instr_tester.check()
