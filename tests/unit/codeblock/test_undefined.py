from __future__ import annotations

from .conftest import DocstringTester


def test_variable_undefined_compute(docstring_tester: DocstringTester) -> None:
    """
    An error is reported when a variable's undefined value is used in a computation.

    .. code-block:: instr

        func compute_undef()
            var int UNDEF
            a := a + UNDEF

    .. code-block:: inputlog

        ERROR: Undefined value of variable "UNDEF" is stored
    """
    docstring_tester.check()


def test_variable_undefined_ioindex(docstring_tester: DocstringTester) -> None:
    """
    An error is reported when a variable's undefined value is used as an I/O index.

    .. code-block:: instr

        func ioindex_undef()
            var int UNDEF
            a := mem[UNDEF]

    .. code-block:: inputlog

        ERROR: Undefined value of variable "UNDEF" is used as an I/O index
    """
    docstring_tester.check()


def test_variable_undefined_return_value(docstring_tester: DocstringTester) -> None:
    """
    An error is reported when a variable's undefined value is returned as a value.

    .. code-block:: instr

        func int return_undef()
            var int UNDEF
            ret := UNDEF

    .. code-block:: inputlog

        ERROR: Undefined value of variable "UNDEF" is returned
    """
    docstring_tester.check()


def test_variable_undefined_return_compute(docstring_tester: DocstringTester) -> None:
    """
    An error is reported when an expression that depends on variable's undefined value
    is returned as a value.

    .. code-block:: instr

        func int return_undef()
            var int UNDEF
            ret := UNDEF + 1

    .. code-block:: inputlog

        ERROR: Undefined value of variable "UNDEF" is returned
    """
    docstring_tester.check()


def test_variable_undefined_return_eliminate(docstring_tester: DocstringTester) -> None:
    """
    No error is reported when a variable's undefined value ultimately has no effect.

    .. code-block:: instr

        func int return_undef()
            var int UNDEF
            ret := UNDEF & 0
    """
    docstring_tester.check()


def test_variable_undefined_return_reference(docstring_tester: DocstringTester) -> None:
    """
    An error is reported when a variable's undefined value is returned as a reference.

    .. code-block:: instr

        func int& return_undef()
            var int UNDEF
            ret = UNDEF

    .. code-block:: inputlog

        ERROR: Undefined value of variable "UNDEF" is returned
    """
    docstring_tester.check()
