from __future__ import annotations

from .conftest import InstructionSetDocstringTester


def test_branch_label_duplicate(instr_tester: InstructionSetDocstringTester) -> None:
    """
    An error is reported when the same label is defined more than once.

    .. code-block:: instr

        func duplicate_label()
            @here
            a := 0
            @here

    .. code-block:: inputlog

        test.instr:4: ERROR: error in body of function "duplicate_label": label "here" already defined
            @here
            ^^^^^
        test.instr:2:
            @here
            ^^^^^
    """
    instr_tester.check()


def test_branch_label_undefined(instr_tester: InstructionSetDocstringTester) -> None:
    """
    An error is reported when a used label is not defined.

    .. code-block:: instr

        func missing_label()
            branch @there

    .. code-block:: inputlog

        test.instr:2: ERROR: Label "there" does not exist
            branch @there
                   ^^^^^^
    """
    instr_tester.check()


def test_branch_label_unused(instr_tester: InstructionSetDocstringTester) -> None:
    """
    An warning is reported when a label is not used.

    .. code-block:: instr

        func unused_label()
            @unused
            branch a == 0 @exit
            a := a - 1
            @exit

    .. code-block:: inputlog

        test.instr:2: warning: Label "unused" is unused
            @unused
            ^^^^^^^
    """
    instr_tester.check()
