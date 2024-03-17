from __future__ import annotations

from .conftest import DocstringTester


def test_branch_label_duplicate(docstring_tester: DocstringTester) -> None:
    """
    An error is reported when the same label is defined more than once.

    .. code-block:: instr

        func duplicate_label()
            @here
            a := 0
            @here

    .. code-block:: inputlog

        ERROR: error in body of function "duplicate_label": label "here" already defined
    """
    docstring_tester.check()


def test_branch_label_undefined(docstring_tester: DocstringTester) -> None:
    """
    An error is reported when a used label is not defined.

    .. code-block:: instr

        func missing_label()
            branch @there

    .. code-block:: inputlog

        ERROR: Label "there" does not exist
    """
    docstring_tester.check()


def test_branch_label_unused(docstring_tester: DocstringTester) -> None:
    """
    An warning is reported when a label is not used.

    .. code-block:: instr

        func unused_label()
            @unused
            branch a == 0 @exit
            a := a - 1
            @exit

    .. code-block:: inputlog

        warning: Label "unused" is unused
    """
    docstring_tester.check()
