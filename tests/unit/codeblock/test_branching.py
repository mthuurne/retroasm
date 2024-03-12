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

    - ERROR: error in body of function "duplicate_label": label "here" already defined
    """
    docstring_tester.check()


def test_branch_label_undefined(docstring_tester: DocstringTester) -> None:
    """
    An error is reported when a used label is not defined.

    .. code-block:: instr

        func missing_label()
            branch @there

    - ERROR: Label "there" does not exist
    """
    docstring_tester.check()
