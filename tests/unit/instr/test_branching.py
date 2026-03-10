from __future__ import annotations

from .conftest import CodeBlockDocstringTester, InstructionSetDocstringTester


def test_branch_label_duplicate(instr_tester: InstructionSetDocstringTester) -> None:
    """
    An error is reported when the same label is defined more than once.

    .. code-block:: instr

        func duplicate_label()
            @here
            a := 0
            @here

    .. code-block:: inputlog

        test.instr:4: ERROR: label "here" already defined
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


# TODO: Add a test case for unreachable code detection.


def test_branch_dump(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    The dump format can handle a branch.

    .. code-block:: instr

        func test()
            branch a < 8 @small
            b := 8
            branch 1 @end
            @small
            b := a
            @end

    TODO: The load in the @small block can be eliminated in cross-block simplification.

    .. code-block:: dump

            load from reg32 a
            goto @small if sign((load(reg32 a) + -8))
                 @1 if !sign((load(reg32 a) + -8))
        @1
            store 8 in reg32 b
            goto @end
        @small
            load from reg32 a
            store load(reg32 a) in reg32 b
            goto @end
        @end
    """
    codeblock_tester.check()
