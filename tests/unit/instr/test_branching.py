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


def test_branch_dump_conditional_jump(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    TODO: The local variable should be eliminated.

    .. code-block:: instr

        func test(u32 A, u1 F)
            branch !F @skip
            pc := A
            @skip

    .. code-block:: dump

            load from A
            store load(A) in var32 A
            load from F
            store load(F) in var1 F
            goto @skip if !load(F)
                 @1 if load(F)
        @1
            store load(A) in reg32 pc
            goto @skip
        @skip
    """
    codeblock_tester.check()


def test_branch_dump_min(codeblock_tester: CodeBlockDocstringTester) -> None:
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

    .. code-block:: dump

            load from reg32 a
            goto @small if sign((load(reg32 a) + -8))
                 @1 if !sign((load(reg32 a) + -8))
        @1
            store 8 in reg32 b
            goto @end
        @small
            store load(reg32 a) in reg32 b
            goto @end
        @end
    """
    codeblock_tester.check()


def test_branch_redundant_blocks(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    The final graph has no empty blocks before `@loop` and `@end`.

    .. code-block:: instr

        func test()
            @loop
            branch a == b @end
            a := a + 1
            branch a < 8 @loop
            @end

    .. code-block:: dump

        @loop
            load from reg32 a
            load from reg32 b
            goto @end if !(load(reg32 a) ^ load(reg32 b))
                 @0 if !!(load(reg32 a) ^ load(reg32 b))
        @0
            store (load(reg32 a) + 1)[:32] in reg32 a
            goto @loop if sign(((load(reg32 a) + 1)[:32] + -8))
                 @end if !sign(((load(reg32 a) + 1)[:32] + -8))
        @end
    """
    codeblock_tester.check()


def test_branch_unused_load_stores(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    Unused load and store operations can be eliminated across branches.

    .. code-block:: instr

        func test(u1 F)
            var u32 I
            branch F @pick_b
            I := a
            branch @read
            @pick_b
            I  := b
            @read
            def u32 ret = ram[I] & 0

    TODO: Eventually, this should be reducible to just "load from F" and "return 0".

    .. code-block:: dump

            load from F
            store load(F) in var1 F
            goto @pick_b if load(F)
                 @1 if !load(F)
        @1
            load from reg32 a
            store load(reg32 a) in var32 I
            goto @read
        @pick_b
            load from reg32 b
            store load(reg32 b) in var32 I
            goto @read
        @read
            return 0
    """
    codeblock_tester.check()
