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
    A warning is reported when a label is not used.

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
    Branches at the CPU level are conditional writes to the program counter.

    .. code-block:: instr

        func test(u32 A, u1 F)
            branch !F @skip
            pc := A
            @skip

    .. code-block:: dump

            load from A
            load from F
            goto @2 if !load(F)
                 @1 if load(F)
        @1
            store load(A) in reg32 pc
            goto @2
        @2
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
            goto @2 if sign((load(reg32 a) + -8))
                 @1 if !sign((load(reg32 a) + -8))
        @1
            store 8 in reg32 b
            goto @3
        @2
            store load(reg32 a) in reg32 b
            goto @3
        @3
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

        @0
            load from reg32 a
            load from reg32 b
            goto @2 if !(load(reg32 a) ^ load(reg32 b))
                 @1 if !!(load(reg32 a) ^ load(reg32 b))
        @1
            store (load(reg32 a) + 1)[:32] in reg32 a
            goto @0 if sign(((load(reg32 a) + 1)[:32] + -8))
                 @2 if !sign(((load(reg32 a) + 1)[:32] + -8))
        @2
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
            goto @1 if (!load(F) | load(F))
        @1
            return 0
    """
    codeblock_tester.check()


def test_branch_halfway_loop(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    A proper graph is produced when the actual entry point is halfway a loop.

    Note that the graph linearization reverses the order of the two halves of the loop body.
    While this would be less efficient during execution, for analysis it makes no difference
    as the graph is the same.

    .. code-block:: instr

        func test()
            branch @halfway
            @loop
            b := b + a
            @halfway
            a := a - 1
            branch a == 0 @loop

    .. code-block:: dump

        @0
            load from reg32 a
            store (load(reg32 a) + $FFFFFFFF)[:32] in reg32 a
            goto @1 if !(load(reg32 a) + $FFFFFFFF)[:32]
                 @2 if !!(load(reg32 a) + $FFFFFFFF)[:32]
        @1
            load from reg32 b
            store (load(reg32 b) + load(reg32 a) + $FFFFFFFF)[:32] in reg32 b
            goto @0
        @2
    """
    codeblock_tester.check()
