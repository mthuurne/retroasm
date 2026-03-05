from __future__ import annotations

from .conftest import InstructionSetDocstringTester


def test_slice_width_unknown(instr_tester: InstructionSetDocstringTester) -> None:
    """
    The width of a slice must be known at the time of definition.

    .. code-block:: instr

        func test(int UNKNOWN)
            a[:UNKNOWN]

    .. code-block:: inputlog

        test.instr:2: ERROR: invalid slice: slice width cannot be determined
            a[:UNKNOWN]
             ^^^^^^^^^^
    """
    instr_tester.check()


def test_slice_width_negative(instr_tester: InstructionSetDocstringTester) -> None:
    """
    The width of a slice cannot be negative.

    .. code-block:: instr

        func test()
            a[5:1]
            a[42:]

    .. code-block:: inputlog

        test.instr:2: ERROR: invalid slice: width (-4) cannot be negative
            a[5:1]
             ^^^^^
        test.instr:3: ERROR: invalid slice: width (-10) cannot be negative
            a[42:]
             ^^^^^
    """
    instr_tester.check()


def test_slice_offset_negative_literal(instr_tester: InstructionSetDocstringTester) -> None:
    """
    A slice offset that is a negative literal is rejected.

    .. code-block:: instr

        func test()
            a[-3]
            a[-3:5]

    .. code-block:: inputlog

        test.instr:2: ERROR: invalid bitwise lookup: slice offset (-3) cannot be negative
            a[-3]
             ^^^^
        test.instr:3: ERROR: invalid slice: slice offset (-3) cannot be negative
            a[-3:5]
             ^^^^^^
    """
    instr_tester.check()


def test_slice_offset_negative_mask(instr_tester: InstructionSetDocstringTester) -> None:
    """
    A slice offset that is unknown but might be negative is rejected.

    .. code-block:: instr

        func test(u8 U, s8 S)
            a[U]
            a[S]
            a[U:U+4]
            a[S:S+4]

    .. code-block:: inputlog

        test.instr:3: ERROR: invalid bitwise lookup: slice offset might be negative
            a[S]
             ^^^
        test.instr:5: ERROR: invalid slice: slice offset might be negative
            a[S:S+4]
             ^^^^^^^
    """
    instr_tester.check()
