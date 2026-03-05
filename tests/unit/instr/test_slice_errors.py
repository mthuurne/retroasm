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
