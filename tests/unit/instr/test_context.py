from __future__ import annotations

from retroasm.mode import Mode

from .conftest import InstructionSetDocstringTester


def test_placeholder_unknown(instr_tester: InstructionSetDocstringTester) -> None:
    """
    A placeholder can represent a type or a mode; in this case it's neither.

    .. code-block:: instr

        mode u32& test
        ... badname X

    .. code-block:: inputlog

        test.instr:2: ERROR: there is no type or mode named "badname"
        ... badname X
            ^^^^^^^

    """
    instr_tester.check()


def test_placeholder_mode_forward(instr_tester: InstructionSetDocstringTester) -> None:
    """
    A mode match can be defined in the context.

    .. code-block:: instr

        mode u32& forward
        R . R . R . reg32 R

    """
    instr_tester.check()

    mode: Mode = instr_tester.parser.modes["forward"]
    (entry,) = mode.entries
    match_placeholders = list(entry.match_placeholders)
    assert len(match_placeholders) == 1
    value_placeholders = list(entry.value_placeholders)
    assert len(value_placeholders) == 0
    (placeholder,) = match_placeholders
    assert placeholder.name == "R"
    assert placeholder.mode.name == "reg32"


def test_placeholder_mode_with_value(
    instr_tester: InstructionSetDocstringTester,
) -> None:
    """
    A mode match placeholder cannot have a defined value.

    .. code-block:: instr

        mode u32& test
        R . R . R . reg32 R = a

    .. code-block:: inputlog

        test.instr:2: ERROR: mode match placeholder cannot have a defined value
        R . R . R . reg32 R = a
                            ^^^

    """
    instr_tester.check()


def test_placeholder_mode_duplicate(
    instr_tester: InstructionSetDocstringTester,
) -> None:
    """
    Placeholder names must be unique.

    .. code-block:: instr

        mode u32& test
        R . R . R . reg32 R, reg32 R

    .. code-block:: inputlog

        test.instr:2: ERROR: failed to define mode match placeholder: name "R" redefined
        R . R . R . reg32 R, reg32 R
                          ~        ^

    """
    instr_tester.check()


def test_placeholder_value_reference(
    instr_tester: InstructionSetDocstringTester,
) -> None:
    """
    A value placeholder cannot have a reference type.

    .. code-block:: instr

        mode u32& test
        N . N . N . u32& N

    .. code-block:: inputlog

        test.instr:2: ERROR: value placeholder cannot be a reference
        N . N . N . u32& N
                    ^^^^

    """
    instr_tester.check()


def test_placeholder_constant_circular(
    instr_tester: InstructionSetDocstringTester,
) -> None:
    """
    A constant's expression cannot use a constant defined after it.
    This also avoids circular definitions.

    .. code-block:: instr

        mode u32& test
        ... int A = B, int B = A

    .. code-block:: inputlog

        test.instr:2: ERROR: bad value for constant "int A": unknown name "B"
        ... int A = B, int B = A
                    ^

    """
    instr_tester.check()
