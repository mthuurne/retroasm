from __future__ import annotations

from retroasm.expression import AddOperator, SignExtension
from retroasm.mode import Mode
from retroasm.symbol import CurrentAddress
from retroasm.types import unlimited

from ..expression.utils import assert_trunc
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


def test_placeholder_mode_in_expr(
    instr_tester: InstructionSetDocstringTester,
) -> None:
    """
    Mode match placeholders cannot be used in context expressions.

    .. code-block:: instr

        mode u32& test
        R . R . R . reg32 R, u32 V = R

    .. code-block:: inputlog

        test.instr:2: ERROR: bad value for constant "u32 V": mode match placeholder "R" cannot be used in context value
        R . R . R . reg32 R, u32 V = R
                                     ^

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


def test_placeholder_value_shadow(instr_tester: InstructionSetDocstringTester) -> None:
    """
    Placeholder names must not shadow global names.

    .. code-block:: instr

        reg
        int x

        mode int test
        x . x . x . int x

    .. code-block:: inputlog

        test.instr:5: ERROR: failed to define value placeholder: name "x" redefined
        x . x . x . int x
                        ^
        test.instr:2:
        int x
            ^
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


def test_placeholder_constant_stateful(
    instr_tester: InstructionSetDocstringTester,
) -> None:
    """
    A constant's expression cannot depend on state such as register contents.

    .. code-block:: instr

        mode u32 test
        . a . A . u32 A = a

    .. code-block:: inputlog

        test.instr:2: ERROR: state cannot be accessed from the context: load from reg32 a
        . a . A . u32 A = a
                          ^

    """
    instr_tester.check()


def test_placeholder_constant_pc_relative(
    instr_tester: InstructionSetDocstringTester,
) -> None:
    """
    The value of the program counter is considered a constant;
    accessing it from the context is allowed.

    .. code-block:: instr

        mode u32 pc_rel
        N . A . A . s16 N, u32 A = pc + N

    """
    instr_tester.check()

    mode: Mode = instr_tester.parser.modes["pc_rel"]
    (entry,) = mode.entries
    value_placeholders = list(entry.value_placeholders)
    assert len(value_placeholders) == 2
    (placeholder_n,) = (p for p in value_placeholders if p.name == "N")
    (placeholder_a,) = (p for p in value_placeholders if p.name == "A")
    expected = AddOperator(SignExtension(placeholder_n.expr, 16), CurrentAddress())
    assert_trunc(placeholder_a.expr, expected, unlimited, 32)
