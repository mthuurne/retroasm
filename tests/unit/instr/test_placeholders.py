from __future__ import annotations

from retroasm.expression import AddOperator, IntLiteral
from retroasm.mode import Mode, ModeMatch
from retroasm.reference import FixedValue
from retroasm.symbol import CurrentAddress
from retroasm.types import unlimited

from ..expression.utils import assert_int_literal, assert_trunc
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

    mode = instr_tester.parser.modes["forward"]
    (entry,) = mode.entries
    reg32 = instr_tester.parser.modes["reg32"]
    assert entry.match_placeholders == {"R": reg32}
    value_placeholders = entry.value_placeholders
    assert len(value_placeholders) == 0


def test_placeholder_mode_with_value(instr_tester: InstructionSetDocstringTester) -> None:
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


def test_placeholder_mode_duplicate(instr_tester: InstructionSetDocstringTester) -> None:
    """
    Placeholder names must be unique.

    .. code-block:: instr

        mode u32& test
        R . R . R . reg32 R, reg32 R

    .. code-block:: inputlog

        test.instr:2: ERROR: failed to define placeholder: name "R" redefined
        R . R . R . reg32 R, reg32 R
                          ~        ^

    """
    instr_tester.check()


def test_placeholder_mode_in_expr(instr_tester: InstructionSetDocstringTester) -> None:
    """
    Mode match placeholders cannot be used in context expressions.

    .. code-block:: instr

        mode u32& test
        R . R . R . reg32 R, u32 V = R

    .. code-block:: inputlog

        test.instr:2: ERROR: in placeholder "V" value: mode match placeholder "R" cannot be used in context value
        R . R . R . reg32 R, u32 V = R
                                     ^

    """
    instr_tester.check()


def test_placeholder_value_negative(instr_tester: InstructionSetDocstringTester) -> None:
    """
    A signed placeholder can have a negative constant value.

    .. code-block:: instr

        mode s8 signed_const
        .. N . s8 N = -123

    """
    instr_tester.check()

    mode: Mode = instr_tester.parser.modes["signed_const"]
    (entry,) = mode.entries

    (placeholder,) = entry.value_placeholders.values()
    assert_int_literal(placeholder.expr, -123)


def test_placeholder_value_reference(instr_tester: InstructionSetDocstringTester) -> None:
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

        test.instr:5: ERROR: failed to define placeholder: name "x" redefined
        x . x . x . int x
                        ^
        test.instr:2:
        int x
            ^
    """
    instr_tester.check()


def test_placeholder_constant_circular(instr_tester: InstructionSetDocstringTester) -> None:
    """
    A constant's expression cannot use a constant defined after it.
    This also avoids circular definitions.

    .. code-block:: instr

        mode u32& test
        ... int A = B, int B = A

    .. code-block:: inputlog

        test.instr:2: ERROR: in placeholder "A" value: unknown name "B"
        ... int A = B, int B = A
                    ^

    """
    instr_tester.check()


def test_placeholder_constant_stateful(instr_tester: InstructionSetDocstringTester) -> None:
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


def test_placeholder_constant_pc_relative(instr_tester: InstructionSetDocstringTester) -> None:
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
    value_placeholders = entry.value_placeholders
    assert len(value_placeholders) == 2
    placeholder_n = value_placeholders["N"]
    placeholder_a = value_placeholders["A"]
    expected = AddOperator(CurrentAddress(), placeholder_n.expr)
    assert_trunc(placeholder_a.expr, expected, unlimited, 32)


def test_placeholder_encode_func_ref(instr_tester: InstructionSetDocstringTester) -> None:
    """
    Function calls are supported in the encoding.

    .. code-block:: instr

        func pad(u8& N)
            def u16& ret = $0;N;$0

        mode u8 pad_imm
        pad(N) . N . N . u8 N

    """
    instr_tester.check()

    mode: Mode = instr_tester.parser.modes["pad_imm"]
    (entry,) = mode.entries

    mode_match = ModeMatch(entry, {"N": FixedValue(IntLiteral(0xEB), 8)}, {})
    (bits,) = mode_match.iter_bits()
    assert bits.width == 16
    assert bits.int_value == 0x0EB0


def test_placeholder_encode_func_val(instr_tester: InstructionSetDocstringTester) -> None:
    """
    Function calls that return something other than simple references are not supported.

    .. code-block:: instr

        func pad(u8& N)
            def u16 ret = $0;N;$0

        mode u8 pad_imm
        pad(N) . N . N . u8 N

    .. code-block:: inputlog

        test.instr:5: ERROR: unsupported operator in encoding: <<
        pad(N) . N . N . u8 N
        ^^^^^^
    """
    instr_tester.check()
