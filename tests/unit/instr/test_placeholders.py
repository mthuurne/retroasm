from __future__ import annotations

from retroasm.encoding import EncodingExpr, EncodingMultiMatch
from retroasm.expression import AddOperator, IntLiteral
from retroasm.mode import ModeMatch
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
    (row,) = mode.rows
    reg32 = instr_tester.parser.modes["reg32"]
    assert row.match_placeholders == {"R": reg32}
    value_placeholders = row.value_placeholders
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

    mode = instr_tester.parser.modes["signed_const"]
    (row,) = mode.rows

    (placeholder,) = row.value_placeholders.values()
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

    mode = instr_tester.parser.modes["pc_rel"]
    (row,) = mode.rows
    value_placeholders = row.value_placeholders
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

    mode = instr_tester.parser.modes["pad_imm"]
    (row,) = mode.rows

    mode_match = ModeMatch(row, {"N": FixedValue(IntLiteral(0xEB), 8)}, {})
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
    instr_tester.create_instruction_set(expect_fail=True)


def test_placeholder_encode_extend(instr_tester: InstructionSetDocstringTester) -> None:
    """
    An 8-bit immediate value in a 16-bit reference takes up 16 bits in the encoding.

    .. code-block:: instr

        func extend(u8& N)
            def u16 V = N
            def u16& ret = V

        mode u8 extend_imm
        extend(N) . N . N . u8 N
    """
    instr_tester.check()

    mode = instr_tester.parser.modes["extend_imm"]
    (row,) = mode.rows

    mode_match = ModeMatch(row, {"N": FixedValue(IntLiteral(0x94), 8)}, {})
    (bits,) = mode_match.iter_bits()
    assert bits.width == 16
    assert bits.int_value == 0x0094


def test_placeholder_encode_multimatch_empty(
    instr_tester: InstructionSetDocstringTester,
) -> None:
    """
    A multi-match placeholder for a mode with an empty encoding results in a warning and
    empty encoding in the mode row that contained the placeholder.

    A real-life example of a mode with an empty encoding is the Z80 mode that selects
    HL, IX or IY depending on the active decode flags.

    .. code-block:: instr

        mode u32& just_a
        . a

        mode u32& still_just_a
        R@ . . R . just_a R

    .. code-block:: inputlog

        test.instr:5: warning: mode "just_a" matched by "R@" does not contain encoding elements
        R@ . . R . just_a R
        ^^                ~
    """
    instr_tester.check()

    mode = instr_tester.parser.modes["still_just_a"]
    (row,) = mode.rows

    assert len(row.encoding.items) == 0


def test_placeholder_encode_multimatch_no_aux(
    instr_tester: InstructionSetDocstringTester,
) -> None:
    """
    A regular plus a multi-match placeholder for a mode with no auxiliary encoding items
    results in a warning and just the regular placeholder in the mode row.

    .. code-block:: instr

        mode u4 imm4
        N . N . N . u4 N

        mode u16 single_bit_mask
        N, N@ . bit N . 1 << N . imm4 N

    .. code-block:: inputlog

        test.instr:5: warning: mode "imm4" matched by "N@" does not contain auxiliary encoding units
        N, N@ . bit N . 1 << N . imm4 N
           ^^                         ~
    """
    instr_tester.check()

    mode = instr_tester.parser.modes["single_bit_mask"]
    (row,) = mode.rows

    assert len(row.encoding.items) == 1
    (enc_item,) = row.encoding.items
    assert isinstance(enc_item, EncodingExpr)
    assert enc_item.encoding_width == 4


def test_placeholder_encode_multimatch_single_item(
    instr_tester: InstructionSetDocstringTester,
) -> None:
    """
    A multi-match placeholder for a mode with a single encoding item results in
    just the regular placeholder in the mode row.

    No warning will be issued: while placeholder could have been regular rather than
    multi-match, using a multi-match placeholder might be preferred for consistency.

    .. code-block:: instr

        mode s8 imm8
        N . N . N . s8 N

        mode s8 signed_byte
        N@ . N . N . imm8 N
    """
    instr_tester.check()

    mode = instr_tester.parser.modes["signed_byte"]
    (row,) = mode.rows

    assert len(row.encoding.items) == 1
    (enc_item,) = row.encoding.items
    assert isinstance(enc_item, EncodingExpr)


def test_placeholder_encode_multimatch_mixed_width(
    instr_tester: InstructionSetDocstringTester,
) -> None:
    """
    A regular and multi-match placeholder for a mode can each have a different width.

    .. code-block:: instr

        mode u32& reg_offset
        R, N . R + N . (R + N)[:32] . reg32 R, u16 N

        mode u32& reg_offset_encpad
        %0;RO, RO@ . RO . RO . reg_offset RO
    """
    instr_tester.check()

    mode = instr_tester.parser.modes["reg_offset_encpad"]
    (row,) = mode.rows

    assert len(row.encoding.items) == 2
    (primary_item, aux_item) = row.encoding.items
    assert isinstance(primary_item, EncodingExpr)
    assert primary_item.encoding_width == 3
    assert isinstance(aux_item, EncodingMultiMatch)
    assert aux_item.encoding_width == 16
