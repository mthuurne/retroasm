from __future__ import annotations

from .conftest import CodeBlockDocstringTester


def test_store_truncate(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    A stored value is truncated to the storage width.

    .. code-block:: instr

        func test(u8& R, int V)
            R := V

    .. code-block:: dump

        load from V
        store load(V)[:8] in R
    """
    codeblock_tester.check()


def test_signed_load_extend(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    Loading from a limited-width signed integer introduces sign extension.

    .. code-block:: instr

        func test(s8& S)
            def int ret = S

    .. code-block:: dump

        load from S
        return s8(load(S))
    """
    codeblock_tester.check()


def test_signed_load_unlimited(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    Load an unlimited width integer.

    No sign extension should be happening here, but the code once contained a bug where
    it would try to apply sign extension and triggered an internal consistency check.

    .. code-block:: instr

        func test(int& S)
            def int ret = S

    .. code-block:: dump

        load from S
        return load(S)
    """
    codeblock_tester.check()


def test_signed_load_wrap(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    Store an unsigned integer and load it as signed.

    .. code-block:: instr

        func test()
            var s8 S := 135
            def int ret = S

    .. code-block:: dump

        return -121
    """
    codeblock_tester.check()


def test_decompose_load_basic_concat(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    Concatenated references are loaded from least to most significant.

    .. code-block:: instr

        func test(u7& R0, u3& R1, u13& R2)
            def int ret = R2;R1;R0

    .. code-block:: dump

        load from R0
        load from R1
        load from R2
        return ((load(R2) << 10) | (load(R1) << 7) | load(R0))
    """
    codeblock_tester.check()


def test_decompose_store_basic_concat(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    Concatenated references are stored from least to most significant.

    .. code-block:: instr

        func test(u7& R0, u3& R1, u13& R2, int V)
            R2;R1;R0 := V

    .. code-block:: dump

        load from V
        store load(V)[:7] in R0
        store load(V)[7:10] in R1
        store load(V)[10:23] in R2
    """
    codeblock_tester.check()


def test_decompose_load_self_concat(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    The same reference can occur multiple times in a concatenation.

    .. code-block:: instr

        func test(u8& R0)
            def int ret = R0;R0

    .. code-block:: dump

        load from R0
        load from R0
        return ((load(R0) << 8) | load(R0))
    """
    codeblock_tester.check()


def test_decompose_store_self_concat(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    The same reference can occur multiple times in a concatenation.

    .. code-block:: instr

        func test(u8& R0, int V)
            R0;R0 := V

    .. code-block:: dump

        load from V
        store load(V)[:8] in R0
        store load(V)[8:16] in R0
    """
    codeblock_tester.check()


def test_decompose_load_basic_slice(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    Loading a slice is done by loading the whole and then slicing the value.

    .. code-block:: instr

        func test(u8& R0)
            def u3& S0 = R0[2:5]
            def int ret = S0

    .. code-block:: dump

        load from R0
        return load(R0)[2:5]
    """
    codeblock_tester.check()


def test_decompose_store_basic_slice(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    Storing a slice is done by loading the whole, updating the sliced bits and writing back
    the result.

    .. code-block:: instr

        func test(u8& R0, int V)
            def u3& S0 = R0[2:5]
            S0 := V

    .. code-block:: dump

        load from V
        load from R0
        store (((load(V) << 2) & $1C) | (load(R0) & $E3)) in R0
    """
    codeblock_tester.check()


def test_decompose_load_slice_extend_unsigned(
    codeblock_tester: CodeBlockDocstringTester,
) -> None:
    """
    Loading a slice beyond an unsigned reference's width will put zeroes into the extra bits.

    This behavior was picked because it supports a dynamic offset (known only at runtime)
    while still using a statically defined slice width.
    Truncating the slice would make the width dynamic, while making it an error to slice
    beyond the reference's width couldn't be enforced statically when the offset is dynamic.

    .. code-block:: instr

        func test(u8& R0)
            def u28& S0 = R0[2:30]
            def int ret = S0

    .. code-block:: dump

        load from R0
        return load(R0)[2:]
    """
    codeblock_tester.check()


def test_decompose_load_slice_extend_signed(
    codeblock_tester: CodeBlockDocstringTester,
) -> None:
    """
    Loading a slice beyond a signed reference's width will sign extend into the extra bits.

    .. code-block:: instr

        func test(s8& R0)
            def u28& S0 = R0[2:30]
            def int ret = S0

    .. code-block:: dump

        load from R0
        return s8(load(R0))[2:30]
    """
    codeblock_tester.check()


def test_decompose_store_slice_extend_unsigned(
    codeblock_tester: CodeBlockDocstringTester,
) -> None:
    """
    Storing a slice beyond an unsigned reference's width will extend with zero bits.

    .. code-block:: instr

        func test(u8& R0, int V)
            def u28& S0 = R0[2:30]
            S0 := V

    TODO: The mask applied to V preserves 28 bits, but R0 can only store 6 of them.

    .. code-block:: dump

        load from V
        load from R0
        store (((load(V) << 2) & $3FFFFFFC) | load(R0)[:2]) in R0
    """
    codeblock_tester.check()


def test_decompose_load_slice_outside(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    A slice outside of a reference is loaded and then ignored.

    .. code-block:: instr

        func test(u8& R0)
            def u30& S0 = R0[12:42]
            def int ret = S0

    .. code-block:: dump

        load from R0
        return 0
    """
    codeblock_tester.check()


def test_decompose_store_slice_outside(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    A slice outside of a reference ignores stores.

    .. code-block:: instr

        func test(u8& R0, int V)
            def u30& S0 = R0[12:42]
            S0 := V

    TODO: The mask applied to V preserves 30 bits, but all of them are outside of R0.

    .. code-block:: dump

        load from V
        load from R0
        store (((load(V) << 12) & $3FFFFFFF000) | load(R0)) in R0
    """
    codeblock_tester.check()


def test_decompose_load_concat_slice(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    Loading a slice of a concatenation is decomposed into slices of the components.

    .. code-block:: instr

        func test(u8& R0, u8& R1, u8& R2)
            def u24& C = R2;R1;R0
            def u13& S = C[5:18]
            def int ret = S

    .. code-block:: dump

        load from R0
        load from R1
        load from R2
        return ((load(R2) << 11) | (load(R1) << 3) | load(R0)[5:])[:13]
    """
    codeblock_tester.check()


def test_decompose_store_concat_slice(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    Storing a slice of a concatenation is decomposed into slices of the components.

    .. code-block:: instr

        func test(u8& R0, u8& R1, u8& R2, int V)
            def u24& C = R2;R1;R0
            def u13& S = C[5:18]
            S := V

    .. code-block:: dump

        load from V
        load from R0
        load from R1
        load from R2
        store ((load(V) << 5) | load(R0)[:5])[:8] in R0
        store load(V)[3:11] in R1
        store (load(V)[11:13] | (load(R2) & $FC)) in R2
    """
    codeblock_tester.check()


def test_decompose_load_nested_slice(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    Reference slices can be sliced again.

    .. code-block:: instr

        func test(u8& R0, u8& R1)
            def u5& S0 = R0[2:7]
            def u4& S1 = R1[1:5]
            def u9& C = S1;S0
            def int ret = C[3:6]

    .. code-block:: dump

        load from R0
        load from R1
        return (load(R0)[5:7] | ((load(R1) << 1) & 4))
    """
    codeblock_tester.check()


def test_decompose_store_nested_slice(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    Reference slices can be sliced again.

    .. code-block:: instr

        func test(u8& R0, u8& R1, int V)
            def u5& S0 = R0[2:7]
            def u4& S1 = R1[1:5]
            def u9& C = S1;S0
            C[3:6] := V

    TODO: I didn't expect two loads from each.

    .. code-block:: dump

        load from V
        load from R0
        load from R1
        load from R0
        store ((((load(V) << 5) | (load(R0) & $1C)) & $7C) | (load(R0) & $83)) in R0
        load from R1
        store ((load(V)[1:] & 2) | (load(R1) & $E1) | (load(R1) & 4)) in R1
    """
    codeblock_tester.check()
