from __future__ import annotations

from .conftest import CodeBlockDocstringTester


def test_no_change(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    A basic register assignment cannot be simplified.

    .. code-block:: instr

        func test()
            b := a

    .. code-block:: dump

        load from reg32 a
        store load(reg32 a) in reg32 b
    """
    codeblock_tester.check()


def test_stored_expression(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    Stored expressions are simplified.

    .. code-block:: instr

        func test()
            a := 1 + 1

    .. code-block:: dump

        store 2 in reg32 a
    """
    codeblock_tester.check()


def test_unused_load_removal(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    Unused loads without side effects are removed.

    .. code-block:: instr

        func test()
            a := a & 0

    .. code-block:: dump

        store 0 in reg32 a
    """
    codeblock_tester.check()


def test_unused_load_nonremoval(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    Unused loads with potential side effects are kept.

    .. code-block:: instr

        func test()
            a := mem[$D0D0] & 0

    .. code-block:: dump

        load from mem[$D0D0]
        store 0 in reg32 a
    """
    codeblock_tester.check()


def test_redundant_load_after_load(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    Redundant successive loads are removed.

    .. code-block:: instr

        func test()
            b := a
            mem[$D0D0] := a

    .. code-block:: dump

        load from reg32 a
        store load(reg32 a) in reg32 b
        store load(reg32 a) in mem[$D0D0]
    """
    codeblock_tester.check()


def test_redundant_load_after_store(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    A redundant load after a store is removed.

    .. code-block:: instr

        func test()
            a := a + 1
            b := a

    .. code-block:: dump

        load from reg32 a
        store (load(reg32 a) + 1)[:32] in reg32 a
        store (load(reg32 a) + 1)[:32] in reg32 b
    """
    codeblock_tester.check()


def test_redundant_same_value_store(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    Redundant stores of an already stored value are removed.

    .. code-block:: instr

        func test()
            b := a
            b := a

    .. code-block:: dump

        load from reg32 a
        store load(reg32 a) in reg32 b
    """
    codeblock_tester.check()


def test_intermediate_store_removal(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    Only the last store to a side-effect free storage is kept.

    .. code-block:: instr

        func test()
            a := 1
            a := 2

    .. code-block:: dump

        store 2 in reg32 a
    """
    codeblock_tester.check()


def test_intermediate_store_nonremoval(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    Every store to a storage with side effects is kept.

    .. code-block:: instr

        func test()
            mem[$D0D0] := 1
            mem[$D0D0] := 2

    .. code-block:: dump

        store 1 in mem[$D0D0]
        store 2 in mem[$D0D0]
    """
    codeblock_tester.check()


def test_uncertain_redundant_load(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    Aliasing prevents loads from being removed.

    'X' might contain a reference to the 'a' register.

    .. code-block:: instr

        func test(u32& X)
            X := a ^ 1
            b := a

    .. code-block:: dump

        load from reg32 a
        store (load(reg32 a) ^ 1) in X
        load from reg32 a
        store load(reg32 a) in reg32 b
    """
    codeblock_tester.check()


def test_alias_swizzled(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    A reference can contain a swizzled version of a register.

    There used to be a bug where the simplifier assumed that writing a register's current
    value to a reference didn't invalidate previously loaded values. However, that is not
    correct when the reference swizzles the bits.

    .. code-block:: instr

        func inner(u32& X)
            X := a  # this changes 'a'
            b := a

        func test()
            a := $12345678
            inner(a[:16];a[16:])

    .. code-block:: dump

        store $56781234 in reg32 a
        store $56781234 in reg32 b
    """
    codeblock_tester.check()


def test_local_variable(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    Loads and stores on local variables are removed.

    .. code-block:: instr

        func test()
            var u32 V := a
            b := V

    .. code-block:: dump

        load from reg32 a
        store load(reg32 a) in reg32 b
    """
    codeblock_tester.check()


def test_unused_storage_removal(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    Unused storages are removed.

    .. code-block:: instr

        func ignore(u32& X)
            nop

        func test()
            ignore(mem[a])

    .. code-block:: dump

    """
    codeblock_tester.check()


def test_return_value(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    A load used in the return value is kept.

    .. code-block:: instr

        func test()
            def u32 ret = a

    .. code-block:: dump

        load from reg32 a
        return load(reg32 a)
    """
    codeblock_tester.check()


def test_return_io_index(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    The index of a returned I/O reference is simplified.

    .. code-block:: instr

        func test()
            def u32& ret = mem[a ^ a]

    .. code-block:: dump

        return mem[0]
    """
    codeblock_tester.check()


def test_return_fixed_value_ref(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    It is possible to return a reference to a fixed value.

    .. code-block:: instr

        func test()
            def u8& ret = $ED

    .. code-block:: dump

        return $ED
    """
    codeblock_tester.check()


def test_ret_truncate(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    The returned value is truncated if it doesn't fit in the return type.

    .. code-block:: instr

        func test()
            def u8 ret = $8472

    .. code-block:: dump

        return $72
    """
    codeblock_tester.check()


def test_return_complex_ref(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    A returned reference can be non-trivial.

    Note that we only simplify expressions, not references, so the returned reference remains
    unchanged. All we really check here is that code block creation doesn't break, but that
    is worthwhile in itself.

    .. code-block:: instr

        func test()
            def u8& ret = (h;l)[:8]

    .. code-block:: dump

        return (reg8 h ; reg8 l)[:8]
    """
    codeblock_tester.check()


def test_repeated_increase_reg(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    Redundant loads and stores on a register are removed.

    .. code-block:: instr

        func test()
            a := a + 1
            a := a + 1
            a := a + 1

    .. code-block:: dump

        load from reg32 a
        store (load(reg32 a) + 3)[:32] in reg32 a
    """
    codeblock_tester.check()


def test_repeated_increase_var(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    All loads and stores on local variables are removed.

    .. code-block:: instr

        func test(u32& X)
            var u32 V := X
            V := V + 1
            V := V + 1
            V := V + 1
            def u32 ret = V

    .. code-block:: dump

        load from X
        return (load(X) + 3)[:32]
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


def test_6502_pull(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    In the 6502 pull instructions, the load of 's' when evaluating the stack pointer
    can be removed, as the value is still known from the increment.

    .. code-block:: instr

        reg
        u8 s
        u16& sp_6502 = $01;s

        io
        u8 mem_6502[u16]

        func test(u8& D)
            s := s + 1
            D := mem_6502[sp_6502]

    .. code-block:: dump

        load from reg8 s
        store (load(reg8 s) + 1)[:8] in reg8 s
        load from mem_6502[((load(reg8 s) + 1) | $100)]
        store load(mem_6502[((load(reg8 s) + 1) | $100)]) in D
    """
    codeblock_tester.check()
