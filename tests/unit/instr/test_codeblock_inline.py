from __future__ import annotations

from .conftest import CodeBlockDocstringTester


def test_inline_easy(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    Inline a simple assignment.

    .. code-block:: instr

        func inner()
            a := $12345

        func test()
            a := 0
            inner()
            def int ret = a

    .. code-block:: dump

        store $12345 in reg32 a
        return $12345
    """
    codeblock_tester.check()


def test_inline_arg_ret(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    Inline a repeated function call with an argument and return value.

    .. code-block:: instr

        func inc(int V)
            def int ret = V + 1

        func test()
            def int ret = inc(inc(inc($100)))

    .. code-block:: dump

        return $103
    """
    codeblock_tester.check()


def test_pass_by_reference(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    A function can update a register passed by reference.

    .. code-block:: instr

        func inc(u32& R)
            R := R + 1

        func test()
            a := $100
            inc(a)
            inc(a)
            inc(a)

    .. code-block:: dump

        store $103 in reg32 a
    """
    codeblock_tester.check()


def test_pass_concat_by_reference(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    Concatenated storages can be passed by reference.

    Note that 'hl' is a concatenation of the 'h' and 'l' registers.

    .. code-block:: instr

        func inner(u16& R)
            R := R + $1234

        func test()
            hl := $ABCD
            inner(hl)
            inner(hl)
            inner(hl)

    .. code-block:: dump

        store $69 in reg8 l
        store $E2 in reg8 h
    """
    codeblock_tester.check()


def test_pass_concat_fixed_by_reference(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    Fixed values can be passed by reference, but they are immutable.

    Note that $CD + $34 = $101, so the high byte will receive a carry on every call.

    .. code-block:: instr

        func inner(u16& R)
            R := R + $1234

        func test()
            h := $AB
            def u16& X = h;$CD
            inner(X)
            inner(X)
            inner(X)

    .. code-block:: dump

        store $E4 in reg8 h
    """
    codeblock_tester.check()


def test_pass_slice_by_reference(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    Slices can be passed by reference.

    .. code-block:: instr

        func inner(u8& R)
            R := R + $12

        func test()
            a := $CDEF
            def u8& X = a[4:12]
            inner(X)
            inner(X)
            inner(X)

    .. code-block:: dump

        store $C14F in reg32 a
    """
    codeblock_tester.check()


def test_reference_arg_signedness(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    Signed references can be passed to unsigned arguments and vice versa.

    The argument type determines whether sign extension is applied upon load.
    While the signess of the passed reference is ignored, its width must match
    the argument width.

    .. code-block:: instr

        func expand_unsigned(u8& R)
            def u32 ret = R

        func expand_signed(s8& R)
            def u32 ret = R

        func test()
            var u8 u := $A4
            var s8 s := $A4
            mem[0] := expand_unsigned(u)
            mem[0] := expand_unsigned(s)
            mem[0] := expand_signed(u)
            mem[0] := expand_signed(s)

    .. code-block:: dump

        store $A4 in mem[0]
        store $A4 in mem[0]
        store $FFFFFFA4 in mem[0]
        store $FFFFFFA4 in mem[0]
    """
    codeblock_tester.check()


def test_return_simple_reference(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    Functions can return a reference to a global.

    .. code-block:: instr

        func inner()
            def u32& ret = a

        func test()
            a := $DC
            inner() := $BA

    .. code-block:: dump

        store $BA in reg32 a
    """
    codeblock_tester.check()


def test_return_io_reference(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    Functions can return a reference to an index in an I/O channel.

    .. code-block:: instr

        func inner(u32 A)
            def u32& ret = mem[A]

        func test()
            a := inner($4002)

    .. code-block:: dump

        load from mem[$4002]
        store load(mem[$4002]) in reg32 a
    """
    codeblock_tester.check()


def test_unique_loads(codeblock_tester: CodeBlockDocstringTester) -> None:
    """
    Multiple instances of the same load with side effects are kept separate.

    If the possibility of side effects is ignored, both loaded values would be considered
    equal and their XOR result would be 0.

    .. code-block:: instr

        func inner()
            def u32 ret = mem[$FFF0]

        func test()
            a := inner() ^ inner()

    .. code-block:: dump

        load from mem[$FFF0]
        load from mem[$FFF0]
        store (load(mem[$FFF0]) ^ load(mem[$FFF0])) in reg32 a
    """
    codeblock_tester.check()
