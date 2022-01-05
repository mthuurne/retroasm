Instruction Set Definition
==========================

Instruction sets are defined in text files with the ``.instr`` file name extension.

Structure
---------

A block starts with a header line followed by a number of lines of block contents. The header line starts with keyword indicating the block type (\ ``instr``\ , ``func``\ , ``reg`` etc.) followed by zero or more arguments, depending on the block type.

Empty lines terminate blocks. Empty lines outside blocks are ignored. Trailing whitespace is stripped; this implies that lines containing only whitespace are considered empty lines.

The ``#`` character marks the remainder of the line as a comment, which is ignored. Lines containing only a comment are not considered empty and therefore do not terminate a block. A literal ``#`` character can be produced by preceding it with a backslash: ``\#``.

Types
-----

The language contains value types and reference types. Currently the only supported value types are integer types.

Integer Types
^^^^^^^^^^^^^

The type :math:`u_N` is an unsigned integer type of :math:`N` bits wide. So for example :math:`u_8` is a byte and :math:`u_1` can hold a Boolean value. The type :math:`s_N` is a signed integer type of :math:`N` bits wide, where bit :math:`N-1` is the sign bit. The types :math:`u_0` and :math:`s_0` are valid and the only value in those types is the number 0. The type :math:`int` is an integer type that can contain arbitrary signed integers (unlimited width).

The :math:`int` type is used in arithmetical expressions. The :math:`u_N` types are used to describe aspects of the hardware, such as registers. The :math:`s_N` types are not often used, but are necessary to for example describe signed offsets in relative addressing modes.

In plain text, such as in instruction set definition files, types are written as follows: ``int``, ``u16`` and ``s8``.

Reference Types
^^^^^^^^^^^^^^^

A reference to a storage location is denoted by placing an ampersand after the value type. For example :math:`u_8^\&`, or ``u8&`` in plain text, is a reference to a byte.

Literals
--------

Integer literals in base 2, 10 and 16 are supported. Integer literals are never negative: for example ``-4`` is considered the unary complement operator ``-`` applied to the literal ``4``.

.. list-table::
   :header-rows: 1

   * - Name
     - Base
     - Notation
     - Value
     - Type
   * - binary
     - 2
     - ``%0101``
     - 5
     - :math:`u_4`
   * - decimal
     - 10
     - ``42``
     - 42
     - :math:`int`
   * - hexadecimal
     - 16
     - ``$7F``
     - 127
     - :math:`u_8`


The width of a binary integer literal is equal to the number of digits it contains; the width of a hexadecimal integer literal is equal to four times the number of digits it contains. In both cases leading zeroes count as digits.

The width of a decimal integer literal is undefined: they are of the type :math:`int`. Leading zeroes are not allowed on decimal integer literals, to avoid confusion with the C notation for octal numbers.

Operators
---------

The following operators can be used in expressions:

.. list-table::
   :header-rows: 1
   :widths: auto

   * - Name
     - Notation
     - Types
   * - bitwise and
     - ``A & B``
     - :math:`int \times int \rightarrow int`
   * - bitwise or
     - ``A | B``
     - :math:`int \times int \rightarrow int`
   * - bitwise xor
     - ``A ^ B``
     - :math:`int \times int \rightarrow int`
   * - shift left
     - ``A << B``
     - :math:`int \times int \rightarrow int`
   * - shift right
     - ``A >> B``
     - :math:`int \times int \rightarrow int`
   * - addition
     - ``A + B``
     - :math:`int \times int \rightarrow int`
   * - subtraction
     - ``A - B``
     - :math:`int \times int \rightarrow int`
   * - type conversion
     - ``to_s(A)``
     - :math:`u_N \rightarrow s_N`
   * -
     - ``to_u(A)``
     - :math:`s_N \rightarrow u_N`
   * - complement
     - ``-A``
     - :math:`int \rightarrow int`
   * - bitwise complement
     - ``~A``
     - :math:`int \rightarrow int`
   * - logical negation
     - ``!A``
     - :math:`int \rightarrow u_1`
   * - equality
     - ``A == B``
     - :math:`int \times int \rightarrow u_1`
   * - inequality
     - ``A != B``
     - :math:`int \times int \rightarrow u_1`
   * - lesser than
     - ``A < B``
     - :math:`int \times int \rightarrow u_1`
   * - lesser or equal
     - ``A <= B``
     - :math:`int \times int \rightarrow u_1`
   * - greater than
     - ``A > B``
     - :math:`int \times int \rightarrow u_1`
   * - greater or equal
     - ``A >= B``
     - :math:`int \times int \rightarrow u_1`
   * - concatenation
     - ``A ; B``
     - | :math:`int^\& \times u_{N}^\& | s_{N}^\& \rightarrow int^\&`
       | :math:`u_{M}^\& \times u_{N}^\& | s_{N}^\& \rightarrow u_{M+N}^\&`
       | :math:`s_{M}^\& \times u_{N}^\& | s_{N}^\& \rightarrow s_{M+N}^\&`
   * - slicing
     - ``A[K:L]``
     - :math:`int^\& \rightarrow u_{L-K}^\&`
   * -
     - ``A[:L]``
     - :math:`int^\& \rightarrow u_{L}^\&`
   * -
     - ``A[K:]``
     - | :math:`int^\& \rightarrow int^\&`
       | :math:`u_{N}^\& \rightarrow u_{N-K}^\&`
       | :math:`s_{N}^\& \rightarrow s_{N-K}^\&`
   * - bitwise lookup
     - ``A[K]``
     - :math:`int^\& \rightarrow u_1^\&`
   * - I/O reference
     - ``C[X]``
     - :math:`u_M \rightarrow u_N^\&`


Most of these operators should be familiar to the reader, but a few may require a more detailed explanation.

The logical negation operator works as in the C language: the negation of zero is 1, the negation of any non-zero number is 0.

Concatenation places one fixed width bit string after another. For example, the concatenation of ``%11`` and ``%001`` is ``%11001``. In numeric value: ``A ; B`` = :math:`A \cdot 2^N + B` , where :math:`B` is of type :math:`u_N`. The signedness of the result of a concatenation matches the signedness of the first operand.

Slicing extracts a region from a bit string: ``A[K:L]`` extracts the bits from and including bit :math:`K` up to and excluding bit :math:`L`, similar to sequence slicing in Python. For example: ``$12CD[4:8]`` = ``$C``. If the lower index of a slice is omitted, the slice starts from bit 0: ``$AB[:4]`` = ``$B``. If the upper index of a slice is omitted, the slice ends at the full width of the sliced expression: ``$AB[4:]`` = ``$A``.

The value of a slice can be expressed using other operators as follows: ``A[K:L]`` = :math:`(A \gg K)\ \&\ (2^{L-K}-1)`. If the lower index is omitted, no shift is performed (shift by 0 bits). If the upper index is omitted, the *and* operation is dropped.

The width of a slice must be known at the time of definition. This means it cannot depend on for example a function argument or a value from a register or immediate. Note that RetroAsm is able to determine that the width of ``A[K:K+N]`` is :math:`N` bits if it knows the value of :math:`N` but not the value of :math:`K`.

A bitwise lookup is equivalent to taking a single bit slice: ``A[K]`` = ``A[K:K+1]``.

An I/O reference is used to read or write data through an I/O channel. The type of the index and the type of the returned value depend on the I/O channel definition, see the Input/Output section for details.

Type Conversions
----------------

Conversion from fixed-width :math:`u_N` or :math:`s_N` integer type to arbitrary-sized integer type :math:`int` is performed automatically when necessary. These conversions can safely be done implicitly since the correct value is always preserved.

Conversion from arbitrary-sized integer type :math:`int` to fixed-width integer type :math:`u_N` is done by truncation: the :math:`N` least significant bits of the value are kept. Truncation can be done explicitly through slicing: ``A[:N]`` will convert :math:`A` to :math:`u_N`. Truncation is done implicitly when an integer value is stored into a fixed-width reference.

Conversion from unsigned to signed or vice versa is done by keeping the bit string identical, which means the value will change if the most significant bit is set. For example ``$84`` is a :math:`u_8` with numeric value 132, but when converted to :math:`s_8` the value becomes -124. Explicit conversion can be performed using the ``to_s`` and ``to_u`` operators. Implicit conversion happens after implicit truncation: the value will be converted to match the signedness of the reference type.

When a reference is used where a value is expected, the value is loaded from the reference. If the reference points to a peripheral, the load operation can have side effects in the peripheral. For example, some hardware status flags are automatically reset when read.

When a value is used where a reference is expected, a reference to a fixed value will be created. When a fixed value reference is loaded from, the result is the value itself. When a fixed value reference is stored to, nothing happens.

Registers
---------

A register definition block can define base registers and register aliases.

A base register is a piece of state in the modeled processor: it can be a general-purpose register that can be used directly in instruction operands or a special-purpose register than must be implicitly accessed through dedicated instructions.

Base registers are defined using the following syntax:

.. code-block::

   reg
   <type> <name>, [<type>] <name> [, [<type>] <name> [, ...]]


The type can be omitted for the every register but the first on the line. If the type is omitted, a register will have the same type as the previously defined register.

A register alias is not a piece of state itself, but a way to access combinations of base registers and constant values. It can be used to for example create a 16-bit register pair from two 8-bit registers, or an 8-bit flag register from several 1-bit flags.

Register aliases are defined by putting ``= <expr>`` after the register name. It is possible to define base registers and aliases on the same line, which makes the full syntax for register definitions as follows:

.. code-block::

   reg
   <type> <name> [= <expr>] [, [<type>] <name> [= <expr>] [, ...]]


The definitions can be split over multiple lines to improve readability. As an example, this block defines all registers of the 6502:

.. code-block::

   reg
   u8 a, x, y
   u1 n, v, b, d, i, z, c
   u8& p = n;v;%1;b;d;i;z;c
   u8 s
   u16 pc


The integer literal ``%1`` in the value of the ``p`` alias above is an example of a fixed value reference: the corresponding bits are always read as that literal value, while writes to those bits are ignored.

If a register can be accessed in multiple ways, for example as an individual register or as part of a register pair, it is recommended to define the smallest unit as a register and define the larger units as aliases. For flags this means defining them individually as registers of type :math:`u_1`.

The program counter register must always be named ``pc``. If the instruction set uses a different name, that other name can be defined as an alias for the ``pc`` register.

Input/Output
------------

Input/output (I/O) is when a CPU reads or writes data from/to memory or peripherals. Some instruction sets perform all I/O through memory addresses (memory-mapped I/O) while other instruction sets also have dedicated I/O ports for accessing peripherals (port-mapped I/O).

The syntax for defining I/O channels is as follows:

.. code-block::

   io
   <element type> <channel name>[<address type>]


For example the Z80 has a 64K (:math:`2^{16}`) memory address space and 256 (:math:`2^8`) I/O ports that are one byte wide:

.. code-block::

   io
   u8 mem[u16]
   u8 port[u8]


When reading a value from an I/O channel, the element type determines the type of the read value. When writing a value to an I/O channel, the written value can be of any type, but the width of the element type determines how many bits are actually stored.

The width of the address type determines the number of significant bits in the index. For example, if the address type is :math:`u_8`, addresses ``$21`` and ``$321`` are considered to address the same element, since they are equal in the last 8 bits.

For a CPU, it doesn't matter what is on the other side of an I/O channel. But for analyzing assembly code it does matter whether I/O is done with RAM, ROM or a peripheral. Therefore an analyzer will need a system definition in addition to an instruction set definition to do its job.

Currently the analyzer assumes that an index of one I/O channel can never alias an index of another I/O channel. In other words, the storages behind two I/O channels are assumed to be disjunct. That might not be true for all hardware however, for example a single register bank might be accessible through both port-mapped I/O and memory-mapped I/O.

Statements
----------

Statements are used to define the operation of the processor.

Each line of a statement block contains a single statement. As usual, an empty line ends a block. It is possible to indent a statement block for better readability, but this optional and has no syntactical meaning.

Assignment
^^^^^^^^^^

The most common statement is assignment, which uses the ``<lhs> := <rhs>`` syntax. An assignment will compute the value of the expression on its right-hand side and store it into the reference on its left-hand side, for example:

.. code-block::

   a := a + 1


Multiple storage locations can be stored into in a single assignment using a concatenated reference. It is also possible to assign to a slice of a reference, which will load its value, combine it with the assigned value and store the result:

.. code-block::

   a[0:4] ; mem[hl] := mem[hl] ; a[0:4]


When storing to a sliced reference, all its subreferences will be loaded from and then stored into, even if they are outside the range of the slice. For example, in the following statement both memory locations are read and then written, regardless of which bit *B* is actually set:

.. code-block::

   (mem[A+1] ; mem[A])[B] := 1


Variables
^^^^^^^^^

Variables can be declared using the syntax ``var <value type> <name>``. Optionally, the variable can be assigned a value in the same statement:

.. code-block::

   var u8 X
   var u1 C := 1


Variables are storage locations that don't represent registers or other hardware storage.

Constants
^^^^^^^^^

Constants can be defined using the syntax ``def <value type> <name> = <expr>``\ :

.. code-block::

   def u8 V = a


As the name implies, constants are immutable. While it is allowed to attempt to write a value to a constant, doing so will not change the constant's value. The reason for allowing writes to constants is that it can be useful to have a concatenated reference that is part writable and part read-only.

A constant's value is evaluated when that constant is defined, so in the example above ``V`` represents the value of the ``a`` register at the time that control reaches the ``def`` statement.

References
^^^^^^^^^^

References to storage locations can be defined using the syntax ``def <reference type> <name> = <expr>``\ :

.. code-block::

   def u8& R = a


The referenced storage location is loaded from or stored to when the reference is used in expressions or assignments, not at the time of the ``def`` statement. However, expressions used as indices to select a storage location in an I/O channel are evaluated as part of the ``def`` statement:

.. code-block::

   def u8& R = mem[hl]


This will create a reference to the memory location at the address specified by the value of ``hl`` at the time of the ``def`` statement. That fixed memory location will be read or written when ``R`` is loaded from or stored to, even when ``hl`` is modified later.

Literals and constants can be part of a reference's definition: when read they produce their value, when written they remain unchanged. For example the stack pointer of the 6502 can be defined as follows:

.. code-block::

   def u16& sp = $01;s


When read, the upper byte of the stack pointer's value will be ``$01``. When written, the upper byte of the written value is ignored.

Flow Control
^^^^^^^^^^^^

Labels are names that identify locations within a function that can be jumped to. Labels are local to the function they are defined in. A label is defined by putting its name, prefixed with the ``@`` symbol, on a line by itself:

.. code-block::

   @skip


It is possible to jump to a label using the ``branch`` statement. An unconditional branch uses the syntax ``branch @<label>`` and a conditional branch uses the syntax ``branch <expr> @<label>``\ :

.. code-block::

   branch !cf @nocarry
   branch @carry


A conditional branch is taken only if the expression's value is not zero; if the value is zero execution continues at the next statement as usual.

Flow control of the instruction set definition is unrelated to flow control of the processor being defined. The latter is modeled by assigning to the ``pc`` register.

No Operation (NOP)
^^^^^^^^^^^^^^^^^^

The ``nop`` statement does absolutely nothing. It can be used in situations where you have to provide a statement but there is nothing to be done.

Functions
---------

Functions can be defined to avoid duplication in instruction set definitions:

.. code-block::

   func <return type> <name>(<arguments>)
   <statements>


Arguments are specified as a type followed by a name and separates by commas. Value arguments are specified using just the type name, while reference arguments use the usual reference syntax of the type name followed by an ampersand. For example the following function header declares a value argument named ``A`` and a reference argument named ``V``\ :

.. code-block::

   func u1 foo(u16 A, u8& V)


Inside a function, value arguments are treated as local variables, meaning they can be modified.

If the return type is empty, the function does not return anything:

.. code-block::

   func push(u8 V)
       mem[$01 ; s] := V
       s := s - 1


If the return type is a value type, the function returns a value by assigning it to a variable named ``ret``\ :

.. code-block::

   func u16 read16(u16 A)
       var u8 L := mem[A]
       var u8 H := mem[A + 1]
       ret := H ; L


If the return type is a reference type, the function returns a reference by defining a reference named ``ret``\ :

.. code-block::

   func u8& indx(u8 A)
       def u8 L = mem[(A + x    )[:8]]
       def u8 H = mem[(A + x + 1)[:8]]
       ret = mem[H ; L]


If a local variable is part of a returned reference, it will be treated as a constant containing the value of that variable at the exit of the function body. It is not possible to modify a local variable after the function has finished executing.

Instruction Decoding
--------------------

Prefixes
^^^^^^^^

Processors like the Z80 and the x86 family have prefixes that modify how instructions work. The syntax described here has only been validated with the Z80, so it might have to be redesigned when support for other instruction sets with prefixes is added later.

A prefix definition uses the following syntax:

.. code-block::

   prefix <decode flags>
   <encoding> . <mnemonic> . <semantics>


The decode flags are defined using a comma-separated list of declarations consisting of a type followed by a name, similar to function arguments. As the term 'flags' suggests, these will typically be Booleans, so of the type ``u1``. A decode flag is not allowed to have the same name as a register, I/O channel or function.

The body of the prefix definition is a series of dot-separated lines, one per prefix. These are similar to the way mode definitions work, as will be explained in the next section.

The definition of the Z80's prefixes looks like this:

.. code-block::

   prefix u1 ixf, u1 iyf
   $DD     .           . ixf;iyf := %10
   $FD     .           . ixf;iyf := %01


Here two flags are defined: ``ixf`` and ``iyf``\ , which select the ``IX`` and ``IY`` register overrides respectively. The prefix byte ``$DD`` before an instruction will set the ``ixf`` flag and the prefix byte ``$FD`` will set the ``iyf`` flag. These two flags are mutually exclusive and only the flag set by the last encountered prefix counts. The mnemonic field is empty, since the prefix is implied by the registers used in the operands: there are no keywords to select prefixes in Z80 assembly.

Decode flags are considered to be zero at the start of the decoding of each instruction. If you want to model a persistent flag, you should instead define a register and one or more instructions to change that register.

Opcode bytes that select different instructions rather than modify existing instructions should not be modeled as prefixes, but as part of the instruction itself. For example, the Z80 ``$CB`` and ``$ED`` opcode bytes are considered part of the instruction encoding, so ``LDIR`` has the encoding ``$ED $B0``\ , not ``$B0`` with a ``$ED`` prefix.

Branch Delay Slots
^^^^^^^^^^^^^^^^^^

There is no way yet to specify that an instruction set uses branch delay slots, but this will likely be added in the future.

Modes
-----

Modes define patterns for specifying the operands of instructions. This includes addressing modes for accessing memory, but also register use.

A mode definition uses the syntax below:

.. code-block::

   mode <type> <name>
   <encoding> . <mnemonic> . <semantics> . <context>


The type in the header is the type for expressions the semantics field. For modes defining register sets and addressing modes this will be a reference type, such as ``u8&`` for 8-bit registers and I/O, while for modes defining immediates or conditions this will be a value type, such as ``u16`` for 16-bit immediates and ``u1`` (Boolean) for conditions.

There can be as many dot-separated lines as necessary to define all entries of a mode, creating a 4-column table.

Encoding, Mnemonic, Semantics
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The encoding field contains the literals used to encode the operand in instructions. This is typically not a full instruction, but only the bits that for example select the register to operate on.

The mnemonic field contains the syntax used in assembly language. It is split into words and symbols. Whitespace can be used to separate words and is otherwise ignored. Words consist of one or more letters, numbers and underscores. All characters that are not whitespace and not allowed in words are considered symbols, which each such character being an individual symbol. For example ``ld (hl),R`` is split into the word ``"ld"``\ , the symbol ``'('``\ , the word ``"hl"``\ , the symbol ``')'``\ , the symbol ``','`` and the word ``"R"``.

The semantics field contains an expression, either a value or a reference to a storage location, that describes the operand in a way RetroAsm can analyze. The expression field can be omitted, in which case the mnemonic field is parsed as the expression; this is useful for registers where the mnemonic is usually just the register name. If the semantics cannot be expressed in a single expression, a function call can be used to include a longer definition.

The optional context field will be explained soon, but first an example using only the first three fields. In this example, a mode is defined that describes the way the Z80 accesses 8-bit operands: (index registers omitted for simplicity's sake)

.. code-block::

   mode u8& reg8
   %000    . b
   %001    . c
   %010    . d
   %011    . e
   %100    . h
   %101    . l
   %110    . (hl)      . mem[hl]
   %111    . a


Context: Placeholders
^^^^^^^^^^^^^^^^^^^^^

The simplest use of the context field is to define immediate values, using the syntax ``<type> <name>``. For example, the definitions below describe the immediate and non-indexed zero page addressing modes of the 6502:

.. code-block::

   mode u8 immediate
   N       . \#N       . N         . u8 N

   mode u8& zeropage
   A       . A         . mem[A]    . u8 A


The names ``N`` and ``A`` that are declared in the context are used in the other fields; these uses are called placeholders. A placeholder represents a value that will substituted at a later time. In the examples above, that value will be an 8-bit unsigned integer.

It is possible to include a mode defined earlier as part of a new mode, using the syntax ``<mode> <name>`` in the context field:

.. code-block::

   mode u16& reg16
   %00     . bc
   %01     . de
   %10     . hl
   %11     . sp

   mode u16& reg16af
   R       . R         . R         . reg16 R
   %11     . af


In the first entry of ``reg16af``\ , the context field is used to match according to the ``reg16`` mode and use the match as-is. The second entry then replaces ``sp`` with ``af``.

When multiple entries match the same encoding, the later entry fully replaces the earlier entry. In mode ``reg16af`` in the example above, not only does the encoding ``%11`` map to the mnemonic ``af``\ , but the mnemonic ``sp`` does not occur at all in mode ``reg16af``.

When there is more than one way of encoding the same instruction, the assembler will use the following priorities:

- shorter encodings are always picked over longer ones
- later entries in mode tables are picked over earlier entries
- if there is still a tie, the lowest possible encoding is picked

In the encoding, mnemonic and semantics field, the placeholder represents that same field in the matched entry from the included mode. For example, the ``R`` in the encoding field represents the encoding of the matched entry, while the ``R`` in the mnemonic field represents the mnemonic of the matched entry.

Placeholders can be used in expressions, for example to define the Z80 flag tests:

.. code-block::

   mode u1 cond2
   %00     . nz        . !zf
   %01     . z         .  zf
   %10     . nc        . !cf
   %11     . c         .  cf

   mode u1 cond3
   %0;C    . C         . C         . cond2 C
   %100    . po        . !pf
   %101    . pe        .  pf
   %110    . p         . !sf
   %111    . m         .  sf


In the above example, the placeholder ``C`` represents the match made in the ``cond2`` mode table. Let's say that the third entry in ``cond2`` was the one matched. In the first entry of ``cond3``\ , the ``C`` in the second column reproduces the matched mnemonic ``nc`` as-is, while the ``C`` in the third column reproduces the semantic expression ``!cf`` as-is. In the first column, ``C`` matches the encoding ``%10`` which is concatenated to a fixed bit of 0 to form the 3-bit encoding ``%010``.

The context field can contain multiple items, separated by commas. It is possible to define constants and references, similar to the ``def`` keyword in functions: constants are defined using ``<type> <name> = <expr>`` and references are defined using ``<type>& <name> = <expr>``. A common use case for context constants is to define relative addressing, for example the following line defines a 16-bit address that is encoded relative to the program counter using an 8-bit signed offset:

.. code-block::

   N       . A         . A         . s8 N, u16 A = pc + N


A context item could have a semantical side effect, such as changing a register or performing I/O. Context items are evaluated left to right, before the semantics field. All context items are evaluated, regardless of whether their placeholder is used.

In instruction sets with variable instruction lengths, some mode entries can have more than one encoding item. A placeholder in the encoding field represents the first encoding item of the matched mode entry. Any unclaimed additional encoding elements are included by adding ``@`` as a suffix to the placeholder. The following theoretical example shows various ways of including encoding elements from one mode into another:

.. code-block::

   mode u16 imm16
   N[:8], N[8:]    . N         . N             . u16 N

   mode u16 base
   %00             . x
   %01             . y
   %10, A          . A         . A             . u8 A
   %11, A@         . A         . A             . imm16 A

   mode u8 offset
   %00             . a
   %01             . b
   %10             . c
   %11, N          . N         . N             . u8 N

   mode u8& relative
   B;D, B@, D@     . (B + D)   . mem[B + D]    . base B, offset D


The mode ``imm16`` defines a 16-bit immediate that is encoded in little endian byte order (lower 8 bits first). It is used in the fourth entry of mode ``base``\ , where the placeholder ``A@`` includes both bytes from the encoding field of ``imm16``\ : both were unclaimed, since this entry only has ``A@`` in it, not ``A`` by itself.

The mode ``relative`` defines relative addressing using a 16-bit base addresses defined in the ``base`` mode and an 8-bit offset defined in the ``offset`` mode. In the encoding field, ``B`` matches the first encoding element of ``base``\ , which is a 2-bit pattern that selects which register or immediate to use. Since ``B`` claimed the first encoding element, ``B@`` here includes only elements after the first, which is none for the ``x`` and ``y`` register options, one byte for the 8-bit address (zero page) option and two bytes for the 16-bit address (absolute) option.

Similarly, ``D`` in the encoding of mode ``relative`` matches the 2-bit pattern that selects the offset to use. ``D@`` is empty when the offset is one of the 8-bit registers ``a``\ , ``b`` or ``c``\ , while it contains the 8-bit immediate offset if the fourth entry in the ``offset`` mode is matched.

Context: Decode Flags
^^^^^^^^^^^^^^^^^^^^^

A final use of the context field is to filter on instruction decode flags, using the syntax ``?<name>``. For example, the undocumented IXH, IXL, IYH and IYL registers of the Z80 could be added to the ``reg8`` mode from the earlier example:

.. code-block::

   %100    . h
   %100    . ixh       .               . ?ix
   %100    . iyh       .               . ?iy
   %101    . l
   %101    . ixl       .               . ?ix
   %101    . iyl       .               . ?iy


Here is an example that defines Z80 indexed addressing, using a combination of a decode flag filter and an immediate:

.. code-block::

   %110    . (hl)      . mem[hl]
   %110, N . (ix + N)  . mem[ix + N]   . ?ix, s8 N
   %110, N . (iy + N)  . mem[iy + N]   . ?iy, s8 N


If there are multiple decode flags tested in the context of a single mode entry, that entry will only be considered a match if all of those flags are set.

Instructions
------------

An instruction definition uses the syntax below:

.. code-block::

   instr <mnemonic base>
   <encoding> . <mnemonic> . <semantics> . <context>


There can be as many dot-separated lines as necessary, creating a 4-column table, just like mode definitions. Also like mode definitions, different forms of an instruction can be defined in separate ``instr`` blocks.

Unlike mode definitions, where the semantics field contains an expression, the semantics field for an instruction contains a statement, such as an assignment or a call to a function that changes a register or performs I/O.

The mnemonic base is prepended to the mnemonic field of every entry. For example, the definition below defines ``ld D,S`` and ``ld D,N`` -- two forms of ``ld``\ , the Z80 load instruction:

.. code-block::

   instr ld
   %01;D;S             . D,S       . D := S        . reg8 D, reg8 S
   %00;D;%110, N       . D,N       . D := N        . reg8 D, u8 N


The mnemonic ``ld b,h`` is matched by the first entry (\ ``D`` matching ``b`` and ``S`` matching ``h``\ ), while ``ld b,12`` is matched by the second entry (\ ``D`` matching ``b`` and ``N`` matching ``12``\ ).

The mnemonic base can be empty if you want to define separate instructions in a single instruction block. For example, these are definitions for the 6502 instructions that set and clear flags:

.. code-block::

   instr
   %000;%110;%00       . clc       . c := 0
   %001;%110;%00       . sec       . c := 1
   %010;%110;%00       . cli       . i := 0
   %011;%110;%00       . sei       . i := 1
   %101;%110;%00       . clv       . v := 0
   %110;%110;%00       . cld       . d := 0
   %111;%110;%00       . sed       . d := 1

Adding New Instruction Sets
---------------------------

You can use the ``retro checkdef`` command to verify instruction set definition files:

.. click:: retroasm.cmdline:checkdef
   :prog: retro checkdef
   :nested: full
