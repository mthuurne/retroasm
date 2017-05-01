Instruction Set Definition
==========================

An instruction set definition in RetroAsm describes the following aspects of a processor's instruction set:

mnemonics
:	A textual representation of an instruction and its operands, as used in assembly language.

opcodes
:	A binary representation of an instruction and its operands, used by the processor when executing instructions from memory.

semantics
:	A formal description of the operation that an instruction performs. This can be used for analyzing code and generating interpreters.

Execution timing is currently not modeled. If this were added in the future, it would probably belong in a separate definition, since execution timing can vary a lot between different processors using the same instruction set. Also, for accurate timing we would have to model a system, not just a processor.

Mnemonics
---------

The mnemonic notation of an instruction consists of an operation and zero or more operands, which are separated by commas. For example `ADD A,5` consists of the operation `ADD` and two operands: `A` and `5`, while `NOP` is a dummy operation with zero operands.

It is possible for the same operation to be used to denote different -- but likely related -- instructions. For example the Z80 operation `ADD` is used for both 8-bit and 16-bit addition.

An operand is an expression. Simple expressions include registers, integer literals and labels, while more complex expressions include instruction set specific addressing modes and mathematical expressions.

Mnemonics are not always consistent semantically. For example the Z80 instruction `JP (HL)` will assign the `HL` register pair to the program counter; the `(HL)` operand suggests the target address would be read from memory at the address stored in `HL` but this is not the case.

An instruction can also have implicit operands, which are values it operates on but which are not listed among the operands in the mnemonic notation. Written condition flags are typically implicit, for example the Z80 instruction `DEC D` will decrease the explicit operand: the `D` register, but it will also (re)set the zero flag depending on the new value of `D` (among other flag writes). Implicit full-size register operands are also possible, for example the Z80 instruction `NEG` will negate the value of the `A` register.

Implicit operands do not appear in the mnemonic notation of an instruction, but do appear in the semantical description. We mention their existence here to demonstrate that the operand list does not always contain all information about which state is inspected and/or modified by an instruction.

Opcodes
-------

The opcode notation of an instruction consists of a series of bits. For some instruction sets, all instructions will have the same width (number of bits), for others the number of bits per instruction is variable.

Currently RetroAsm represents opcodes as a sequence of one or more bytes. This could be expanded in the future to allow for example 32-bit opcodes to be expressed as a single value instead of 4 separate bytes.

An opcode consists of literal bits and encodings of the operands. The operand encoding is usually the same for many instructions, so it has its own definition which is referenced from the instruction definition. For example most Z80 instructions with 8-bit operands use the same 3-bit encoding which can encode the 7 main 8-bit registers and the `(HL)` operand.

Semantics
---------

The semantical description of an instruction specifies the operation performed by an instruction: what state it changes and what I/O it performs. The semantical description uses a simple imperative language consisting only of assignments and conditional branches.

Only the functional aspects of the operation are considered relevant; it doesn't matter how it is implemented in hardware. For example the Z80 had a 4-bit ALU, but in the instruction set definition we can use arithmetic of any width.

File Format
===========

Instruction sets are defined in text files with the `.instr` file name extension.

Structure
---------

A block starts with a header line followed by a number of lines of block contents. The header line starts with keyword indicating the block type (`instr`, `func`, `reg` etc.) followed by zero or more arguments, depending on the block type.

Empty lines terminate blocks. Empty lines outside blocks are ignored. Trailing whitespace is stripped; this implies that lines containing only whitespace are considered empty lines.

The `#` character marks the remainder of the line as a comment, which is ignored. Lines containing only a comment are not considered empty and therefore do not terminate a block. A literal `#` character can be produced by preceding it with a backslash: `\#`.

Types
-----

The language contains value types and reference types. Currently the only supported value types are integer types.

### Integer Types

The type `u`*N* is an unsigned integer type of *N* bits wide. So for example `u8` is a byte and `u1` can hold a Boolean value. The type `s`*N* is a signed integer type of *N* bits wide, where bit *N*-1 is the sign bit. The types `u0` and `s0` are valid and the only value in those types is the number 0. The type `int` is an integer type that can contain arbitrary signed integers (unlimited width).

The `int` type is used in arithmetical expressions. The `u`*N* types are used to describe aspects of the hardware, such as registers. The `s`*N* types are not often used, but are necessary to for example describe signed offsets in relative addressing modes.

### Reference Types

A reference to a storage location is denoted by placing an ampersand after the value type. For example `u8&` is a reference to a byte.

Literals
--------

Integer literals in base 2, 10 and 16 are supported. Integer literals are never negative: for example `-4` is considered the unary complement operator `-` applied to the literal `4`.

Name        |Base | Notation | Value | Type
:---------- |----:| --------:| -----:|:----:
binary      | 2   | %0101    | 5     | u4
decimal     |10   | 42       | 42    | int
hexadecimal |16   | $7F      | 127   | u8

The width of a binary integer literal is equal to the number of digits it contains; the width of a hexadecimal integer literal is equal to four times the number of digits it contains. In both cases leading zeroes count as digits.

The width of a decimal integer literal is undefined: they are of the type `int`. Leading zeroes are not allowed on decimal integer literals, to avoid confusion with the C notation for octal numbers.

Operators
---------

The following operators can be used in expressions:

Name                |Notation           |Types
:------------------ |:----------------- |:------
bitwise and         | *A* & *B*         | int &times; int &rarr; int
bitwise or          | *A* &#124; *B*    | int &times; int &rarr; int
bitwise xor         | *A* ^ *B*         | int &times; int &rarr; int
shift left          | *A* << *B*        | int &times; int &rarr; int
shift right         | *A* >> *B*        | int &times; int &rarr; int
addition            | *A* + *B*         | int &times; int &rarr; int
subtraction         | *A* - *B*         | int &times; int &rarr; int
type conversion     | to_s(*A*)         | u*N* &rarr; s*N*
                    | to_u(*A*)         | s*N* &rarr; u*N*
complement          | -*A*              | int &rarr; int
bitwise complement  | ~*A*              | int &rarr; int
logical negation    | !*A*              | int &rarr; u1
equality            | *A* == *B*        | int &times; int &rarr; u1
inequality          | *A* != *B*        | int &times; int &rarr; u1
lesser than         | *A* < *B*         | int &times; int &rarr; u1
lesser or equal     | *A* <= *B*        | int &times; int &rarr; u1
greater than        | *A* > *B*         | int &times; int &rarr; u1
greater or equal    | *A* >= *B*        | int &times; int &rarr; u1
concatenation       | *A* ; *B*         | int& &times; (u&#124;s)*N*& &rarr; int&
                    |                   | u*M*& &times; (u&#124;s)*N*& &rarr; u(*M*+*N*)&
                    |                   | s*M*& &times; (u&#124;s)*N*& &rarr; s(*M*+*N*)&
slicing             | *A*[*K*:*L*]      | int& &rarr; u(*L*-*K*)&
                    | *A*[:*L*]         | int& &rarr; u*L*&
                    | *A*[*K*:]         | int& &rarr; int&
                    |                   | u*N*& &rarr; u(*N*-*K*)&
                    |                   | s*N*& &rarr; s(*N*-*K*)&
bitwise lookup      | *A*[*K*]          | int& &rarr; u1&
I/O reference       | *C*[*X*]          | u*M* &rarr; u*N*&

Most of these operators should be familiar to the reader, but a few may require a more detailed explanation.

The logical negation operator works as in the C language: the negation of zero is 1, the negation of any non-zero number is 0.

Concatenation puts one fixed width bit string after another. For example, the concatenation of `%11` and `%001` is `%11001`. In numeric value: *A* ; *B* = *A*\*2<sup>*N*</sup> + *B*, where *B* is of type `u`*N*. The signedness of the result of a concatenation matches the signedness of the first operand.

Slicing extracts a region from a bit string: *A*[*K*:*L*] extracts the bits from and including bit *K* up to and excluding bit *L*, similar to sequence slicing in Python. For example: `$12CD[4:8]` = `$C`. If the lower index of a slice is omitted, the slice starts from bit 0: `$AB[:4]` = `$B`. If the upper index of a slice is omitted, the slice ends at the full width of the sliced expression: `$AB[4:]` = `$A`.

The value of a slice can be expressed using other operators as follows: *A*[*K*:*L*] = (*A* >> *K*) & (2<sup>*L-K*</sup> - 1). If the lower index is omitted, no shift is performed (shift by 0 bits). If the upper index is omitted, the 'and' part is dropped.

The width of a slice must be known at the time of definition. This means it cannot depend on for example a function argument or a value from a register or immediate. Note that RetroAsm is able to determine that the width of *A*[*K*:*K*+*N*] is *N* bits if it knows the value of *N* but not the value of *K*.

A bitwise lookup is equivalent to taking a single bit slice: *A*[*K*] = *A*[*K*:*K*+1].

An I/O reference is used to read or write data through an I/O channel. The type of the index and the type of the returned value depend on the I/O channel definition, see the Input/Output section for details.

Type Conversions
----------------

Conversion from fixed-width (`u`*N* or `s`*N*) integer to arbitrary-sized integer (`int`) is performed automatically when necessary. These conversions can safely be done implicitly since the correct value is always preserved.

Conversion from arbitrary-sized integer (`int`) to fixed-width (`u`*N*) integer is done by truncation: the *N* least significant bits of the value are kept. Truncation can be done explicitly through slicing: *A*[:*N*] will convert *A* to `u`*N*. Truncation is done implicitly when an integer value is stored into a fixed-width reference.

Conversion from unsigned to signed or vice versa is done by keeping the bit string identical, which means the value will change if the most significant bit is set. For example `$84` is a `u8` with numeric value 132, but when converted to `s8` the value becomes -124. Explicit conversion can be performed using the `to_s` and `to_u` operators. Implicit conversion happens after implicit truncation: the value will be converted to match the signedness of the reference type.

When a reference is used where a value is expected, the value is loaded from the reference. If the reference points to a peripheral, the load operation can have side effects in the peripheral. For example, some hardware status flags are automatically reset when read.

When a value is used where a reference is expected, a reference to a fixed value will be created. When a fixed value reference is loaded from, the result is the value itself. When a fixed value reference is stored to, nothing happens.

Registers
---------

A register definition consists of a name and a fixed-width type. A register definition block can define multiple registers and register aliases:

    reg
    <value type> <name>*
    <value type> <alias> = <expr>

For example this block defines all registers of the 6502:

    reg
    u8  a x y
    u1  n v b d i z c
    u8  p = n ; v ; %1 ; b ; d ; i ; z ; c
    u8  s
    u16 pc

The type declaration on aliases is redundant, but mandatory for consistency and as an extra validation.

An integer literal in an alias expression is an example of a fixed value reference: the corresponding bits are always read as that literal value, while writes to those bits are ignored.

If a register can be accessed in multiple ways, for example as an individual register or as part of a register pair, it is recommended to define the smallest unit as a register and define the larger units as aliases. For flags this means defining them individually as registers of type `u1`.

The program counter register must always be named `pc`. If the instruction set uses a different name, that other name can be defined as an alias for the `pc` register.

Input/Output
------------

Input/output (I/O) is when a CPU reads or writes data from/to memory or peripherals. Some instruction sets perform all I/O through memory addresses (memory mapped I/O) while other instruction sets also have dedicated I/O ports for accessing peripherals (I/O mapped I/O).

The syntax for defining I/O channels is as follows:

    io
    <element type> <channel name>[<address type>]

For example the Z80 has a 64K (2<sup>16</sup>) memory address space and 256 (2<sup>8</sup>) I/O ports that are one byte wide:

    io
    u8 mem[u16]
    u8 port[u8]

When reading a value from an I/O channel, the element type determines the type of the read value. When writing a value to an I/O channel, the written value can be of any type, but the width of the element type determines how many bits are actually stored.

The width of the address type determines the number of significant bits in the index. For example, if the address type is `u8`, addresses `0x21` and `0x321` are considered to address the same element, since they are equal in the last 8 bits.

For a CPU, it doesn't matter what is on the other side of an I/O channel. But for analyzing assembly code it does matter whether I/O is done with RAM, ROM or a peripheral. Therefore an analyzer will need a system definition in addition to an instruction set definition to do its job.

Currently the analyzer assumes that an index of one I/O channel can never alias an index of another I/O channel. In other words, the storages behind two I/O channels are assumed to be disjunct. That might not be true for all hardware however, for example a single register bank might be accessible through both I/O mapped I/O and memory mapped I/O.

Statements
----------

Statements are used to define the operation of the processor.

Each line of a statement block contains a single statement. As usual, an empty line ends a block. It is possible to indent a statement block for better readability, but this optional and has no syntactical meaning.

### Assignment

The most common statement is assignment, which uses the `<lhs> := <rhs>` syntax. An assignment will compute the value of the expression on its right-hand side and store it into the reference on its left-hand side, for example:

    a := a + 1

Multiple storage locations can be stored into in a single assignment using a concatenated reference. It is also possible to assign to a slice of a reference, which will load its value, combine it with the assigned value and store the result:

    a[0:4] ; mem[hl] := mem[hl] ; a[0:4]

When storing to a sliced reference, all its subreferences will be loaded from and then stored into, even if they are outside the range of the slice. For example, in the following statement both memory locations are read and then written, regardless of which bit *B* is actually set:

    (mem[A+1] ; mem[A])[B] := 1

### Variables

Variables can be declared using the syntax `var <value type> <name>`. Optionally, the variable can be assigned a value in the same statement:

    var u8 X
    var u1 C := 1

Variables are storage locations that don't represent registers or other hardware storage.

### Constants

Constants can be defined using the syntax `def <value type> <name> = <expr>`:

    def u8 V = a

As the name implies, constants are immutable. The expression value is evaluated when the constant is defined, so in the example above `V` represents the value of the `a` register at the time that control reaches the `def` statement.

### References

References to storage locations can be defined using the syntax `def <reference type> <name> = <expr>`:

    def u8& R = a

The referenced storage location is loaded from or stored to when the reference is used in expressions or assignments, not at the time of the `def` statement. However, expressions used as indices to select a storage location in an I/O channel are evaluated as part of the `def` statement:

    def u8& R = mem[hl]

This will create a reference to the memory location at the address specified by the value of `hl` at the time of the `def` statement. That fixed memory location will be read or written when `R` is loaded from or stored to, even when `hl` is modified later.

### Flow Control

Labels are names that identify locations within a function that can be jumped to. Labels are local to the function they are defined in. A label is defined by putting its name, prefixed with the `@` symbol, on a line by itself:

    @skip

It is possible to jump to a label using the `branch` statement. An unconditional branch uses the syntax `branch @<label>` and a conditional branch uses the syntax `branch <expr> @<label>`:

    branch !cf @nocarry
    branch @carry

A conditional branch is taken only if the expression's value is not zero; if the value is zero execution continues at the next statement as usual.

Flow control of the instruction set definition is unrelated to flow control of the processor being defined. The latter is modeled by assigning to the `pc` register.

### No Operation (NOP)

The `nop` statement does absolutely nothing. It can be used in situations where you have to provide a statement but there is nothing to be done.

Functions
---------

Functions can be defined to avoid duplication in instruction set definitions:

    func <return type> <name>(<arguments>)
    <statements>

Arguments are specified as a type followed by a name and separates by commas. Value arguments are specified using just the type name, while reference arguments use the usual reference syntax of the type name followed by an ampersand. For example the following function header declares a value argument named `A` and a reference argument named `V`:

    func u1 foo(u16 A, u8& V)

Inside a function, value arguments are treated as local variables, meaning they can be modified.

If the return type is empty, the function does not return anything:

    func push(u8 V)
        mem[$01 ; s] := V
        s := s - 1

If the return type is a value type, the function returns a value by assigning it to a variable named `ret`:

    func u16 read16(u16 A)
        var u8 L := mem[A]
        var u8 H := mem[A + 1]
        ret := H ; L

If the return type is a reference type, the function returns a reference by defining a reference named `ret`:

    func u8& indx(u8 A)
        def u8 L = mem[(A + x    )[:8]]
        def u8 H = mem[(A + x + 1)[:8]]
        ret = mem[H ; L]

Modes
-----

Modes define patterns for specifying the operands of instructions. This includes addressing modes for accessing memory, but also register use.

A mode definition uses the syntax below:

    mode <type> <name>
    <opcode> . <mnemonic> . <semantics> . <context>

The type in the header is the type for expressions the semantics field. For modes defining register sets and addressing modes this will be a reference type, such as `u8&` for 8-bit registers and I/O, while for modes defining immediates or conditions this will be a value type, such as `u16` for 16-bit immediates and `u1` (Boolean) for conditions.

There can be as many dot-separated lines as necessary to define all entries of a mode, creating a 4-column table.

The opcode field contains the literals used to encode the operand in instruction opcodes. This is typically not a full instruction opcode, but only the bits that encode for example the register to operate on. If the opcode field is empty, it is interpreted as the zero-width bit string (value of type `u0`).

The mnemonic field contains the syntax used in assembly language.

The semantics field contains a expression, either a value or a reference to a storage location, that describes the operand in a way RetroAsm can analyze. The expression field can be omitted, in which case the mnemonic field is parsed as the expression; this is useful for registers where the mnemonic is usually just the register name. If the semantics cannot be expressed in a single expression, a function call can be used to include a longer definition.

The optional context field will be explained soon, but first an example using only the first three fields. In this example, a mode is defined that describes the way the Z80 accesses 8-bit operands: (index registers omitted for simplicity's sake)

    mode u8& reg8
    %000    . b
    %001    . c
    %010    . d
    %011    . e
    %100    . h
    %101    . l
    %110    . (hl)    . mem[hl]
    %111    . a

The simplest use of the context field is to include a mode defined earlier as part of a new mode:

    mode u16& reg16
    %00     . bc
    %01     . de
    %10     . hl
    %11     . sp

    mode u16& reg16af
    R       . R         . R         . reg16 R
    %11     . af

In the first entry of `reg16af`, the context field is used here to match according to the `reg16` mode and use the match as-is. The second entry then replaces `sp` with `af`.

When an entry's encoding is the same as the encoding of an earlier entry, the later entry overrides the earlier entry. In `reg16af` in the example above, not only does the encoding `%11` map to the mnemonic `af`, but the mnemonic `sp` does not occur at all in mode `reg16af`.

The `R` in the example above is a placeholder introduced by the context field. In each other field, the placeholder represents the same field in the matched entry from the other mode. For example, the `R` in the encoding field represents the encoding of the matched entry, while the `R` in the mnemonic field represents the mnemonic of the matched entry.

Placeholders can be used in expressions, for example to define the Z80 flag tests:

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

In the above example, the placeholder `C` represents the match made in the `cond2` mode table. Let's say that the third entry in `cond2` was the one matched. In the first entry of `cond3`, the `C` in the second column reproduces the matched mnemonic `nc` as-is, while the `C` in the third column reproduces the semantic expression `!cf` as-is. In the first column, `C` matches the encoding `%10` which is concatenated to a fixed bit of 0 to form the 3-bit encoding `%010`.

The context field can also be used to filter on instruction decoding flags, using the syntax `?<name>`. For example, the undocumented IXH, IXL, IYH and IYL registers of the Z80 could be added to the `reg8` mode from the earlier example:

    %100    . h
    %100    . ixh       .          . ?ix
    %100    . iyh       .          . ?iy
    %101    . l
    %101    . ixl       .          . ?ix
    %101    . iyl       .          . ?iy

A mode entry will only be considered a match if all flags listed in the context are set.

Instructions
------------

An instruction definition uses the syntax below:

    instr <mnemonic base>
    <opcode> . <mnemonic> . <semantics> . <context>

There can be as many dot-separated lines as necessary, creating a 4-column table, just like mode definitions. Also like mode definitions, different forms of an instruction can be defined in separate `instr` blocks.

Unlike mode definitions, where the semantics field contains an expression, the semantics field for an instruction contains a statement, such as an assignment or a call to a function that changes a register or performs I/O.

The mnemonic base is prepended to the mnemonic field of every entry. For example, the definition below defines `ld D,S` and `ld D,N` -- two forms of `ld`, the Z80 load instruction:

    instr ld
    %01;D;S             . D,S               . D := S            . reg8 D, reg8 S
    %00;D;%110, N       . D,N               . D := N            . reg8 D, u8 N

The mnemonic `ld b,h` is matched by the first entry (`D` matching `b` and `S` matching `h`), while `ld b,12` is matched by the second entry (`D` matching `b` and `N` matching `12`).

The mnemonic base can be empty if you want to define separate instructions in a single instruction block. For example, these are definitions for the 6502 instructions that set and clear flags:

    instr
    %000;%110;%00       . clc       . c := 0
    %001;%110;%00       . sec       . c := 1
    %010;%110;%00       . cli       . i := 0
    %011;%110;%00       . sei       . i := 1
    %101;%110;%00       . clv       . v := 0
    %110;%110;%00       . cld       . d := 0
    %111;%110;%00       . sed       . d := 1
