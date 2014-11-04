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

Empty lines terminate blocks. Empty lines outside blocks are ignored. Trailing whitespace is stripped; this implies that lines containing only whitespace are considered empty lines.

The `#` character marks the remainder of the line as a comment, which is ignored. Lines containing only a comment are not considered empty and therefore do not terminate a block. A literal `#` character can be produced by preceding it with a backslash: `\#`.

Types
-----

The type `i`*N* is an integer type of *N* bits wide. So for example `i8` is a byte and `i1` can hold a Boolean value. The type `i0` is valid and the only value of that type is the number 0. Whether an integer value is signed or unsigned is determined by the operation that uses the value; it is not part of the value's type.

Currently no other types besides integers are supported.

Literals
--------

Integer literals in base 2, 10 and 16 are supported. The width of a binary integer literal is equal to the number of digits it contains; the width of a hexadecimal integer literal is equal to four times the number of digits it contains. In both cases leading zeroes count as digits. The width of a decimal integer literal is the smallest number of bits that can contain the literal's value. Leading zeroes are not allowed on decimal integer literals, to avoid confusion with the C notation for octal numbers.

Name        |Base | Notation | Value | Type
----------- |----:| --------:| -----:|:----:
binary      | 2   | %0101    | 5     | i4
decimal     |10   | 42       | 42    | i6
hexadecimal |16   | $7F      | 127   | i8

There are no negative integer literals: for example `-4` is considered unary negation applied to the number 4.

Registers
---------

A register definition consists of a name and a type. A register definition block can define multiple registers and register aliases:

    = reg
    <type> <name>*
    <type> <alias> = <expr>

For example this block defines all registers of the 6502:

    = reg
    i8  a x y
    i1  n v b d i z c
    i8  p = n ; v ; %1 ; b ; d ; i ; z ; c
    i8  s
    i16 pc

The type declaration on aliases is redundant, but mandatory for consistency and as an extra validation.

An integer literal in an alias expression means the corresponding bits are always read as that literal value, while writes to those bits are ignored.

If a register can be accessed in multiple ways, for example as an individual register or as part of a register pair, it is recommended to define the smallest unit as a register and define the larger units as aliases. For flags this means defining them individually as registers of type `i1`.

The program counter register must always be named `pc`. If the instruction set uses a different name, that other name can be defined as an alias for the `pc` register.
