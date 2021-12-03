Concepts
========

Instructions are the basic units that a processor can execute. An instruction consist of an operation, such as load or add, and zero or more operands that specify what data to operate on, such as registers or memory locations.

An instruction set definition in RetroAsm describes the following aspects of each instruction:

mnemonic
:   A textual representation of the operation and its operands, as used in assembly language.

encoding
:   A binary representation of the operation and its operands, as used by the processor when executing instructions from memory.

semantics
:   A formal description of the operation that an instruction performs.

Mnemonics and encoding are handled by many traditional tools that deal with machine language: an assembler translates a mnemonic version of a program to an encoded version, while a disassembler translates an encoded version to a mnemonic version. The addition of semantics allows other types of processing of machine language programs, such as static code analysis and the automatic generation of interpreters.

Execution timing is currently not modeled. If this were added in the future, it would probably belong in a separate definition, since execution timing can vary a lot between different processors using the same instruction set. Also, for accurate timing we would have to model a system, not just a processor.

Mnemonic
--------

The mnemonic notation of an instruction consists of an operation and zero or more operands, which are separated by commas. For example ``ADD A,5`` consists of the operation ``ADD`` and two operands: ``A`` and ``5``\ , while ``NOP`` is a dummy operation with zero operands.

It is possible for the same operation to be used to denote different -- but likely related -- instructions. For example the Z80 operation ``ADD`` is used for both 8-bit and 16-bit addition.

An operand is an expression. Simple expressions include registers, integer literals and labels, while more complex expressions include instruction set specific addressing modes and mathematical expressions.

Mnemonics are not always consistent semantically. For example the Z80 instruction ``JP (HL)`` will assign the ``HL`` register pair to the program counter; the ``(HL)`` operand suggests the target address would be read from memory at the address stored in ``HL`` but this is not the case.

An instruction can also have implicit operands, which are values it operates on but which are not listed among the operands in the mnemonic notation. Written condition flags are typically implicit, for example the Z80 instruction ``DEC D`` will decrease the explicit operand: the ``D`` register, but it will also (re)set the zero flag depending on the new value of ``D`` (among other flag writes). Implicit full-size register operands are also possible, for example the Z80 instruction ``NEG`` will negate the value of the ``A`` register.

Implicit operands do not appear in the mnemonic notation of an instruction, but do appear in the semantical description. We mention their existence here to demonstrate that the operand list does not always contain all information about which state is inspected and/or modified by an instruction.

Encoding
--------

The encoding of an instruction consists of the bits that identify the operation to perform (the opcode) and the encodings of the operands. Some instruction sets have a fixed instruction width, for example all MIPS instructions are encoded in 32 bits, while other instruction sets have variable instruction width, for example instructions for the 6502 are encoded in one to three bytes each.

RetroAsm allows instruction encodings to include multiple items, but requires all items to have the same width. So for example an instruction set that has some instructions encoded in one 16-bit code and other instructions in two 16-bit codes is supported, while an instruction set that encodes an instruction as a 16-bit code followed by an 8-bit code is not supported.

Because operand encodings are usually the same for many instructions, they can be defined once and then referenced from the instruction definitions. For example most Z80 instructions with 8-bit operands use the same 3-bit encoding which can encode the 7 main 8-bit registers and the ``(HL)`` operand.

Semantics
---------

The semantical description of an instruction specifies the operation performed by an instruction: what state it changes and what I/O it performs. The semantical description uses a simple imperative language consisting only of assignments and conditional branches.

Only the functional aspects of the operation are considered relevant; it doesn't matter how it is implemented in hardware. For example the Z80 had a 4-bit ALU, but in the instruction set definition we can use arithmetic of any width.
