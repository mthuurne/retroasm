Assembler
=========

Numbers
-------

The preferred syntax for hexadecimal and binary numbers is to use the ``$``  and ``%`` prefix respectively:

.. code-block:: ca65

   HEXADECIMAL_NUMBER:     EQU     $6D
   BINARY_NUMBER:          EQU     %01101101

C/Python style prefixes ``0x`` and ``0b`` are also supported:

.. code-block:: nasm

   HEXADECIMAL_NUMBER:     EQU     0x6D
   BINARY_NUMBER:          EQU     0b01101101

Finally, the ``h`` and ``b`` suffixes can be used.
On a hexadecimal number starting with 'A'-'F', a leading zero has to be added to tell apart the number from a label.

.. code-block:: nasm

   HEXADECIMAL_NUMBER:     EQU     6Dh
   HIGHER_HEX_NUMBER:      EQU     0C9h
   BINARY_NUMBER:          EQU     01101101b




Command Line Syntax
-------------------

.. click:: retroasm.cmdline:asm
   :prog: retro asm
   :nested: full
