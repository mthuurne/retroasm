.. role:: raw-html-m2r(raw)
   :format: html


Primitives
==========

While the instruction set definition language is pretty low-level, for analysis we want even lower-level primitives. Those primitives and how higher-level objects are built from them are described in this document.

Expression
----------

An *expression* is a tree containing literals and constants in its leaves and mathematical operators in its inner nodes. For example ``A + 1`` is represented by an expression tree with an ``add`` operator at its root and the constant ``A`` and the literal ``1`` as its children.

The following types of expression nodes exist:


* integer literals (\ ``123``\ )
* constants (\ ``X``\ )
* integer complement (\ ``-X``\ )
* Boolean negation / zero test (\ ``!X``\ )
* sign test (\ ``X < 0``\ )
* sign extension
* bitwise operators: and, or, xor
* addition
* left/right shift by fixed amount (\ ``X << 4``\ )
* left/right shift by variable amount (\ ``X << Y``\ )

All expressions are computed using unlimited-width signed integers. Conversion from/to limited-width types happens on load/store. Loading an unsigned value uses implicit zero extension, loading a signed value wraps the loaded value in a sign extension node. When a value is stored in a limited-width location, it is truncated.

Subtraction
^^^^^^^^^^^

Subtraction is handled by adding the integer complement: ``A - B`` is parsed to ``A + -B``.

Bitwise Complement
^^^^^^^^^^^^^^^^^^

Bitwise complement is handled by bitwise *xor*\ : ``~A`` is parsed to ``A ^ -1``.

Comparison
^^^^^^^^^^

Comparisons are converted to primitives as follows:

.. list-table::
   :header-rows: 1

   * - Comparison operator
     - Definition language
     - Primitives
   * - equality
     - ``A == B``
     - ``!(A ^ B)``
   * - inequality
     - ``A != B``
     - ``!!(A ^ B)``
   * - lesser than
     - ``A < B``
     - ``A + -B < 0``
   * - greater than
     - ``A > B``
     - ``B + -A < 0``
   * - lesser or equal
     - ``A <= B``
     - ``!(B + -A < 0)``
   * - greater or equal
     - ``A >= B``
     - ``!(A + -B <  0)``


Note that ``X < 0`` is the sign test primitive, so for example ``A + -B < 0`` is the expression tree ``sign_test(add(A, complement(B)))``.

Shifting
^^^^^^^^

A shift by a fixed amount is equivalent to a shift by a variable amount where that variable amount is a literal. However, a shift by a fixed amount is a very common operation and offers so much more options for analysis that it's worth having a dedicated node type for it.

Truncation
^^^^^^^^^^

Truncation is an operation that preserves only the last *N* bits of a value, the rest is replaced by zeroes. There is no dedicated node type for truncation: a bit mask is computed with the value 2\ :raw-html-m2r:`<sup>*N*</sup>` - 1, meaning *N* consecutive ones in binary, and the truncation is handled by a bitwise *and* of the value and this mask.

Concatenation
^^^^^^^^^^^^^

A concatenation is performed by a bitwise *or* with a shifted value. For example, ``A;B`` is parsed to ``(A << wB) | B``\ , where ``wB`` is the width of ``B`` in bits. Note that the type system of the definition language requires the width of ``B`` to be fixed and known, so we can always use the "shift by fixed amount" primitive.

Slicing
^^^^^^^

Slices are decomposed into shifts and truncations. For example ``A[4:12]`` is parsed to ``(A >> 4) & $FF``\ , where ``$FF`` is an all-ones bit mask that's 12 - 4 = 8 bits wide.

Storage
-------

A *storage* is an atomic location where bits can be loaded from and stored into. Typical examples are CPU registers, I/O ports and memory at a specific address.

A storage has a width (how many bits it contains) and provides information that can be used to eliminate redundant loads and stores.

Bit String
----------

A *bit string* is similar to a storage, but not atomic. This means it's possible to concatenate or slice bit strings. For example the 8-bit registers ``H`` and ``L`` can be combined into a single 16-bit register ``HL`` and load/stores from/to ``HL`` will be split across ``H`` and ``L``.

Bit strings are created from storages, expressions (fixed value) or by concatenating or slicing other bit strings.

Reference
---------

A *reference* adds a type to a bit string. Since RetroAsm currently only supports integers, in practice this means it adds sign extension when a signed value is loaded.

Code Block
----------

A *code block* contains a series of load/store nodes, that each load from or store into one storage.

There is no final design yet for how branching will be handled, but the currently linear series of nodes will have to be replaced by some kind of directed graph.

Mathematical computations are handled by expressions, not by nodes. For example ``A := A + 1`` would be represented by two nodes: ``load V from A`` and ``store V + 1 into A``\ ; there is no node for the addition.

Values and references are passed into code blocks via *arguments*\ , which are represented by named storages. These are substituted when a code block is called.

Values and references are passed out of code blocks via *returned bit strings*. Unlike arguments these are not named, but identified by their position (index) in the series of returned bit strings.

Function
--------

A *function* is a high-level wrapper around a code block. Similar to how a reference wraps a bit string, it adds typing to the arguments and returned value/reference. Unlike code blocks, a function can return at most one value/reference.

Note that code block arguments and returned bit strings are both using a pass-by-reference mechanism. Functions support pass-by-value as well; this is emulated using load/store (argument) and fixed values (return).
