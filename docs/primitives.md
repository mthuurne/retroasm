Primitives
==========

While the instruction set definition language is pretty low-level, for analysis we want even lower-level primitives. Those primitives and how higher-level objects are built from them are described in this document.

Expression
----------

An *expression* is a tree containing literals and constants in its leaves and mathematical operators in its inner nodes. For example `A + 1` is represented by an expression tree with an `add` operator at its root and the constant `A` and the literal `1` as its children.

All expressions are computed using unlimited-width signed integers. Conversion from/to limited-width types happens on load/store.

Storage
-------

A *storage* is an atomic location where bits can be loaded from and stored into. Typical examples are CPU registers, I/O ports and memory at a specific address.

A storage has a width (how many bits it contains) and provides information that can be used to eliminate redundant loads and stores.

Bit String
----------

A *bit string* is similar to a storage, but not atomic. This means it's possible to concatenate or slice bit strings. For example the 8-bit registers `H` and `L` can be combined into a single 16-bit register `HL` and load/stores from/to `HL` will be split across `H` and `L`.

Bit strings are created from storages, expressions (fixed value) or by concatenating or slicing other bit strings.

Reference
---------

A *reference* adds a type to a bit string. Since RetroAsm currently only supports integers, in practice this means it adds sign extension when a signed value is loaded.

Code Block
----------

A *code block* contains a series of load/store nodes, that each load from or store into one storage.

There is no final design yet for how branching will be handled, but the currently linear series of nodes will have to be replaced by some kind of directed graph.

Mathematical computations are handled by expressions, not by nodes. For example `A := A + 1` would be represented by two nodes: `load V from A` and `store V + 1 into A`; there is no node for the addition.

Values and references are passed into code blocks via *arguments*, which are represented by named storages. These are substituted when a code block is called.

Values and references are passed out of code blocks via *returned bit strings*. Unlike arguments these are not named, but identified by their position (index) in the series of returned bit strings.

Function
--------

A *function* is a high-level wrapper around a code block. Similar to how a reference wraps a bit string, it adds typing to the arguments and returned value/reference. Unlike code blocks, a function can return at most one value/reference.

Note that code block arguments and returned bit strings are both using a pass-by-reference mechanism. Functions support pass-by-value as well; this is emulated using load/store (argument) and fixed values (return).
