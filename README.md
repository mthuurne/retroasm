RetroAsm
========

<https://github.com/mthuurne/retroasm/>

Assembly Toolkit
----------------

RetroAsm is a toolkit for assembly language. It consists of:

- instruction set definitions
- library (Python) for assembly analysis and manipulation
- command line tools: assembler, disassembler etc.

Its initial focus is on handling CPUs used in 80's and 90's computers, but this could be expanded to more modern CPUs over time.

Is This a Table-driven Assembler?
---------------------------------

Yes, but it's more than that. The instruction set definitions contain not only the encoding (opcodes) and mnemonics, but also semantics (behavior) for all instructions. This opens up all kinds of interesting applications: (none of these are written yet)

- static code checking for assembly
- automatic CPU emulator generation
- tracing disassembler that can tell apart code from data

RetroAsm is a bit like [LLVM](https://llvm.org/), but focused on assembly instead of high-level languages. Also RetroAsm is specifically designed to be used in both directions: mnemonics to opcodes and vice versa, while LLVM is designed for compilation and not for decompilation.

Current State
-------------

RetroAsm isn't ready for any kind of practical use. It does a lot of cool things, but none of its tools are complete enough for actual work.

Until the 1.0 release, expect backwards incompatible changes everywhere: in the instruction set definition language, in the API, in the tools command line syntax etc.

Development
-----------

If you want to try RetroAsm, start by cloning the Git repository:

    $ git clone https://github.com/mthuurne/retroasm.git

Make sure the `python` command starts Python version 3.8 or later. If this is not the default on your system, you can use for example [pyenv](https://github.com/pyenv/pyenv) or [virtualenv](https://virtualenv.pypa.io/) to set up an environment with the right Python version.

RetroAsm uses the [Poetry build system](https://poetry.eustace.io/) for managing its development environment. Using the [recommended installation procedure](https://github.com/sdispater/poetry#installation) instead of pip helps separate Poetry's dependencies from those of the software it manages.

Start a shell in the virtual environment managed by Poetry:

    $ poetry shell

Install RetroAsm and its runtime and development dependencies:

    $ poetry install

One of the tools Poetry will install for you is [Invoke](https://www.pyinvoke.org/), which can be used to perform a few useful developer tasks. You can see the full list of tasks by running:

    $ inv --list

The task you will probably want to run first is `docs`, which generates the documentation files:

    $ inv docs

Now you can read the documentation in `output/docs/`.
