RetroAsm
========

https://github.com/mthuurne/retroasm/

.. contents:: Contents:


Assembly Toolkit
----------------

RetroAsm is a toolkit for assembly language. It consists of:

* instruction set definitions
* library (Python) for assembly analysis and manipulation
* command line tools: assembler, disassembler etc.

Its initial focus is on handling CPUs used in 80's and 90's computers, but this could be expanded to more modern CPUs over time.

Is This a Table-driven Assembler?
---------------------------------

Yes, but it's more than that. The instruction set definitions contain not only the encoding (opcodes) and mnemonics, but also semantics (behavior) for all instructions. This opens up all kinds of interesting applications: (none of these are written yet)

* static code checking for assembly
* automatic CPU emulator generation
* tracing disassembler that can tell apart code from data

RetroAsm is a bit like `LLVM <https://llvm.org/>`_\ , but focused on assembly instead of high-level languages. Also RetroAsm is specifically designed to be used in both directions: mnemonics to opcodes and vice versa, while LLVM is designed for compilation and not for decompilation.

Current State
-------------

RetroAsm isn't ready for any kind of practical use. It does a lot of cool things, but none of its tools are complete enough for actual work.

Until the 1.0 release, expect backwards incompatible changes everywhere: in the instruction set definition language, in the API, in the tools command line syntax etc.

Shell Completion
----------------

If you're using Bash:

.. code-block::

   $ mkdir -p ~/.local/share/bash-completion/completions
   $ _RETRO_COMPLETE=source_bash retro > ~/.local/share/bash-completion/completions/retroasm


On every new shell you start, command lines that start with ``retro`` should now be completed in a meaningful way when you press Tab.

If you're using a different shell, please consult the `Click documentation <https://click.palletsprojects.com/en/7.x/bashcomplete/>`_.

Development
-----------

If you want to try RetroAsm, start by cloning the Git repository:

.. code-block::

   $ git clone https://github.com/mthuurne/retroasm.git


RetroAsm requires Python version 3.9 or later. If this is not available for your system, you can use for example `pyenv <https://github.com/pyenv/pyenv>`_ to build it.

RetroAsm uses the `Poetry build system <https://python-poetry.org/>`_ for managing its development environment. Using the `recommended installation procedure <https://python-poetry.org/docs/#installation>`_ instead of pip helps separate Poetry's dependencies from those of the software it manages.

Start a shell in the virtual environment managed by Poetry:

.. code-block::

   $ poetry shell


Install RetroAsm and its runtime and development dependencies:

.. code-block::

   $ poetry install


One of the tools Poetry will install for you is `Invoke <https://www.pyinvoke.org/>`_\ , which can be used to perform a few useful developer tasks. You can see the full list of tasks by running:

.. code-block::

   $ inv --list


The task you will probably want to run first is ``docs``\ , which generates the documentation files:

.. code-block::

   $ inv docs


Now you can read the documentation in ``output/docs/``.

RetroAsm uses `pre-commit <https://pre-commit.com/>`_ to automatically check and reformat source code before it is committed. Running the following command once in your Git work area sets up the pre-commit hooks:

.. code-block::

   $ pre-commit install
