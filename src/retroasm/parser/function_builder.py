from __future__ import annotations

from collections.abc import Iterator, Mapping
from typing import cast

from ..codeblock_builder import SemanticsCodeBlockBuilder
from ..function import Function
from ..input import BadInput, DelayedError, ErrorCollector, InputLocation
from ..namespace import GlobalNamespace, LocalNamespace
from ..reference import Reference, SingleStorage
from ..storage import ArgStorage
from ..types import IntType, ReferenceType
from .expression_builder import emit_code_from_statements
from .expression_nodes import ParseNode
from .expression_parser import parse_statement
from .linereader import DefLineReader


def _parse_body(
    reader: DefLineReader, collector: ErrorCollector
) -> Iterator[ParseNode]:
    """
    Parses the lines of a code block, yielding the statements.
    The full block is parsed, even in the presence of errors.
    Errors are appended to `collector` as they are discovered.
    """
    for line in reader.iter_block():
        try:
            yield parse_statement(line)
        except BadInput as ex:
            collector.error(f"failed to parse statement: {ex}", location=ex.locations)


def create_func(
    reader: DefLineReader,
    collector: ErrorCollector,
    func_name_location: InputLocation,
    ret_type: None | IntType | ReferenceType,
    ret_type_location: InputLocation | None,
    args: Mapping[str, IntType | ReferenceType],
    arg_name_locations: Mapping[str, InputLocation],
    global_namespace: GlobalNamespace,
) -> Function:
    builder = SemanticsCodeBlockBuilder()
    namespace = LocalNamespace(global_namespace, builder)
    for arg_name, arg_decl in args.items():
        arg_loc = arg_name_locations[arg_name]
        if isinstance(arg_decl, ReferenceType):
            # Pass-by-reference.
            namespace.add_argument(arg_name, arg_decl.type, arg_loc)
        else:
            # Pass-by-value.
            # Create reference to passed argument.
            storage = ArgStorage(arg_name, arg_decl.width)
            arg_ref = Reference(SingleStorage(storage), arg_decl)
            # Add local variable.
            var_ref = namespace.add_variable(arg_name, arg_decl, arg_loc)
            # Store initial value.
            value = arg_ref.emit_load(builder, arg_loc)
            var_ref.emit_store(builder, value, arg_loc)

    ret_ref: Reference | None
    if ret_type is not None and not isinstance(ret_type, ReferenceType):
        assert ret_type_location is not None, ret_type
        ret_ref = namespace.add_variable("ret", ret_type, ret_type_location)

    try:
        with collector.check():
            body_nodes = _parse_body(reader, collector)
            emit_code_from_statements(
                collector, "function body", namespace, body_nodes, ret_type
            )

        if ret_type is None:
            ret_ref = None
        elif isinstance(ret_type, ReferenceType):
            ret_ref = cast(Reference, namespace.elements["ret"])

        code = namespace.create_code_block(
            ret_ref, collector=collector, location=func_name_location
        )
    except DelayedError:
        code = None

    try:
        func = Function(ret_type, args, code)
    except ValueError as ex:
        collector.error(
            f'error in function "{func_name_location.text}": {ex}',
            location=func_name_location,
        )
        code = None
        func = Function(ret_type, args, code)

    if code is not None:
        # Warn about unused arguments.
        # Note that simplification can remove usage that has no effect on
        # execution, but it is probably a good idea to warn about that too.
        code_args = code.arguments
        for arg_name in args.keys():
            if arg_name not in code_args:
                func_name = func_name_location.text
                collector.warning(
                    f'unused argument "{arg_name}" in function "{func_name}"',
                    location=arg_name_locations[arg_name],
                )

    return func
