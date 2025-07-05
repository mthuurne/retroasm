from __future__ import annotations

from collections.abc import Iterator, Mapping

from ..codeblock_builder import SemanticsCodeBlockBuilder, returned_bits
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
    reader: DefLineReader, collector: ErrorCollector, where_desc: str
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
            collector.error(
                f"error in {where_desc}: failed to parse statement: {ex}",
                location=ex.locations,
            )


def create_func(
    reader: DefLineReader,
    collector: ErrorCollector,
    func_name_location: InputLocation,
    args: Mapping[str, IntType | ReferenceType],
    arg_name_locations: Mapping[str, InputLocation],
    global_namespace: GlobalNamespace,
) -> Function:
    func_name = func_name_location.text
    builder = SemanticsCodeBlockBuilder()
    namespace = LocalNamespace(global_namespace)
    for arg_name, arg_decl in args.items():
        arg_loc = arg_name_locations[arg_name]
        try:
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
        except BadInput as ex:
            reader.skip_block()
            raise BadInput(
                f'error in argument "{arg_name}" of function "{func_name}": {ex}',
                *ex.locations,
            ) from ex

    try:
        with collector.check():
            where_desc = f'body of function "{func_name}"'
            body_nodes = _parse_body(reader, collector, where_desc)
            emit_code_from_statements(
                collector, where_desc, namespace, builder, body_nodes
            )
    except DelayedError:
        code_errors = True
    else:
        code_errors = False

    # Note that it is possible we might not find "ret" in case of errors,
    # but the alternative is to not define the function at all.
    # Time will tell which approach is more helpful to the end user.
    ret_ref = namespace.elements.get("ret")
    assert ret_ref is None or isinstance(ret_ref, Reference), ret_ref

    if code_errors:
        code = None
    else:
        try:
            with collector.check():
                code = builder.create_code_block(
                    returned_bits(ret_ref),
                    collector=collector,
                    location=func_name_location,
                )
        except DelayedError:
            code = None

    ret_type = None if ret_ref is None else ret_ref.type
    try:
        func = Function(ret_type, args, code)
    except ValueError as ex:
        collector.error(
            f'error in function "{func_name}": {ex}',
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
                collector.warning(
                    f'unused argument "{arg_name}" in function "{func_name}"',
                    location=arg_name_locations[arg_name],
                )

    return func
