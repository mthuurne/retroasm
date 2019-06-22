from typing import Iterator, Mapping, Union

from .codeblock_builder import SemanticsCodeBlockBuilder
from .expression_builder import emitCodeFromStatements
from .expression_parser import ParseError, ParseNode, parseStatement
from .function import Function
from .linereader import DefLineReader, DelayedError
from .namespace import GlobalNamespace, LocalNamespace
from .types import IntType, ReferenceType


def _parseBody(reader: DefLineReader) -> Iterator[ParseNode]:
    '''Parses the lines of a code block, yielding the statements.
    The full block is parsed, even in the presence of errors.
    Errors are appended to `reader` as they are discovered.
    '''
    for line in reader.iterBlock():
        try:
            yield parseStatement(line)
        except ParseError as ex:
            reader.error(
                'failed to parse statement: %s', ex, location=ex.location
                )

def createFunc(reader: DefLineReader,
               funcName: str,
               retType: Union[None, IntType, ReferenceType],
               args: Mapping[str, Union[IntType, ReferenceType]],
               globalNamespace: GlobalNamespace
               ) -> Function:
    headerLocation = reader.location

    builder = SemanticsCodeBlockBuilder()
    namespace = LocalNamespace(globalNamespace, builder)
    for argName, argDecl in args.items():
        if isinstance(argDecl, ReferenceType):
            namespace.addReferenceArgument(
                argName, argDecl.type, headerLocation
                )
        else:
            namespace.addValueArgument(argName, argDecl, headerLocation)
    if retType is not None and not isinstance(retType, ReferenceType):
        namespace.addVariable('ret', retType, headerLocation)

    try:
        with reader.checkErrors():
            bodyNodes = _parseBody(reader)
            emitCodeFromStatements(
                reader, 'function body', namespace, bodyNodes, retType
                )
    except DelayedError:
        code = None
    else:
        try:
            code = namespace.createCodeBlock(
                log=reader, location=headerLocation
                )
        except ValueError:
            code = None

    try:
        func = Function(retType, args, code)
    except ValueError as ex:
        reader.error(
            'error in function "%s": %s', funcName, ex, location=headerLocation
            )
        code = None
        func = Function(retType, args, code)

    if code is not None:
        # Warn about unused arguments.
        # Note that simplification can remove usage that has no effect on
        # execution, but it is probably a good idea to warn about that too.
        codeArgs = code.arguments
        for argName in args.keys():
            if argName not in codeArgs:
                reader.warning(
                    'unused argument "%s" in function "%s"',
                    argName, funcName, location=headerLocation
                    )

    return func
