from .codeblock_builder import SemanticsCodeBlockBuilder
from .expression_builder import emitCodeFromStatements
from .expression_parser import ParseError, parseStatement
from .function import Function
from .linereader import DelayedError
from .types import ReferenceType

def _parseBody(reader):
    '''Parses the lines of a code block, yielding the statements.
    The full block is parsed, even in the presence of errors.
    Errors are appended to the given LineReader as they are discovered.
    '''
    for line in reader.iterBlock():
        try:
            yield parseStatement(line, reader.getLocation())
        except ParseError as ex:
            reader.error(
                'failed to parse statement: %s', ex, location=ex.location
                )

def createFunc(reader, funcName, retType, args, globalNamespace):
    headerLocation = reader.getLocation()

    builder = SemanticsCodeBlockBuilder(globalNamespace)
    namespace = builder.namespace
    for argName, argDecl in args.items():
        if isinstance(argDecl, ReferenceType):
            namespace.addReferenceArgument(
                argName, argDecl.type, headerLocation
                )
        else:
            namespace.addValueArgument(
                builder, argName, argDecl, headerLocation
                )
    if retType is not None and not isinstance(retType, ReferenceType):
        namespace.addVariable('ret', retType, headerLocation)

    try:
        with reader.checkErrors():
            emitCodeFromStatements(reader, builder, _parseBody(reader), retType)
    except DelayedError:
        code = None
    else:
        try:
            code = builder.createCodeBlock(log=reader)
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
