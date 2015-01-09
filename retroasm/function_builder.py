from .codeblock_builder import CodeBlockBuilder, emitCodeFromStatements
from .expression_parser import parseStatement
from .function import Function
from .linereader import DelayedError
from .types import Reference

def _parseBody(log, lines, context):
    '''Parses the lines of a code block, yielding the statements.
    The full sequence of lines is parsed, even in the presence of errors.
    Errors are appended to the given log as they are discovered.
    '''
    for line in lines:
        try:
            yield parseStatement(line, context)
        except ValueError as ex:
            log.error('error parsing statement: %s', ex)

def createFunc(reader, funcName, retType, args, globalContext):
    headerLocation = reader.getLocation()

    builder = CodeBlockBuilder(globalContext)
    for argName, argDecl in args.items():
        if isinstance(argDecl, Reference):
            builder.emitLocalReference(argName, argDecl.type)
        else:
            builder.emitValueArgument(argName, argDecl)
    if retType is not None:
        builder.emitVariable('ret', retType)

    statements = _parseBody(reader, reader.iterBlock(), builder.context)
    try:
        with reader.checkErrors():
            locations = emitCodeFromStatements(reader, builder, statements)
    except DelayedError:
        code = None
    else:
        try:
            code = builder.createCodeBlock(reader, locations)
        except ValueError:
            code = None

    try:
        func = Function(funcName, retType, args, code)
    except ValueError as ex:
        reader.error('%s', ex, location=headerLocation)
        code = None
        func = Function(funcName, retType, args, code)

    if code is not None:
        # Warn about unused arguments.
        # Note that simplification can remove usage that has no effect on
        # execution, but it is probably a good idea to warn about that too.
        for argName in args.keys():
            if func.findArg(argName) is None:
                reader.warning(
                    'unused argument "%s" in function "%s"',
                    argName, funcName, location=headerLocation
                    )

    return func
