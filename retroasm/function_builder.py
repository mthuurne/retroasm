from .codeblock_builder import CodeBlockBuilder, emitCodeFromStatements
from .expression_builder import BadExpression, createStatement
from .expression_parser import ParseError, parseStatement
from .function import Function
from .linereader import DelayedError
from .types import Reference

def _parseBody(reader, context):
    '''Parses the lines of a code block, yielding the statements.
    The full block is parsed, even in the presence of errors.
    Errors are appended to the given LineReader as they are discovered.
    '''
    for line in reader.iterBlock():
        try:
            tree = parseStatement(line, reader.getLocation())
            yield createStatement(tree, context)
        except ParseError as ex:
            reader.error('failed to parse statement: %s', ex, span=ex.span)
        except BadExpression as ex:
            reader.error('bad expression: %s', ex, location=ex.location)

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

    statements = _parseBody(reader, builder.context)
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
