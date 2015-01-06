from .codeblock_builder import CodeBlockBuilder, emitCodeFromAssignments
from .function import Function
from .linereader import DelayedError
from .types import Reference

def createFunc(reader, name, retType, args, assignments):
    headerLocation = reader.getLocation()

    builder = CodeBlockBuilder()
    for argName, argDecl in args.items():
        if not isinstance(argDecl, Reference):
            builder.emitValueArgument(argName, argDecl)
    try:
        with reader.checkErrors():
            locations = emitCodeFromAssignments(reader, builder, assignments)
    except DelayedError:
        code = None
    else:
        try:
            code = builder.createCodeBlock(reader, locations)
        except ValueError:
            code = None

    try:
        func = Function(name, retType, args, code)
    except ValueError as ex:
        reader.error('%s', ex, location=headerLocation)
        code = None
        func = Function(name, retType, args, code)

    if code is not None:
        # Warn about unused arguments.
        # Note that simplification can remove usage that has no effect on
        # execution, but it is probably a good idea to warn about that too.
        for argName in args.keys():
            if func.findArg(argName) is None:
                reader.warning(
                    'unused argument "%s" in function "%s"',
                    argName, name, location=headerLocation
                    )

    return func
