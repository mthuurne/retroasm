from .codeblock_builder import createCodeBlockFromAssignments
from .function import Function

def createFunc(reader, name, retType, args, assignments):
    headerLocation = reader.getLocation()
    code = createCodeBlockFromAssignments(reader, assignments)

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
