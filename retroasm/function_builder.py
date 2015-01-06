from .codeblock_builder import CodeBlockBuilder, emitCodeFromAssignments
from .expression import Expression
from .expression_parser import parseExpr
from .function import Function
from .linereader import DelayedError
from .storage import LocalReference, Variable
from .types import IntType, Reference

from collections import ChainMap

def _parseAssignments(log, lines, context):
    '''Parses the given lines as a series of assignments, yields the
    assignments as pairs of expressions.
    The full sequence of lines is parsed, even in the presence of errors.
    Errors are appended to the given log as they are discovered.
    '''
    for line in lines:
        parts = line.split(':=')
        if len(parts) < 2:
            # No assignment.
            lhsStr = None
            rhsStr = line
        elif len(parts) == 2:
            lhsStr, rhsStr = parts
        else:
            log.error('multiple assignments in a single line')
            continue

        if lhsStr is None:
            lhs = None
        else:
            try:
                lhs = parseExpr(lhsStr, context)
            except ValueError as ex:
                log.error('error in left hand side of assignment: %s', ex)
                continue

        try:
            rhs = parseExpr(rhsStr, context)
            if lhs is not None:
                Expression.checkScalar(rhs)
        except ValueError as ex:
            log.error('error in right hand side of assignment: %s', ex)
            continue

        yield lhs, rhs

def createFunc(reader, funcName, retType, args, globalContext):
    headerLocation = reader.getLocation()

    builder = CodeBlockBuilder()
    for argName, argDecl in args.items():
        if isinstance(argDecl, Reference):
            builder.getReferenceID(LocalReference(argName, argDecl.type))
        else:
            builder.emitValueArgument(argName, argDecl)
    if retType is not None:
        builder.getReferenceID(Variable('ret', retType))

    localContext = dict(
        (name, builder.references[rid])
        for name, rid in builder.nameToReferenceID.items()
        )
    combinedContext = ChainMap(localContext, globalContext)

    assignments = _parseAssignments(reader, reader.iterBlock(), combinedContext)
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
