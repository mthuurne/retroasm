from typing import Iterator, Mapping, Optional, Union, cast

from .codeblock_builder import SemanticsCodeBlockBuilder
from .expression_builder import emitCodeFromStatements
from .expression_parser import ParseError, ParseNode, parseStatement
from .function import Function
from .linereader import DefLineReader, DelayedError, InputLocation
from .namespace import GlobalNamespace, LocalNamespace
from .reference import Reference, SingleStorage
from .storage import ArgStorage
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
                'failed to parse statement: %s', ex, location=ex.locations
                )

def createFunc(reader: DefLineReader,
               funcNameLocation: InputLocation,
               retType: Union[None, IntType, ReferenceType],
               retTypeLocation: Optional[InputLocation],
               args: Mapping[str, Union[IntType, ReferenceType]],
               argNameLocations: Mapping[str, InputLocation],
               globalNamespace: GlobalNamespace
               ) -> Function:

    builder = SemanticsCodeBlockBuilder()
    namespace = LocalNamespace(globalNamespace, builder)
    for argName, argDecl in args.items():
        argLoc = argNameLocations[argName]
        if isinstance(argDecl, ReferenceType):
            # Pass-by-reference.
            namespace.addArgument(argName, argDecl.type, argLoc)
        else:
            # Pass-by-value.
            # Create reference to passed argument.
            storage = ArgStorage(argName, argDecl.width)
            argRef = Reference(SingleStorage(storage), argDecl)
            # Add local variable.
            varRef = namespace.addVariable(argName, argDecl, argLoc)
            # Store initial value.
            value = argRef.emitLoad(builder, argLoc)
            varRef.emitStore(builder, value, argLoc)
    retRef: Optional[Reference]
    if retType is not None and not isinstance(retType, ReferenceType):
        assert retTypeLocation is not None, retType
        retRef = namespace.addVariable('ret', retType, retTypeLocation)

    try:
        with reader.checkErrors():
            bodyNodes = _parseBody(reader)
            emitCodeFromStatements(
                reader, 'function body', namespace, bodyNodes, retType
                )
    except DelayedError:
        code = None
    else:
        if retType is None:
            retRef = None
        elif isinstance(retType, ReferenceType):
            retRef = cast(Reference, namespace.elements['ret'])

        try:
            code = namespace.createCodeBlock(
                retRef, log=reader, location=funcNameLocation
                )
        except ValueError:
            code = None

    try:
        func = Function(retType, args, code)
    except ValueError as ex:
        reader.error(
            'error in function "%s": %s', funcNameLocation.text, ex,
            location=funcNameLocation
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
                    argName, funcNameLocation.text,
                    location=argNameLocations[argName]
                    )

    return func
