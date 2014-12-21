from .codeblock import (
    ArgumentConstant, CodeBlockBuilder, emitCodeFromAssignments
    )
from .expression import LocalReference, LocalValue
from .linereader import DelayedError

class Function:

    def __init__(self, name, args, code):
        self.name = name
        self.args = args
        self.code = code

    def __repr__(self):
        return 'Function(%s, %s, %s)' % (
            repr(self.name), repr(self.args), repr(self.code)
            )

    def __str__(self):
        return 'func %s(%s)' % (
            self.name,
            ', '.join(arg.formatDecl() for arg in self.args.values())
            )

    def dump(self):
        print(str(self))
        if self.code is None:
            print('    no code')
        else:
            self.code.dump()

    def findArg(self, argName):
        '''Searches the representation of the argument with the given name in
        this function's code block.
        For pass-by-reference, a LocalReference is returned. For pass-by-value,
        either a LocalValue or an ArgumentConstant is returned. If the argument
        does not occur in the code block, None is returned. If no argument with
        the given name existed when the function was created, KeyError is
        raised. If this Function doesn't have a code block, ValueError is
        raised.
        '''
        if self.code is None:
            raise ValueError('Function does not have a code block')
        arg = self.args[argName]
        if isinstance(arg, LocalValue):
            # If the code block was simplified, the LocalValue will have been
            # replaced by an ArgumentConstant.
            for const in self.code.constants.values():
                if isinstance(const, ArgumentConstant):
                    if const.name == argName:
                        return const
        for ref in self.code.references.values():
            if isinstance(ref, (LocalReference, LocalValue)):
                if ref.name == argName:
                    assert ref is arg, (ref, arg)
                    return ref
        return None

def createFunc(reader, name, args, assignments):
    headerLocation = reader.getLocation()
    builder = CodeBlockBuilder()
    try:
        with reader.checkErrors():
            emitCodeFromAssignments(reader, builder, assignments)
    except DelayedError:
        code = None
    else:
        code = builder.createCodeBlock()
        code.simplify()
    func = Function(name, args, code)

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
