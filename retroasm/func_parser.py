from .codeblock import CodeBlockBuilder, emitCodeFromAssignments

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
        self.code.dump()

def createFunc(log, name, args, assignments):
    builder = CodeBlockBuilder()
    emitCodeFromAssignments(log, builder, assignments)
    code = builder.createCodeBlock()
    code.simplify()
    return Function(name, args, code)
