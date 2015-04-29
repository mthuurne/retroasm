from .codeblock import ArgumentConstant
from .storage import LocalReference
from .types import IntType, Reference

class Function:

    def __init__(self, name, retType, args, code):
        if code is not None:
            # Check consistency between declared return type and code block.
            if retType is None:
                if code.retCid is not None:
                    raise ValueError(
                        'function "%s" has no return type, '
                        'but its code block has a return value' % name
                        )
            elif code.retCid is None:
                raise ValueError(
                    'missing return value assignment in function "%s"' % name
                    )

        self.name = name
        self.retType = retType
        self.args = args
        self.code = code

    def __repr__(self):
        return 'Function(%s, %s, %s, %s)' % (
            repr(self.name), repr(self.retType), repr(self.args),
            repr(self.code)
            )

    def __str__(self):
        return 'func %s%s(%s)' % (
            '' if self.retType is None else '%s ' % self.retType,
            self.name,
            ', '.join(
                '%s %s' % (decl, name)
                for name, decl in self.args.items()
                )
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
        For pass-by-value arguments, an ArgumentConstant is returned.
        For pass-by-reference arguments, a LocalReference is returned.
        If the argument does not occur in the code block, None is returned.
        If no argument with the given name existed when the function was
        created, KeyError is raised.
        If this Function doesn't have a code block, ValueError is raised.
        '''
        if self.code is None:
            raise ValueError('Function does not have a code block')
        arg = self.args[argName]
        if isinstance(arg, IntType):
            # Look for an ArgumentConstant with the same name.
            for const in self.code.constants.values():
                if isinstance(const, ArgumentConstant):
                    if const.name == argName:
                        return const
        elif isinstance(arg, Reference):
            # Look for a LocalReference with the same name.
            for ref in self.code.references.values():
                if isinstance(ref, LocalReference):
                    if ref.name == argName:
                        return ref
        else:
            assert False, arg
        return None
