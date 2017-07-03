from .codeblock import ArgumentValue, ComputedConstant, Store
from .storage import IOStorage, RefArgStorage
from .types import IntType, ReferenceType

class Function:

    def __init__(self, name, retType, args, code):
        if code is not None:
            # Check consistency between declared return type and code block.
            if retType is None:
                if code.retRef is not None:
                    raise ValueError(
                        'function "%s" has no return type, '
                        'but its code block defines "ret"' % name
                        )
            elif isinstance(retType, ReferenceType):
                if code.retRef is None:
                    raise ValueError(
                        'function "%s" should return a reference '
                        'but does not define "ret"' % name
                        )
            else: # returns a value
                if code.retRef is None:
                    raise ValueError(
                        'missing return value assignment in function "%s"'
                        % name
                        )

        self.name = name
        self.retType = retType
        self.args = args
        self.code = code

    def __repr__(self):
        return 'Function(%r, %r, %r, %r)' % (
            self.name, self.retType, self.args, self.code
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
        For pass-by-value arguments, an ArgumentValue is returned.
        For pass-by-reference arguments, a RefArgStorage is returned.
        If the argument does not occur in the code block, None is returned.
        If no argument with the given name existed when the function was
        created, KeyError is raised.
        If this Function doesn't have a code block, ValueError is raised.
        '''
        if self.code is None:
            raise ValueError('Function does not have a code block')
        arg = self.args[argName]
        if isinstance(arg, IntType):
            # Look for an ArgumentValue with the same name.
            def iterExprs():
                for const in self.code.constants.values():
                    if isinstance(const, ComputedConstant):
                        yield const.expr
                for node in self.code.nodes:
                    if isinstance(node, Store):
                        yield node.expr
                for storage in self.code.storages:
                    if isinstance(storage, IOStorage):
                        yield storage.index
            for expr in iterExprs():
                for value in expr.iterInstances(ArgumentValue):
                    if value.name == argName:
                        return value
        elif isinstance(arg, ReferenceType):
            # Look for a RefArgStorage with the same name.
            for storage in self.code.storages:
                if isinstance(storage, RefArgStorage):
                    if storage.name == argName:
                        return storage
        else:
            assert False, arg
        return None
