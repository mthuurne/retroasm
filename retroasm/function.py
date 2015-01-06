from .codeblock import ArgumentConstant
from .expression import Expression
from .storage import LocalReference, checkStorage
from .types import IntType, Reference

from inspect import signature

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
            elif code.constants[code.retCid].type != retType:
                raise ValueError(
                    'function "%s" has return type "%s", '
                    'but its code block has return type "%s"'
                    % (name, retType, code.constants[code.retCid].type)
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

class FunctionCall(Expression):
    '''Expression that represents the value returned by a user-defined function.
    '''
    __slots__ = ('_func', '_args')

    func = property(lambda self: self._func)
    args = property(lambda self: self._args)

    def __init__(self, func, args):
        if not isinstance(func, Function):
            raise TypeError('func must be Function, got %s' % type(func))
        if len(args) != len(func.args):
            raise ValueError(
                'argument count mismatch: %s takes %d argument(s), '
                'but %d argument(s) provided'
                % (func.name, len(func.args), len(args))
                )
        Expression.__init__(self, func.retType)
        self._func = func
        self._args = tuple(Expression.checkScalar(arg) for arg in args)
        for i, (name, decl, value) in enumerate(self.iterArgNameDeclValue(), 1):
            if isinstance(decl, Reference):
                if not checkStorage(value):
                    raise ValueError(
                        'value for argument %d ("%s") is not a storage '
                        'location: %s'
                        % (i, name, value)
                        )
                if value.type != decl.type:
                    raise ValueError(
                        'argument %d ("%s") has type "%s", '
                        'but the provided storage ("%s") has type "%s"'
                        % (i, name, decl, value, value.type)
                        )

    def _ctorargs(self, *exprs, **kwargs):
        cls = self.__class__
        if exprs:
            raise ValueError('%s does not take star args' % cls.__name__)
        kwargs.setdefault('func', self._func)
        kwargs.setdefault('args', self._args)
        return signature(cls).bind(**kwargs)

    def __str__(self):
        return '%s(%s)' % (
            self._func.name,
            ', '.join(str(arg) for arg in self._args)
            )

    def _equals(self, other):
        return ( # pylint: disable=protected-access
            self._func is other._func and
            len(self._args) == len(other._args) and
            all(arg1 == arg2 for arg1, arg2 in zip(self._args, other._args))
            )

    def _checkScalar(self):
        if self._type is None:
            raise ValueError(
                'attempt to use result of function "%s" that returns no value'
                % self._func.name
                )

    def _complexity(self):
        # Functions will be inlined when forming code blocks.
        return 1 << 50

    def iterArgNameDeclValue(self):
        '''Iterates through the arguments of this function call, where each
        element is a tuple of the name, declaration and value of the argument.
        '''
        for (name, decl), value in zip(self._func.args.items(), self._args):
            yield name, decl, value
