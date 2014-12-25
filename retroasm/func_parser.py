from .codeblock import (
    ArgumentConstant, CodeBlockBuilder, emitCodeFromAssignments
    )
from .expression import Expression, LocalReference, LocalValue
from .linereader import DelayedError

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
                    % (retType, code.constants[code.retCid].type)
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

def createFunc(reader, name, retType, args, assignments):
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

class FunctionCall(Expression):
    '''Expression that represents the value returned by a user-defined function.
    '''
    __slots__ = ('_func', '_args')

    func = property(lambda self: self._func)
    args = property(lambda self: self._args)

    def __init__(self, func, args):
        if not isinstance(func, Function):
            raise TypeError('func must be Function, got %s' % type(func))
        Expression.__init__(self, func.retType)
        self._func = func
        self._args = tuple(Expression.checkScalar(arg) for arg in args)

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
        return (
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
