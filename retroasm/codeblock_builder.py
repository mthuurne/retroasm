from .codeblock import ArgumentValue, Load, LoadedValue, Store
from .codeblock_simplifier import CodeBlockSimplifier
from .expression import optSlice
from .linereader import BadInput
from .namespace import LocalNamespace
from .reference import BitString, FixedValue, Reference, SingleStorage
from .storage import IOStorage, RefArgStorage, Variable
from .types import IntType, maskForWidth
from .utils import checkType

class CodeBlockBuilder:
    _scope = property()

    def __init__(self, namespace):
        self.namespace = namespace

    def dump(self):
        '''Prints the current state of this code block builder on stdout.
        '''
        if 'ret' in self.namespace:
            print('    return ref %s' % self.namespace['ret'])

    def _addNamedStorage(self, name, storage, typ, location):
        bits = SingleStorage(storage)
        ref = Reference(bits, typ)
        self.namespace.define(name, ref, location)
        return ref

    def emitVariable(self, name, typ, location):
        var = Variable(typ.width, self._scope)
        return self._addNamedStorage(name, var, typ, location)

    def emitIOReference(self, channel, index):
        addrWidth = channel.addrType.width
        truncatedIndex = optSlice(index, 0, addrWidth)
        storage = IOStorage(channel, truncatedIndex)
        bits = SingleStorage(storage)
        return Reference(bits, channel.elemType)

    def defineReference(self, name, value, location):
        '''Defines a reference with the given name and value.
        Returns the given value.
        Raises NameExistsError if the name is already taken.
        '''
        checkType(value, Reference, 'value')
        self.namespace.define(name, value, location)
        return value

    def emitLoadBits(self, storage, location):
        '''Loads the value from the given storage by emitting a Load node on
        this builder.
        Returns an expression that represents the loaded value.
        '''
        raise NotImplementedError

    def emitStoreBits(self, storage, value, location):
        '''Stores the value of the given expression in the given storage by
        emitting a Store node on this builder.
        '''
        raise NotImplementedError

    def inlineFunctionCall(self, func, argMap, location):
        '''Inlines a call to the given function with the given arguments.
        All arguments should be passed as references: value arguments should
        have their expression wrapped in a FixedValue.
        Returns a Reference containing the value returned by the inlined
        function, or None if the function does not return anything.
        '''
        raise NotImplementedError

class IllegalStateAccess(BadInput):
    '''Raised when an operation is attempted that reads or writes state
    in a situation where that is not allowed.
    '''

class StatelessCodeBlockBuilderMixin:
    '''A CodeBlockBuilder can inherit this to raise IllegalStateAccess when its
    users attempt touch any state, such as performing register access or I/O.
    '''

    def emitLoadBits(self, storage, location):
        raise IllegalStateAccess(
            'attempt to read state: %s' % storage,
            location
            )

    def emitStoreBits(self, storage, value, location):
        raise IllegalStateAccess(
            'attempt to write state: %s' % storage,
            location
            )

    def inlineFunctionCall(self, func, argMap, location):
        # TODO: This is probably overly strict: calling a function that does
        #       not touch state should be fine.
        raise IllegalStateAccess(
            'attempt to call function ("%s")' % func.name,
            location
            )

class GlobalCodeBlockBuilder(StatelessCodeBlockBuilderMixin, CodeBlockBuilder):
    _scope = 0

class LocalCodeBlockBuilder(CodeBlockBuilder):
    _scope = 1

    def __init__(self, parentNamespace):
        namespace = LocalNamespace(parentNamespace)
        CodeBlockBuilder.__init__(self, namespace)

    def emitValueArgument(self, name, typ, location):
        '''Adds a passed-by-value argument to this code block.
        The initial value is represented by an ArgumentValue and is stored
        into a corresponding local Variable that is created by this method.
        Returns the Reference to the corresponding Variable.
        '''
        raise NotImplementedError

class EncodingCodeBlockBuilder(
        StatelessCodeBlockBuilderMixin, LocalCodeBlockBuilder
        ):

    def emitValueArgument(self, name, typ, location):
        checkType(typ, IntType, 'value argument')
        width = typ.width
        value = ArgumentValue(name, maskForWidth(width))
        bits = FixedValue(value, width)
        ref = Reference(bits, typ)
        return self.defineReference(name, ref, location)

class SemanticsCodeBlockBuilder(LocalCodeBlockBuilder):

    def __init__(self, parentNamespace):
        LocalCodeBlockBuilder.__init__(self, parentNamespace)
        self.nodes = []

    def dump(self):
        for node in self.nodes:
            print('    %s (%s-bit)' % (node, node.storage.width))
        super().dump()

    def createCodeBlock(self, ret='ret', log=None):
        '''Returns a CodeBlock object containing the items emitted so far.
        The state of the builder does not change.
        If 'ret' is a reference, that will be used as the returned reference.
        If 'ret' is an existing name in this block's namespace, the reference
        with that name will be used as the returned reference.
        If 'ret' is None or a non-existing name, the created code block will
        not return anything.
        Raises ValueError if this builder does not represent a valid code block.
        If a log is provided, errors are logged individually as well.
        '''
        if isinstance(ret, str):
            ret = self.namespace.get(ret)
        code = CodeBlockSimplifier(self.nodes, ret)

        # Check for reading of uninitialized variables.
        ununitializedLoads = []
        initializedVariables = set()
        for node in code.nodes:
            storage = node.storage
            if isinstance(storage, Variable) and storage.scope == 1:
                if isinstance(node, Load):
                    if storage not in initializedVariables:
                        ununitializedLoads.append(node)
                elif isinstance(node, Store):
                    initializedVariables.add(storage)
        if ununitializedLoads:
            if log is not None:
                for load in ununitializedLoads:
                    log.error(
                        'variable is read before it is initialized',
                        location=load.location
                        )
            raise ValueError(
                'Code block reads %d uninitialized variable(s)'
                % len(ununitializedLoads)
                )

        # Finalize code block.
        code.simplify()
        code.freeze()
        return code

    def emitValueArgument(self, name, typ, location):
        '''Adds a passed-by-value argument to this code block.
        The initial value is represented by an ArgumentValue and is stored
        into the corresponding Variable.
        Returns the Reference to the corresponding Variable.
        '''
        checkType(typ, IntType, 'value argument')
        value = ArgumentValue(name, maskForWidth(typ.width))

        # Add Variable.
        ref = self.emitVariable(name, typ, location)

        # Store initial value.
        ref.emitStore(self, value, location)

        return ref

    def emitReferenceArgument(self, name, typ, location):
        storage = RefArgStorage(name, typ.width)
        return self._addNamedStorage(name, storage, typ, location)

    def emitLoadBits(self, storage, location):
        load = Load(storage, location)
        self.nodes.append(load)
        return load.expr

    def emitStoreBits(self, storage, value, location):
        self.nodes.append(Store(value, storage, location))

    def inlineFunctionCall(self, func, argMap, location):
        code = func.code
        if code is None:
            # Missing body, probably because of earlier errors.
            return None

        badArgs = argMap.keys() - func.args.keys()
        if badArgs:
            raise KeyError(
                'Non-existing arguments passed: %s' % ', '.join(badArgs)
                )
        missingArgs = func.args.keys() - argMap.keys()
        if missingArgs:
            raise KeyError(
                'Missing values for arguments: %s' % ', '.join(missingArgs)
                )

        return self.inlineBlock(code, argMap.__getitem__)

    def inlineBlock(self, code, argFetcher):
        '''Inlines another code block into this one.
        The given argument fetcher function, when called with an argument name,
        should return the bit string passed for that argument, or None if the
        argument should remain an argument in the inlined block.
        Returns a Reference containing the value returned by the inlined
        block, or None if the inlined block does not return anything.
        '''

        loadResults = {}
        def substExpr(expr):
            if isinstance(expr, ArgumentValue):
                arg = argFetcher(expr.name)
                if arg is not None:
                    return arg.emitLoad(self, None)
            elif isinstance(expr, LoadedValue):
                return loadResults.get(expr)
            return None
        def importExpr(expr):
            return expr.substitute(substExpr)

        def importStorageUncached(storage):
            if isinstance(storage, RefArgStorage):
                bits = argFetcher(storage.name)
                if bits is not None:
                    assert isinstance(bits, BitString), repr(bits)
                    assert storage.width == bits.width, \
                        (storage.width, bits.width)
                    return bits
                newStorage = storage
            else:
                newStorage = storage.substituteExpressions(importExpr)
            return SingleStorage(newStorage)
        storageCache = {}
        def importStorage(storage):
            '''Returns a bit string containing the imported version of the given
            storage.
            '''
            bits = storageCache.get(storage)
            if bits is None:
                bits = importStorageUncached(storage)
                storageCache[storage] = bits
            return bits

        # Copy nodes.
        for node in code.nodes:
            expr = node.expr
            bits = importStorage(node.storage)
            if isinstance(node, Load):
                assert isinstance(expr, LoadedValue), expr
                value = bits.emitLoad(self, node.location)
                loadResults[expr] = value
            elif isinstance(node, Store):
                newExpr = importExpr(expr)
                bits.emitStore(self, newExpr, node.location)
            else:
                assert False, node

        # Determine return value.
        retRef = code.retRef
        if retRef is None:
            return None
        else:
            return retRef.substitute(importStorage, importExpr)
