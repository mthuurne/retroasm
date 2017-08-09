from .codeblock import ArgumentValue, Load, LoadedValue, Store
from .codeblock_simplifier import CodeBlockSimplifier
from .linereader import BadInput
from .reference import BitString, SingleStorage
from .storage import RefArgStorage, Variable

class CodeBlockBuilder:

    def dump(self):
        '''Prints the current state of this code block builder on stdout.
        '''
        pass

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

class StatelessCodeBlockBuilder(CodeBlockBuilder):
    '''A CodeBlockBuilder that raises IllegalStateAccess when its users attempt
    touch any state, such as performing register access or I/O.
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

class SemanticsCodeBlockBuilder(CodeBlockBuilder):

    def __init__(self):
        self.nodes = []

    def dump(self):
        for node in self.nodes:
            print('    %s (%s-bit)' % (node, node.storage.width))
        super().dump()

    def createCodeBlock(self, returned, log=None):
        '''Returns a CodeBlock object containing the items emitted so far.
        The state of the builder does not change.
        The 'returned' sequence contains the bits strings that will be the
        returned values for the created block.
        Raises ValueError if this builder does not represent a valid code block.
        If a log is provided, errors are logged individually as well.
        '''
        code = CodeBlockSimplifier(self.nodes, returned)

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

        returned = self.inlineBlock(code, argMap.__getitem__)
        if len(returned) == 1:
            return returned[0]
        else:
            assert len(returned) == 0, returned
            return None

    def inlineBlock(self, code, argFetcher=None):
        '''Inlines another code block into this one.
        The given argument fetcher function, when called with an argument name,
        should return the bit string passed for that argument, or None if the
        argument should remain an argument in the inlined block.
        Returns a list of BitStrings containing the values returned by the
        inlined block.
        '''
        if argFetcher is None:
            # No substitution takes place, so we can copy nodes and returned
            # bits as-is.
            self.nodes += code.nodes
            return code.returned

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
        return [
            retBits.substitute(importStorage, importExpr)
            for retBits in code.returned
            ]
