from .codeblock import (
    ArgumentConstant, ComputedConstant, ConstantValue, Load, LoadedConstant,
    Store
    )
from .codeblock_simplifier import CodeBlockSimplifier
from .context import Context
from .expression import IntLiteral, truncate
from .function import Function
from .linereader import BadInput
from .storage import (
    ComposedStorage, FixedValue, IOChannel, IOReference, LocalReference,
    Register, Storage, Variable
    )
from .types import IntType, Reference, maskForWidth
from .utils import checkType

from itertools import chain

class CodeBlockBuilder:

    def __init__(self, context):
        self.constants = []
        self.references = []
        self.context = context

    def dump(self):
        '''Prints the current state of this code block builder on stdout.
        '''
        print('    constants:')
        for const in self.constants:
            if isinstance(const, ComputedConstant):
                print('        C%-2d =  %s' % (const.cid, const.expr))
            elif isinstance(const, LoadedConstant):
                print('        C%-2d <- R%d' % (const.cid, const.rid))
            elif isinstance(const, ArgumentConstant):
                print('        C%-2d :  %s' % (const.cid, const.name))
            else:
                assert False, const
        print('    references:')
        for rid, ref in enumerate(self.references):
            print('        %-4s R%-2d = %s' % ('u%d&' % ref.width, rid, ref))

    def emitCompute(self, expr):
        '''Returns a ConstantValue that represents the value computed by the
        given expression.
        '''
        if isinstance(expr, ConstantValue):
            return expr
        else:
            cid = len(self.constants)
            constant = ComputedConstant(cid, expr)
            self.constants.append(constant)
            return ConstantValue(cid, expr.mask)

    def _emitReference(self, storage):
        '''Adds a reference to the given storage, returning the reference ID.
        '''
        if not isinstance(storage, Storage):
            raise TypeError('expected Storage, got %s' % type(storage).__name__)
        if isinstance(storage, IOReference):
            if not isinstance(storage.index, ConstantValue):
                raise TypeError('I/O index must be ConstantValue')
        rid = len(self.references)
        self.references.append(storage)
        return rid

    def _addNamedReference(self, ref, location):
        rid = self._emitReference(ref)
        composed = ComposedStorage.single(rid, ref.width)
        self.context.define(ref.name, composed, location)
        return rid

    def emitVariable(self, name, refType, location):
        return self._addNamedReference(Variable(name, refType), location)

    def emitLocalReference(self, name, refType, location):
        return self._addNamedReference(LocalReference(name, refType), location)

    def emitIOReference(self, channel, index):
        indexConst = self.emitCompute(truncate(index, channel.addrWidth))
        return self._emitReference(IOReference(channel, indexConst))

    def emitFixedValue(self, expr, width):
        '''Emits a constant representing the result of the given expression.
        Returns the reference ID of the corresponding FixedValue.
        '''
        const = self.emitCompute(expr)
        return self._emitReference(FixedValue(const.cid, width))

    def defineReference(self, name, value, location):
        '''Defines a reference with the given name and value.
        Returns the given value.
        Raises NameExistsError if the name is already taken.
        '''
        checkType(value, ComposedStorage, 'value')
        self.context.define(name, value, location)
        return value

    def emitLoad(self, rid, location):
        '''Adds a node that loads a value from the referenced storage.
        Returns an expression that wraps the loaded value.
        '''
        raise NotImplementedError

    def emitStore(self, rid, expr, location):
        '''Adds a node that stores a value in the referenced storage.
        '''
        raise NotImplementedError

    def inlineFunctionCall(self, func, argMap, location):
        '''Inlines a call to the given function with the given arguments.
        Returns a ComposedStorage containing the value returned by the inlined
        function, or None if the function does not return anything.
        '''
        raise NotImplementedError

class BadGlobalOperation(BadInput):
    '''Raised when an operation is attempted that is not allowed in the global
    scope.
    '''

class GlobalCodeBlockBuilder(CodeBlockBuilder):

    def __init__(self):
        CodeBlockBuilder.__init__(self, Context())

    def emitRegister(self, reg, location):
        checkType(reg, Register, 'register')
        return self._addNamedReference(reg, location)

    def emitLoad(self, rid, location):
        raise BadGlobalOperation(
            'attempt to read from storage in global scope: %s'
            % self.references[rid],
            location
            )

    def emitStore(self, rid, expr, location):
        raise BadGlobalOperation(
            'attempt to write to storage in global scope: %s'
            % self.references[rid],
            location
            )

    def inlineFunctionCall(self, func, argMap, location):
        raise BadGlobalOperation(
            'attempt to call function ("%s") in global scope' % func.name,
            location
            )

class _LocalContext(Context):
    '''A context for local blocks, that can import entries from the global
    context on demand.
    Its goal is to avoid a lot of redundant references when a block is first
    created: although the simplifier can remove those, that is a pretty
    inefficient operation which should be applied to non-trivial cases only.
    '''

    def __init__(self, localBuilder, globalBuilder):
        Context.__init__(self)
        self.localBuilder = localBuilder
        self.globalBuilder = globalBuilder
        self.importMap = {}

    def __contains__(self, key):
        return super().__contains__(key) or key in self.globalBuilder.context

    def __getitem__(self, key):
        try:
            return super().__getitem__(key)
        except KeyError:
            storage = self.globalBuilder.context[key]
            if isinstance(storage, (Function, IOChannel)):
                pass
            elif isinstance(storage, ComposedStorage):
                storage = ComposedStorage((
                    (self._importReference(rid), index, width)
                    for rid, index, width in storage
                    ))
            else:
                assert False, (key, repr(storage))
            self.elements[key] = storage
            self.locations[key] = None
            return storage

    def _importReference(self, globalRid):
        '''Imports the given global reference ID into the local context.
        Returns the local reference ID.
        '''
        importMap = self.importMap
        try:
            return importMap[globalRid]
        except KeyError:
            globalBuilder = self.globalBuilder
            localBuilder = self.localBuilder
            ref = globalBuilder.references[globalRid]
            if isinstance(ref, FixedValue):
                # Import constant ID as well.
                const = globalBuilder.constants[ref.cid]
                assert isinstance(const, ComputedConstant), repr(const)
                expr = const.expr
                cid = len(localBuilder.constants)
                localBuilder.constants.append(ComputedConstant(cid, expr))
                ref = FixedValue(cid, ref.width)
            # pylint: disable=protected-access
            localRid = localBuilder._emitReference(ref)
            importMap[globalRid] = localRid
            return localRid

class LocalCodeBlockBuilder(CodeBlockBuilder):

    def __init__(self, globalBuilder):
        context = _LocalContext(self, globalBuilder)
        CodeBlockBuilder.__init__(self, context)
        self.nodes = []

    def dump(self):
        super().dump()
        print('    nodes:')
        for node in self.nodes:
            print('        %s' % node)

    def createCodeBlock(self, log=None):
        '''Returns a CodeBlock object containing the items emitted so far.
        The state of the builder does not change.
        Raises ValueError if this builder does not represent a valid code block.
        If a log is provided, errors are logged individually as well.
        '''
        code = CodeBlockSimplifier(self.constants, self.references, self.nodes)

        # Check for reading of uninitialized variables.
        ununitializedLoads = []
        initializedVariables = set()
        for node in code.nodes:
            rid = node.rid
            ref = code.references[rid]
            if isinstance(ref, Variable):
                if isinstance(node, Load):
                    if rid not in initializedVariables:
                        ununitializedLoads.append(node)
                elif isinstance(node, Store):
                    initializedVariables.add(rid)
        if ununitializedLoads:
            if log is not None:
                for load in ununitializedLoads:
                    log.error(
                        'variable "%s" is read before it is initialized'
                        % code.references[load.rid].decl,
                        location=load.location
                        )
            raise ValueError(
                'Code block reads uninitialized variable(s): %s' % ', '.join(
                    code.references[load.rid].decl
                    for load in ununitializedLoads
                    )
                )

        if 'ret' in self.context:
            if not any(
                    isinstance(ref, Variable) and ref.name == 'ret'
                    for ref in code.references.values()
                    ):
                assert code.retRef is None, code.retRef
                code.retRef = self.context['ret']

        # Finalize code block.
        code.simplify()
        code.freeze()
        return code

    def emitLoad(self, rid, location):
        ref = self.references[rid]
        if isinstance(ref, FixedValue):
            cid = ref.cid
        else:
            cid = len(self.constants)
            self.nodes.append(Load(cid, rid, location))
            self.constants.append(LoadedConstant(cid, rid))
        return ConstantValue(cid, maskForWidth(ref.width))

    def emitStore(self, rid, expr, location):
        constant = self.emitCompute(expr)
        self.nodes.append(Store(constant.cid, rid, location))

    def emitValueArgument(self, name, decl, location):
        '''Adds a passed-by-value argument to this code block.
        The initial value is represented by an ArgumentConstant and is loaded
        into the corresponding Variable.
        Returns the reference ID of the corresponding Variable.
        '''
        assert isinstance(decl, IntType), decl

        # Add ArgumentConstant.
        cid = len(self.constants)
        constant = ArgumentConstant(name, cid)
        self.constants.append(constant)

        # Store initial value.
        rid = self.emitVariable(name, decl, location)
        self.nodes.insert(0, Store(cid, rid, location))

        return rid

    def inlineFunctionCall(self, func, argMap, location):
        code = func.code
        if code is None:
            # Missing body, probably because of earlier errors.
            return IntLiteral(0)

        argMap = dict(argMap)
        newMap = {}
        for name, decl in func.args.items():
            value = argMap.pop(name)
            if isinstance(decl, IntType):
                value = truncate(value, decl.width)
            elif isinstance(decl, Reference):
                pass
            else:
                assert False, decl
            newMap[name] = value
        if argMap:
            raise KeyError(
                'Non-existing arguments passed: %s' % ', '.join(argMap.keys())
                )

        return self.inlineBlock(code, newMap)

    def inlineBlock(self, code, context):
        '''Inlines another code block into this one.
        Returns a ComposedStorage containing the value returned by the inlined
        block, or None if the inlined block does not return anything.
        '''
        constants = self.constants
        references = self.references

        # Map old constant IDs to new IDs; don't copy yet.
        cidMap = dict(
            (oldCid, newCid)
            for newCid, oldCid in enumerate(
                code.constants.keys(), len(constants)
                )
            )

        # For each old rid, create a corresponding storage in this block.
        refMap = {}
        for rid, ref in code.references.items():
            if isinstance(ref, LocalReference):
                storage = context[ref.name]
                assert ref.width == storage.width, (ref.width, storage.width)
            else:
                if isinstance(ref, FixedValue):
                    newRef = FixedValue(cidMap[ref.cid], ref.width)
                else:
                    # Shallow copy because references are immutable.
                    newRef = ref
                newRid = len(references)
                references.append(newRef)
                storage = ComposedStorage.single(newRid, newRef.width)
            refMap[rid] = storage

        # Copy constants.
        for cid, const in code.constants.items():
            assert cid == const.cid, const
            if isinstance(const, ArgumentConstant):
                value = context[const.name]
                constants.append(ComputedConstant(cidMap[const.cid], value))
            elif isinstance(const, ComputedConstant):
                def substCid(expr):
                    if isinstance(expr, ConstantValue):
                        return ConstantValue(cidMap[expr.cid], expr.mask)
                    else:
                        return None
                constants.append(ComputedConstant(
                    cidMap[const.cid],
                    const.expr.substitute(substCid)
                    ))
            elif isinstance(const, LoadedConstant):
                # Will be filled in when Load node is copied.
                constants.append(None)
            else:
                assert False, const

        # Substitute index constants.
        # This cannot be done when originally copying the references
        # because at that time the constants haven't been added yet.
        for composedStorage in refMap.values():
            for rid, index_, width_ in composedStorage:
                ref = references[rid]
                if isinstance(ref, IOReference):
                    index = ref.index
                    references[rid] = IOReference(
                        ref.channel,
                        ConstantValue(cidMap[index.cid], index.mask)
                        )

        # Copy nodes.
        for node in code.nodes:
            composedStorage = refMap[node.rid]
            newCid = cidMap[node.cid]
            if isinstance(node, Load):
                value = composedStorage.emitLoad(self, node.location)
                constants[newCid] = ComputedConstant(newCid, value)
            elif isinstance(node, Store):
                value = ConstantValue(newCid, -1)
                composedStorage.emitStore(self, value, node.location)
            else:
                assert False, node

        # Determine return value.
        retRef = code.retRef
        return None if retRef is None else ComposedStorage(chain.from_iterable(
            refMap[rid].slice(index, width)
            for rid, index, width in retRef
            ))
