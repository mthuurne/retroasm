from .codeblock import (
    ArgumentConstant, BoundReference, ComputedConstant, ConstantValue, Load,
    LoadedConstant, Store
    )
from .codeblock_simplifier import CodeBlockSimplifier
from .context import Context
from .expression import (
    AndOperator, IntLiteral, LShift, OrOperator, optSlice, truncate
    )
from .function import Function
from .linereader import BadInput
from .storage import (
    FixedValue, IOChannel, IOStorage, Register, Storage, UnknownStorage,
    Variable
    )
from .types import IntType, Reference, maskForWidth
from .utils import checkType

from itertools import chain

class CodeBlockBuilder:

    def __init__(self, context):
        self.constants = []
        self.storages = []
        self.context = context

    def dump(self):
        '''Prints the current state of this code block builder on stdout.
        '''
        print('    constants:')
        for const in self.constants:
            if isinstance(const, ComputedConstant):
                print('        C%-2d =  %s' % (const.cid, const.expr))
            elif isinstance(const, LoadedConstant):
                print('        C%-2d <- S%d' % (const.cid, const.sid))
            elif isinstance(const, ArgumentConstant):
                print('        C%-2d :  %s' % (const.cid, const.name))
            else:
                assert False, const
        print('    storages:')
        for sid, storage in enumerate(self.storages):
            print('        S%-2d : %s  (%d-bit)' % (sid, storage, storage.width))

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

    def _addStorage(self, storage):
        '''Adds the given storage to this code block.
        Returns the storage ID.
        '''
        if not isinstance(storage, Storage):
            raise TypeError('expected Storage, got %s' % type(storage).__name__)
        if isinstance(storage, IOStorage):
            if not isinstance(storage.index, ConstantValue):
                raise TypeError('I/O index must be ConstantValue')
        sid = len(self.storages)
        self.storages.append(storage)
        return sid

    def _addNamedStorage(self, storage, location):
        sid = self._addStorage(storage)
        ref = BoundReference.single(sid, storage.width)
        self.context.define(storage.name, ref, location)
        return ref

    def emitVariable(self, name, refType, location):
        return self._addNamedStorage(Variable(name, refType), location)

    def emitIOReference(self, channel, index):
        indexConst = self.emitCompute(optSlice(index, 0, channel.addrWidth))
        sid = self._addStorage(IOStorage(channel, indexConst))
        return BoundReference.single(sid, channel.elemWidth)

    def emitFixedValue(self, expr, width):
        '''Emits a constant representing the result of the given expression.
        Returns the storage ID of the corresponding FixedValue.
        '''
        const = self.emitCompute(expr)
        return self._addStorage(FixedValue(const.cid, width))

    def defineReference(self, name, value, location):
        '''Defines a reference with the given name and value.
        Returns the given value.
        Raises NameExistsError if the name is already taken.
        '''
        checkType(value, BoundReference, 'value')
        self.context.define(name, value, location)
        return value

    def emitLoad(self, boundRef, location):
        '''Loads the value of the given bound reference by emitting Load nodes
        on this builder.
        Returns an expression that wraps the loaded value.
        '''
        raise NotImplementedError

    def emitStore(self, boundRef, expr, location):
        '''Stores the value of the given expression in the given bound reference
        by emitting Store nodes (and Load nodes for partial updates) on this
        builder.
        '''
        raise NotImplementedError

    def inlineFunctionCall(self, func, argMap, location):
        '''Inlines a call to the given function with the given arguments.
        Returns a BoundReference containing the value returned by the inlined
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
        return self._addNamedStorage(reg, location)

    def emitLoad(self, boundRef, location):
        raise BadGlobalOperation(
            'attempt to read from storage in global scope: %s'
            % boundRef.present(self.storages),
            location
            )

    def emitStore(self, boundRef, expr, location):
        raise BadGlobalOperation(
            'attempt to write to storage in global scope: %s'
            % boundRef.present(self.storages),
            location
            )

    def inlineFunctionCall(self, func, argMap, location):
        raise BadGlobalOperation(
            'attempt to call function ("%s") in global scope' % func.name,
            location
            )

class _LocalContext(Context):
    '''A context for local blocks, that can import entries from its parent
    context on demand.
    Its goal is to avoid a lot of redundant references when a block is first
    created: although the simplifier can remove those, that is a pretty
    inefficient operation which should be applied to non-trivial cases only.
    '''

    def __init__(self, localBuilder, parentBuilder):
        Context.__init__(self)
        self.localBuilder = localBuilder
        self.parentBuilder = parentBuilder
        self.importMap = {}

    def __contains__(self, key):
        return super().__contains__(key) or key in self.parentBuilder.context

    def __getitem__(self, key):
        try:
            return super().__getitem__(key)
        except KeyError:
            value = self.parentBuilder.context[key]
            if isinstance(value, (Function, IOChannel)):
                pass
            elif isinstance(value, BoundReference):
                value = BoundReference((
                    (self._importStorage(sid), index, width)
                    for sid, index, width in value
                    ))
            else:
                assert False, (key, repr(value))
            self.elements[key] = value
            self.locations[key] = None
            return value

    def _importStorage(self, parentSid):
        '''Imports the given parent storage ID into the local context.
        Returns the local storage ID.
        '''
        importMap = self.importMap
        try:
            return importMap[parentSid]
        except KeyError:
            parentBuilder = self.parentBuilder
            localBuilder = self.localBuilder
            storage = parentBuilder.storages[parentSid]
            if isinstance(storage, FixedValue):
                # Import constant ID as well.
                const = parentBuilder.constants[storage.cid]
                assert isinstance(const, ComputedConstant), repr(const)
                expr = const.expr
                cid = len(localBuilder.constants)
                localBuilder.constants.append(ComputedConstant(cid, expr))
                storage = FixedValue(cid, storage.width)
            # pylint: disable=protected-access
            localSid = localBuilder._addStorage(storage)
            importMap[parentSid] = localSid
            return localSid

class LocalCodeBlockBuilder(CodeBlockBuilder):

    def __init__(self, parentBuilder):
        context = _LocalContext(self, parentBuilder)
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
        code = CodeBlockSimplifier(self.constants, self.storages, self.nodes)

        # Check for reading of uninitialized variables.
        ununitializedLoads = []
        initializedVariables = set()
        for node in code.nodes:
            sid = node.sid
            storage = code.storages[sid]
            if isinstance(storage, Variable):
                if isinstance(node, Load):
                    if sid not in initializedVariables:
                        ununitializedLoads.append(node)
                elif isinstance(node, Store):
                    initializedVariables.add(sid)
        if ununitializedLoads:
            if log is not None:
                for load in ununitializedLoads:
                    log.error(
                        'variable "%s" is read before it is initialized'
                        % code.storages[load.sid].decl,
                        location=load.location
                        )
            raise ValueError(
                'Code block reads uninitialized variable(s): %s' % ', '.join(
                    code.storages[load.sid].decl
                    for load in ununitializedLoads
                    )
                )

        if 'ret' in self.context:
            if not any(
                    isinstance(storage, Variable) and storage.name == 'ret'
                    for storage in code.storages.values()
                    ):
                assert code.retRef is None, code.retRef
                code.retRef = self.context['ret']

        # Finalize code block.
        code.simplify()
        code.freeze()
        return code

    def _emitSingleLoad(self, sid, location):
        storage = self.storages[sid]
        if isinstance(storage, FixedValue):
            cid = storage.cid
        else:
            cid = len(self.constants)
            self.nodes.append(Load(cid, sid, location))
            self.constants.append(LoadedConstant(cid, sid))
        return ConstantValue(cid, maskForWidth(storage.width))

    def emitLoad(self, boundRef, location):
        terms = []
        offset = 0
        for sid, index, width in boundRef:
            value = self._emitSingleLoad(sid, location)
            sliced = optSlice(value, index, width)
            terms.append(sliced if offset == 0 else LShift(sliced, offset))
            offset += width
        return terms[0] if len(terms) == 1 else OrOperator(*terms)

    def _emitSingleStore(self, sid, expr, location):
        constant = self.emitCompute(expr)
        self.nodes.append(Store(constant.cid, sid, location))

    def emitStore(self, boundRef, value, location):
        offset = 0
        for sid, index, width in boundRef:
            valueSlice = optSlice(value, offset, width)
            storageWidth = self.storages[sid].width
            if index == 0 and width == storageWidth:
                # Full width: store only.
                combined = valueSlice
            else:
                # Partial width: combine with loaded old value.
                oldVal = self._emitSingleLoad(sid, location)
                storageMask = maskForWidth(storageWidth)
                valueMask = maskForWidth(width) << index
                maskLit = IntLiteral(storageMask & ~valueMask)
                combined = OrOperator(
                    AndOperator(oldVal, maskLit),
                    LShift(valueSlice, index)
                    )
            self._emitSingleStore(sid, combined, location)
            offset += width

    def emitValueArgument(self, name, decl, location):
        '''Adds a passed-by-value argument to this code block.
        The initial value is represented by an ArgumentConstant and is loaded
        into the corresponding Variable.
        Returns the BoundReference to the corresponding Variable.
        '''
        assert isinstance(decl, IntType), decl

        # Add ArgumentConstant.
        cid = len(self.constants)
        constant = ArgumentConstant(name, cid)
        self.constants.append(constant)

        # Store initial value.
        ref = self.emitVariable(name, decl, location)
        value = ConstantValue(cid, maskForWidth(decl.width))
        self.emitStore(ref, value, location)

        return ref

    def emitReferenceArgument(self, name, refType, location):
        return self._addNamedStorage(UnknownStorage(name, refType), location)

    def inlineFunctionCall(self, func, argMap, location):
        code = func.code
        if code is None:
            # Missing body, probably because of earlier errors.
            return None

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
        Returns a BoundReference containing the value returned by the inlined
        block, or None if the inlined block does not return anything.
        '''
        constants = self.constants
        storages = self.storages

        # Map old constant IDs to new IDs; don't copy yet.
        cidMap = dict(
            (oldCid, newCid)
            for newCid, oldCid in enumerate(
                code.constants.keys(), len(constants)
                )
            )

        # For each old sid, create a corresponding storage in this block.
        refMap = {}
        for sid, storage in code.storages.items():
            if isinstance(storage, UnknownStorage):
                ref = context[storage.name]
                assert storage.width == ref.width, (storage.width, ref.width)
            else:
                if isinstance(storage, FixedValue):
                    newStorage = FixedValue(cidMap[storage.cid], storage.width)
                else:
                    # Shallow copy because storages are immutable.
                    newStorage = storage
                newSid = len(storages)
                storages.append(newStorage)
                ref = BoundReference.single(newSid, newStorage.width)
            refMap[sid] = ref

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
        # This cannot be done when originally copying the storages
        # because at that time the constants haven't been added yet.
        for boundReference in refMap.values():
            for sid, index_, width_ in boundReference:
                storage = storages[sid]
                if isinstance(storage, IOStorage):
                    index = storage.index
                    storages[sid] = IOStorage(
                        storage.channel,
                        ConstantValue(cidMap[index.cid], index.mask)
                        )

        # Copy nodes.
        for node in code.nodes:
            boundReference = refMap[node.sid]
            newCid = cidMap[node.cid]
            if isinstance(node, Load):
                value = self.emitLoad(boundReference, node.location)
                constants[newCid] = ComputedConstant(newCid, value)
            elif isinstance(node, Store):
                value = ConstantValue(newCid, -1)
                self.emitStore(boundReference, value, node.location)
            else:
                assert False, node

        # Determine return value.
        retRef = code.retRef
        return None if retRef is None else BoundReference(chain.from_iterable(
            refMap[sid].slice(index, width)
            for sid, index, width in retRef
            ))
