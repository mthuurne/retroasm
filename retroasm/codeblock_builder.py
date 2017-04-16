from .codeblock import (
    ArgumentConstant, ComputedConstant, ConstantValue, FixedValue, Load,
    LoadedConstant, Reference, SingleReference, Store
    )
from .codeblock_simplifier import CodeBlockSimplifier
from .context import Context
from .expression import optSlice
from .function import Function
from .linereader import BadInput
from .storage import IOChannel, IOStorage, RefArgStorage, Storage, Variable
from .types import IntType, maskForWidth
from .utils import checkType

class CodeBlockBuilder:
    _scope = property()

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
            print('        S%-2d : %s  (%s-bit)' % (sid, storage, storage.width))

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
        ref = SingleReference(self, sid, storage.type)
        self.context.define(storage.name, ref, location)
        return ref

    def emitVariable(self, name, refType, location):
        var = Variable(name, refType, self._scope)
        return self._addNamedStorage(var, location)

    def emitIOReference(self, channel, index):
        addrWidth = channel.addrType.width
        indexConst = self.emitCompute(optSlice(index, 0, addrWidth))
        sid = self._addStorage(IOStorage(channel, indexConst))
        return SingleReference(self, sid, channel.elemType)

    def emitFixedValue(self, expr, typ):
        '''Emits a constant representing the result of the given expression.
        Returns a FixedValue reference to the constant value.
        '''
        const = self.emitCompute(expr)
        return FixedValue(self, const.cid, typ)

    def defineReference(self, name, value, location):
        '''Defines a reference with the given name and value.
        Returns the given value.
        Raises NameExistsError if the name is already taken.
        '''
        checkType(value, Reference, 'value')
        self.context.define(name, value, location)
        return value

    def emitLoadBits(self, sid, location):
        '''Loads the value from the storage with the given ID by emitting
        a Load node on this builder.
        Returns an expression that represents the loaded value.
        '''
        raise NotImplementedError

    def emitStoreBits(self, sid, value, location):
        '''Stores the value of the given expression in the storage with the
        given ID by emitting a Store node on this builder.
        '''
        raise NotImplementedError

    def inlineFunctionCall(self, func, argMap, location):
        '''Inlines a call to the given function with the given arguments.
        Returns a Reference containing the value returned by the inlined
        function, or None if the function does not return anything.
        '''
        raise NotImplementedError

class BadGlobalOperation(BadInput):
    '''Raised when an operation is attempted that is not allowed in the global
    scope.
    '''

class GlobalCodeBlockBuilder(CodeBlockBuilder):
    _scope = 0

    def __init__(self):
        CodeBlockBuilder.__init__(self, Context())

    def emitLoadBits(self, sid, location):
        raise BadGlobalOperation(
            'attempt to read from storage in global scope: %s'
            % self.storages[sid],
            location
            )

    def emitStoreBits(self, sid, value, location):
        raise BadGlobalOperation(
            'attempt to write to storage in global scope: %s'
            % self.storages[sid],
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
            elif isinstance(value, Reference):
                value = value.clone(self._importSingleRef)
            else:
                assert False, (key, repr(value))
            self.elements[key] = value
            self.locations[key] = None
            return value

    def _importSingleRef(self, parentRef):
        '''Imports the given parent storage ID into the local context.
        Returns a reference to the local storage.
        '''
        parentSid = parentRef.sid
        importMap = self.importMap
        try:
            return importMap[parentSid]
        except KeyError:
            storage = self.parentBuilder.storages[parentSid]
            localBuilder = self.localBuilder
            # pylint: disable=protected-access
            localSid = localBuilder._addStorage(storage)
            localRef = SingleReference(localBuilder, localSid, parentRef.type)
            importMap[parentSid] = localRef
            return localRef

class LocalCodeBlockBuilder(CodeBlockBuilder):
    _scope = 1

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
            if isinstance(storage, Variable) and storage.scope == 1:
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
                    isinstance(storage, Variable) and storage.scope == 1
                    and storage.name == 'ret'
                    for storage in code.storages.values()
                    ):
                assert code.retRef is None, code.retRef
                code.retRef = self.context['ret']

        # Finalize code block.
        code.simplify()
        code.freeze()
        return code

    def emitValueArgument(self, name, decl, location):
        '''Adds a passed-by-value argument to this code block.
        The initial value is represented by an ArgumentConstant and is loaded
        into the corresponding Variable.
        Returns the Reference to the corresponding Variable.
        '''
        assert isinstance(decl, IntType), decl

        # Add ArgumentConstant.
        cid = len(self.constants)
        constant = ArgumentConstant(name, cid)
        self.constants.append(constant)

        # Add Variable.
        ref = self.emitVariable(name, decl, location)

        # Store initial value.
        value = ConstantValue(cid, maskForWidth(decl.width))
        ref.emitStore(value, location)

        return ref

    def emitReferenceArgument(self, name, refType, location):
        return self._addNamedStorage(RefArgStorage(name, refType), location)

    def emitLoadBits(self, sid, location):
        storage = self.storages[sid]
        cid = len(self.constants)
        self.nodes.append(Load(cid, sid, location))
        self.constants.append(LoadedConstant(cid, sid))
        return ConstantValue(cid, maskForWidth(storage.width))

    def emitStoreBits(self, sid, value, location):
        constant = self.emitCompute(value)
        self.nodes.append(Store(constant.cid, sid, location))

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

        return self.inlineBlock(code, argMap)

    def inlineBlock(self, code, context):
        '''Inlines another code block into this one.
        Returns a Reference containing the value returned by the inlined
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

        # For each old SID, create a corresponding storage in this block.
        refMap = {}
        for sid, storage in code.storages.items():
            if isinstance(storage, RefArgStorage):
                ref = context[storage.name]
                assert storage.width == ref.width, (storage.width, ref.width)
            else:
                if isinstance(storage, IOStorage):
                    index = storage.index
                    newStorage = IOStorage(
                        storage.channel,
                        ConstantValue(cidMap[index.cid], index.mask)
                        )
                else:
                    # Shallow copy because storages are immutable.
                    newStorage = storage
                newSid = len(storages)
                storages.append(newStorage)
                # Note: It doesn't matter if the original reference for this
                #       storage was signed, since the sign extension will
                #       have been copied as part of a ComputedConstant.
                #       We are copying the _emitLoadBits() output here,
                #       not the emitLoad() output.
                ref = SingleReference(self, newSid, IntType.u(newStorage.width))
            refMap[sid] = ref

        # Copy nodes.
        for node in code.nodes:
            ref = refMap[node.sid]
            newCid = cidMap[node.cid]
            if isinstance(node, Load):
                value = ref.emitLoad(node.location)
                constants[newCid] = ComputedConstant(newCid, value)
            elif isinstance(node, Store):
                value = ConstantValue(newCid, -1)
                ref.emitStore(value, node.location)
            else:
                assert False, node

        # Determine return value.
        retRef = code.retRef
        if retRef is None:
            return None
        elif isinstance(retRef, FixedValue):
            return FixedValue(self, cidMap[retRef.cid], retRef.type)
        else:
            return retRef.clone(lambda ref: refMap[ref.sid])
