from .codeblock import (
    ArgumentValue, ComputedConstant, ConstantValue, FixedValue, Load,
    LoadedConstant, Reference, SingleReference, Store
    )
from .codeblock_simplifier import CodeBlockSimplifier
from .expression import optSlice
from .linereader import BadInput
from .namespace import LocalNamespace
from .storage import IOStorage, RefArgStorage, Storage, Variable
from .types import IntType, maskForWidth
from .utils import checkType

class CodeBlockBuilder:
    _scope = property()

    def __init__(self, namespace):
        self.constants = []
        self.storages = []
        self.namespace = namespace

    def dump(self):
        '''Prints the current state of this code block builder on stdout.
        '''
        print('    constants:')
        for const in self.constants:
            if isinstance(const, ComputedConstant):
                print('        C%-2d =  %s' % (const.cid, const.expr))
            elif isinstance(const, LoadedConstant):
                print('        C%-2d <- %s' % (const.cid, const.storage))
            else:
                assert False, const
        print('    storages:')
        for sid, storage in enumerate(self.storages):
            print('        S%-2d : %s  (%s-bit)' % (sid, storage, storage.width))
        if 'ret' in self.namespace:
            retRef = self.namespace['ret']
            storage = self.storages[retRef.sid]
            if not (isinstance(storage, Variable) and storage.name == 'ret'):
                print('        ret = %s' % retRef)

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
        checkType(storage, Storage, 'storage')
        sid = len(self.storages)
        self.storages.append(storage)
        return sid

    def _addNamedStorage(self, storage, location):
        sid = self._addStorage(storage)
        ref = SingleReference(self, sid, storage.type)
        self.namespace.define(storage.name, ref, location)
        return ref

    def emitVariable(self, name, refType, location):
        var = Variable(name, refType, self._scope)
        return self._addNamedStorage(var, location)

    def emitIOReference(self, channel, index):
        addrWidth = channel.addrType.width
        truncatedIndex = optSlice(index, 0, addrWidth)
        sid = self._addStorage(IOStorage(channel, truncatedIndex))
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
        self.namespace.define(name, value, location)
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

class IllegalStateAccess(BadInput):
    '''Raised when an operation is attempted that reads or writes state
    in a situation where that is not allowed.
    '''

class StatelessCodeBlockBuilderMixin:
    '''A CodeBlockBuilder can inherit this to raise IllegalStateAccess when its
    users attempt touch any state, such as performing register access or I/O.
    '''

    def emitLoadBits(self, sid, location):
        raise IllegalStateAccess(
            'attempt to read state: %s' % self.storages[sid],
            location
            )

    def emitStoreBits(self, sid, value, location):
        raise IllegalStateAccess(
            'attempt to write state: %s' % self.storages[sid],
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

class EncodingCodeBlockBuilder(
        StatelessCodeBlockBuilderMixin, CodeBlockBuilder
        ):
    _scope = 0

    def __init__(self, parentBuilder):
        namespace = LocalNamespace(self, parentBuilder)
        CodeBlockBuilder.__init__(self, namespace)

class LocalCodeBlockBuilder(CodeBlockBuilder):
    _scope = 1

    def __init__(self, parentBuilder):
        namespace = LocalNamespace(self, parentBuilder)
        CodeBlockBuilder.__init__(self, namespace)
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
                        'variable "%s" is read before it is initialized'
                        % load.storage.decl,
                        location=load.location
                        )
            raise ValueError(
                'Code block reads uninitialized variable(s): %s' % ', '.join(
                    load.storage.decl
                    for load in ununitializedLoads
                    )
                )

        if 'ret' in self.namespace:
            assert code.retRef is None, code.retRef
            # Note that at this point, the code block still uses the exact same
            # SIDs and CIDs as we do, so we only have to replace the block
            # objects with in SingleReferences and FixedValues.
            code.retRef = self.namespace['ret'].clone(
                lambda ref, code=code: SingleReference(code, ref.sid, ref.type),
                lambda ref, code=code: FixedValue(code, ref.cid, ref.type)
                )

        # Finalize code block.
        code.simplify()
        code.freeze()
        return code

    def emitValueArgument(self, name, decl, location):
        '''Adds a passed-by-value argument to this code block.
        The initial value is represented by an ArgumentValue and is stored
        into the corresponding Variable.
        Returns the Reference to the corresponding Variable.
        '''
        assert isinstance(decl, IntType), decl
        value = ArgumentValue(name, maskForWidth(decl.width))

        # Add constant.
        cid = len(self.constants)
        constant = ComputedConstant(cid, value)
        self.constants.append(constant)

        # Add Variable.
        ref = self.emitVariable(name, decl, location)

        # Store initial value.
        ref.emitStore(value, location)

        return ref

    def emitReferenceArgument(self, name, refType, location):
        return self._addNamedStorage(RefArgStorage(name, refType), location)

    def emitLoadBits(self, sid, location):
        storage = self.storages[sid]
        cid = len(self.constants)
        self.constants.append(LoadedConstant(cid, storage))
        expr = ConstantValue(cid, maskForWidth(storage.width))
        self.nodes.append(Load(expr, storage, location))
        return expr

    def emitStoreBits(self, sid, value, location):
        storage = self.storages[sid]
        expr = self.emitCompute(value)
        self.nodes.append(Store(expr, storage, location))

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

    def inlineBlock(self, code, namespace):
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

        def substCid(expr):
            if isinstance(expr, ArgumentValue):
                return namespace[expr.name]
            elif isinstance(expr, ConstantValue):
                return ConstantValue(cidMap[expr.cid], expr.mask)
            else:
                return None

        # Copy constants.
        for cid, const in code.constants.items():
            assert cid == const.cid, const
            if isinstance(const, ComputedConstant):
                constants.append(ComputedConstant(
                    cidMap[const.cid],
                    const.expr.substitute(substCid)
                    ))
            elif isinstance(const, LoadedConstant):
                # Will be filled in when Load node is copied.
                constants.append(None)
            else:
                assert False, const

        # For each old storage, create a corresponding storage in this block.
        refMap = {}
        for storage in code.storages.values():
            if isinstance(storage, RefArgStorage):
                ref = namespace[storage.name]
                assert storage.width == ref.width, (storage.width, ref.width)
            else:
                if isinstance(storage, IOStorage):
                    newStorage = IOStorage(
                        storage.channel,
                        storage.index.substitute(substCid)
                        )
                elif isinstance(storage, Variable) and storage.scope == 1 \
                        and storage.name == 'ret':
                    newStorage = Variable('inlined_ret', storage.type, 1)
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
            refMap[storage] = ref

        # Copy nodes.
        for node in code.nodes:
            ref = refMap[node.storage]
            if isinstance(node, Load):
                value = ref.emitLoad(node.location)
                newCid = cidMap[node.expr.cid]
                constants[newCid] = ComputedConstant(newCid, value)
            elif isinstance(node, Store):
                value = node.expr.substitute(substCid)
                ref.emitStore(value, node.location)
            else:
                assert False, node

        # Determine return value.
        retRef = code.retRef
        if retRef is None:
            return None
        else:
            return retRef.clone(
                lambda ref: refMap[ref.storage],
                lambda ref: FixedValue(self, cidMap[ref.cid], ref.type)
                )
