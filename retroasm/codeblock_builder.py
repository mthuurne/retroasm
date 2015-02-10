from .codeblock import (
    ArgumentConstant, ComputedConstant, ConstantValue, Load, LoadedConstant,
    Store
    )
from .codeblock_simplifier import CodeBlockSimplifier
from .context import NameExistsError
from .expression import Truncation, unit
from .function import Function
from .storage import (
    ComposedStorage, Concatenation, FixedValue, IOChannel, IOReference,
    LocalReference, NamedStorage, Storage, Variable
    )
from .types import IntType, unlimited
from .utils import checkType

class _CodeBlockContext:
    '''A cache for local references and on-demand imported global references.
    Its goal is to avoid a lot of redundant references when a block is first
    created: although the simplifier can remove those, that is a pretty
    inefficient operation which should be applied to non-trivial cases only.
    '''

    def __init__(self, builder, globalContext):
        self.builder = builder
        self.globalContext = globalContext
        self.localContext = {}

    def __contains__(self, key):
        return key in self.localContext or key in self.globalContext

    def __getitem__(self, key):
        try:
            return self.localContext[key]
        except KeyError:
            ref = self.globalContext[key]
            if isinstance(ref, (Function, IOChannel)):
                return ref
            elif isinstance(ref, Concatenation):
                # Aliases are converted to Slices-in-Concatenation.
                return ComposedStorage((
                    (self._importReference(sl.expr), sl.index, sl.width)
                    for sl in ref.exprs
                    ))
            else:
                # Plain register.
                rid = self._importReference(ref)
                return ComposedStorage.single(rid, ref.width)

    def define(self, name, value, location):
        localContext = self.localContext
        if name in localContext:
            raise NameExistsError('attempt to redefine "%s"' % name, location)
        localContext[name] = value

    def _importReference(self, ref):
        '''Imports the given named reference into the local context.
        Returns the local reference ID.
        '''
        # Note: At the moment only named storages are allowed in the global
        #       context.
        assert isinstance(ref, NamedStorage), repr(ref)
        name = ref.name
        try:
            composed = self.localContext[name]
        except KeyError:
            # pylint: disable=protected-access
            location = self.globalContext.locations[name]
            rid = self.builder._addNamedReference(ref, location)
        else:
            (rid, index_, width_), = composed
        return rid

class CodeBlockBuilder:

    def __init__(self, globalContext={}, reader=None):
        self.constants = []
        self.references = []
        self.nodes = []
        self.context = _CodeBlockContext(self, globalContext)
        self.reader = reader
        self.loadLocations = {}

    def dump(self):
        '''Prints the current state of this code block builder on stdout.
        '''
        print('    constants:')
        for const in self.constants:
            if isinstance(const, ComputedConstant):
                print('        %-4s C%-2d =  %s' % (
                    const.type, const.cid, const.expr
                    ))
            elif isinstance(const, LoadedConstant):
                print('        %-4s C%-2d <- R%d' % (
                    const.type, const.cid, const.rid
                    ))
            elif isinstance(const, ArgumentConstant):
                print('        %-4s C%-2d :  %s' % (
                    const.type, const.cid, const.name
                    ))
            else:
                assert False, const
        print('    references:')
        for rid, ref in enumerate(self.references):
            print('        %-4s R%-2d = %s' % ('%s&' % ref.type, rid, ref))
        print('    nodes:')
        for node in self.nodes:
            print('        %s' % node)

    def getLocation(self):
        return None if self.reader is None else self.reader.getLocation()

    def createCodeBlock(self):
        '''Returns a CodeBlock object containing the items emitted so far.
        The state of the builder does not change.
        Raises ValueError if this builder does not represent a valid code block.
        If a reader was provided to the constructor, errors are logged as well.
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
            log = self.reader
            if log is not None:
                for load in ununitializedLoads:
                    log.error(
                        'variable "%s" is read before it is initialized'
                        % code.references[load.rid].decl,
                        location=self.loadLocations[load.cid]
                        )
            raise ValueError(
                'Code block reads uninitialized variable(s): %s' % ', '.join(
                    code.references[load.rid].decl
                    for load in ununitializedLoads
                    )
                )

        # Finalize code block.
        code.simplify()
        code.freeze()
        return code

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
            return ConstantValue(cid, expr.type)

    def emitLoad(self, rid):
        '''Adds a node that loads a value from the referenced storage.
        Returns an expression that wraps the loaded value.
        '''
        cid = len(self.constants)
        self.loadLocations[cid] = self.getLocation()
        load = Load(cid, rid)
        self.nodes.append(load)
        refType = self.references[rid].type
        constant = LoadedConstant(cid, rid, refType)
        self.constants.append(constant)
        return ConstantValue(cid, refType)

    def emitStore(self, rid, expr):
        '''Adds a node that stores a value in the referenced storage.
        '''
        constant = self.emitCompute(expr)
        self.nodes.append(Store(constant.cid, rid))

    def _emitReference(self, storage):
        '''Adds a reference to the given storage, returning the reference ID.
        '''
        if not isinstance(storage, Storage):
            raise TypeError('expected Storage, got %s' % type(storage).__name__)
        if storage.width is unlimited:
            raise ValueError('storages must have fixed width')
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

    def emitValueArgument(self, name, decl, location):
        '''Adds a passed-by-value argument to this code block.
        The initial value is represented by an ArgumentConstant and is loaded
        into the corresponding Variable.
        Returns the reference ID of the corresponding Variable.
        '''
        assert isinstance(decl, IntType), decl

        # Add ArgumentConstant.
        cid = len(self.constants)
        constant = ArgumentConstant(name, cid, decl)
        self.constants.append(constant)

        # Store initial value.
        rid = self.emitVariable(name, decl, location)
        self.nodes.insert(0, Store(cid, rid))

        return rid

    def emitIOReference(self, channel, index):
        indexConst = self.emitCompute(Truncation(index, channel.addrType.width))
        return self._emitReference(IOReference(channel, indexConst))

    def emitFixedValue(self, expr):
        '''Emits a constant representing the result of the given expression.
        Returns the reference ID of the corresponding FixedValue.
        '''
        const = self.emitCompute(expr)
        return self._emitReference(FixedValue(const.cid, const.type))

    def defineConstant(self, name, expr, location):
        '''Defines a constant with the given name and value.
        Returns a ConstantValue for the newly defined constant.
        Raises NameExistsError if the name is already taken.
        '''
        const = self.emitCompute(expr)
        self.context.define(name, const, location)
        return const

    def defineReference(self, name, value, location):
        '''Defines a reference with the given name and value.
        Returns the given value.
        Raises NameExistsError if the name is already taken.
        '''
        checkType(value, ComposedStorage, 'value')
        self.context.define(name, value, location)
        return value

    def inlineBlock(self, code, context):
        '''Inlines another code block into this one.
        Returns an expression representing the return value of the inlined
        block.
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
        ridMap = {}
        for rid, ref in code.references.items():
            if isinstance(ref, LocalReference):
                storage = context[ref.name]
                assert ref.type == storage.type, (ref.type, storage.type)
            else:
                # Shallow copy is sufficient because references are immutable.
                newRid = len(references)
                references.append(ref)
                storage = ComposedStorage.single(newRid, ref.width)
            ridMap[rid] = storage

        # Copy constants.
        for cid, const in code.constants.items():
            assert cid == const.cid, const
            if isinstance(const, ArgumentConstant):
                value = context[const.name]
                declWidth = const.type.width
                if value.width > declWidth:
                    value = Truncation(value, declWidth)
                constants.append(ComputedConstant(cidMap[const.cid], value))
            elif isinstance(const, ComputedConstant):
                def substCid(expr):
                    if isinstance(expr, ConstantValue):
                        return ConstantValue(cidMap[expr.cid], expr.type)
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
        for composedStorage in ridMap.values():
            for rid, index_, width_ in composedStorage:
                ref = references[rid]
                if isinstance(ref, IOReference):
                    references[rid] = IOReference(
                        ref.channel,
                        ConstantValue(
                            cidMap[ref.index.cid], ref.channel.addrType
                            )
                        )

        # Copy nodes.
        for node in code.nodes:
            composedStorage = ridMap[node.rid]
            if isinstance(node, Load):
                newCid = cidMap[node.cid]
                value = composedStorage.emitLoad(self)
                constants[newCid] = ComputedConstant(newCid, value)
            elif isinstance(node, Store):
                const = constants[cidMap[node.cid]]
                value = ConstantValue(const.cid, const.type)
                composedStorage.emitStore(self, value)
            else:
                assert False, node

        # Determine return value.
        retCid = code.retCid
        if retCid is None:
            return unit
        else:
            retCid = cidMap[retCid]
            return ConstantValue(retCid, constants[retCid].type)
