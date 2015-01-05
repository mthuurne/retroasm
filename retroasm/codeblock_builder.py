from .codeblock import (
    ArgumentConstant, ComputedConstant, ConstantValue, Load, LoadedConstant,
    Store
    )
from .codeblock_simplifier import CodeBlockSimplifier
from .expression import (
    Concatenation, IntLiteral, IntType, LShift, OrOperator, Slice, Truncation,
    unit
    )
from .function import FunctionCall
from .linereader import DelayedError
from .storage import (
    IOReference, LocalReference, NamedValue, Storage, ValueArgument, Variable,
    VariableDeclaration, checkStorage
    )

def decomposeConcat(storage):
    '''Iterates through the storage locations inside a concatenation.
    Each element is a pair of a Storage and an offset.
    '''
    if isinstance(storage, IntLiteral):
        pass
    elif isinstance(storage, Storage):
        yield storage, 0
    elif isinstance(storage, Concatenation):
        for concatTerm, concatOffset in storage.iterWithOffset():
            for storage, offset in decomposeConcat(concatTerm):
                yield storage, concatOffset + offset
    else:
        raise ValueError('non-storage expression: %s' % storage)

class CodeBlockBuilder:

    def __init__(self):
        self.constants = []
        self.references = []
        self.nodes = []
        self.nameToReference = {}

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

    def getReferenceID(self, storage):
        if isinstance(storage, NamedValue):
            name = storage.name
            try:
                return self.nameToReference[name]
            except KeyError:
                rid = self.emitReference(storage)
                self.nameToReference[name] = rid
                return rid
        else:
            return self.emitReference(storage)

    def createCodeBlock(self, reader=None, locations={}):
        '''Returns a CodeBlock object containing the items emitted so far.
        The state of the builder does not change.
        Raises ValueError if this builder does not represent a valid code block.
        If a reader is provided, errors are logged there, taking source
        location from the provided dictionary, if any.
        '''
        code = CodeBlockSimplifier(self.constants, self.references, self.nodes)

        # Check for reading of uninitialized variables.
        ununitializedLoads = []
        initializedVariables = set()
        for node in code.nodes:
            rid = node.rid
            ref = code.references[rid]
            if isinstance(ref, Variable) and not isinstance(ref, ValueArgument):
                if isinstance(node, Load):
                    if rid not in initializedVariables:
                        ununitializedLoads.append(node)
                elif isinstance(node, Store):
                    initializedVariables.add(rid)
        if ununitializedLoads:
            if reader is not None:
                for load in ununitializedLoads:
                    reader.error(
                        'variable "%s" is read before it is initialized'
                        % code.references[load.rid].formatDecl(),
                        location=locations.get(load.cid)
                        )
            raise ValueError(
                'Code block reads uninitialized variable(s): %s' % ', '.join(
                    code.references[load.rid].formatDecl()
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

    def emitAssignment(self, lhs, rhs):
        '''Adds a node that stores a value in the referenced storage.
        '''
        rhsConst = self.emitCompute(rhs)
        for storage, offset in decomposeConcat(lhs):
            self.emitStore(
                self.getReferenceID(storage),
                self.emitCompute(Slice(rhsConst, offset, storage.width))
                )

    def emitReference(self, storage):
        '''Adds a reference to the given storage, returning the reference ID.
        '''
        if not isinstance(storage, Storage):
            raise TypeError('expected Storage, got %s' % type(storage))
        if storage.width is None:
            raise ValueError('storages must have fixed width')
        if isinstance(storage, IOReference):
            if not isinstance(storage.index, ConstantValue):
                raise TypeError('I/O index must be ConstantValue')
        rid = len(self.references)
        self.references.append(storage)
        return rid

    def inlineBlock(self, code, context):
        '''Inlines another code block into this one.
        Returns an expression representing the return value of the inlined
        block.
        '''
        constants = self.constants
        references = self.references
        nodes = self.nodes

        # Map old constant IDs to new IDs; don't copy yet.
        cidMap = dict(
            (oldCid, newCid)
            for newCid, oldCid in enumerate(
                code.constants.keys(), len(constants)
                )
            )

        # Copy references.
        ridMap = {}
        for rid, ref in code.references.items():
            # Shallow copy is sufficient because references are immutable.
            if isinstance(ref, LocalReference):
                decomposed = []
                for storage, offset in decomposeConcat(context[ref.name]):
                    decomposed.append((len(references), offset))
                    references.append(storage)
                ridMap[rid] = decomposed
            else:
                ridMap[rid] = len(references)
                references.append(ref)

        # Copy constants.
        for cid, const in code.constants.items():
            assert cid == const.cid, const
            if isinstance(const, ArgumentConstant):
                value = context[const.name]
                declWidth = const.type.width
                if declWidth is not None and value.width != declWidth:
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
                rid = ridMap[const.rid]
                if isinstance(rid, list):
                    # Will be filled in when Load node is copied.
                    constants.append(None)
                else:
                    constants.append(
                        LoadedConstant(cidMap[const.cid], rid, const.type)
                        )
            else:
                assert False, const

        # Substitute index constants.
        # This cannot be done when originally copying the references
        # because at that time the constants haven't been added yet.
        for newRid in ridMap.values():
            if isinstance(newRid, list):
                rids = [rid for rid, offset in newRid]
            else:
                rids = [newRid]
            for rid in rids:
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
            newRid = ridMap[node.rid]
            if isinstance(newRid, list):
                if isinstance(node, Load):
                    consts = [
                        (self.emitLoad(rid), offset)
                        for rid, offset in newRid
                        ]
                    combined = OrOperator(*(LShift(*eo) for eo in consts))
                    newCid = cidMap[node.cid]
                    constants[newCid] = ComputedConstant(newCid, combined)
                elif isinstance(node, Store):
                    const = constants[cidMap[node.cid]]
                    value = ConstantValue(const.cid, const.type)
                    for rid, offset in newRid:
                        cid = self.emitCompute(
                            Slice(value, offset, references[rid].width)
                            ).cid
                        nodes.append(Store(cid, rid))
                else:
                    assert False, node
            else:
                newCid = cidMap[node.cid]
                nodes.append(node.__class__(newCid, newRid))

        # Determine return value.
        retCid = code.retCid
        if retCid is None:
            return unit
        else:
            retCid = cidMap[retCid]
            return ConstantValue(retCid, constants[retCid].type)

def emitCodeFromAssignments(reader, builder, assignments):
    '''Creates a code block from the given assignments.
    Returns a dictionary that maps each constant ID of a LoadedConstant to the
    reader location that constant was loaded at.
    '''

    locations = {}

    def emitLoad(rid):
        expr = builder.emitLoad(rid)
        locations[expr.cid] = reader.getLocation()
        return expr

    def substituteReferences(expr):
        subst = substituteIOIndices(expr)
        if subst is not None:
            expr = subst
        # It seems the type inference ignores isistance() and deduces the wrong
        # types, leading to false positives.
        # pylint: disable=no-member
        if isinstance(expr, Storage):
            if isinstance(expr, NamedValue) and expr.name == 'ret':
                reader.error('function return value "ret" is write-only')
            return emitLoad(builder.getReferenceID(expr))
        elif isinstance(expr, FunctionCall):
            func = expr.func
            argMap = {}
            try:
                with reader.checkErrors():
                    for name, decl, value in expr.iterArgNameDeclValue():
                        if isinstance(decl, IntType):
                            argMap[name] = builder.emitCompute(
                                value.substitute(substituteReferences)
                                )
                        elif isinstance(decl, LocalReference):
                            if checkStorage(value):
                                argMap[name] = value
                            else:
                                reader.error(
                                    'non-storage argument "%s" for reference '
                                    'argument "%s"' % (value, decl.formatDecl())
                                    )
                        else:
                            assert False, decl
            except DelayedError:
                return None
            else:
                return builder.inlineBlock(func.code, argMap)
        else:
            return None

    def substituteIOIndices(expr):
        if isinstance(expr, IOReference):
            index = builder.emitCompute(Truncation(
                expr.index.substitute(substituteReferences),
                expr.channel.addrType.width
                ))
            return IOReference(expr.channel, index)
        else:
            return None

    for lhs, rhs in assignments:
        if lhs is None and rhs.type is not None:
            if isinstance(rhs, VariableDeclaration):
                continue
            else:
                reader.warning('result is ignored')
        elif isinstance(lhs, IntLiteral):
            # Assigning to a literal as part of a concatenation can be useful,
            # but assigning to only a literal is probably a mistake.
            reader.warning('assigning to literal has no effect')

        # Substitute LoadedConstants for all references, such that we have
        # a side-effect free version of the right hand side expression.
        rhsLoadedRefs = rhs.substitute(substituteReferences)

        if lhs is not None:
            # Constify the I/O indices to force emission of all loads before
            # we emit any stores.
            lhsConstIndices = lhs.substitute(substituteIOIndices)
            try:
                builder.emitAssignment(lhsConstIndices, rhsLoadedRefs)
            except ValueError as ex:
                reader.error('error on left hand side of assignment: %s', ex)

    return locations

def createCodeBlockFromAssignments(reader, assignments):
    builder = CodeBlockBuilder()
    try:
        with reader.checkErrors():
            locations = emitCodeFromAssignments(reader, builder, assignments)
    except DelayedError:
        return None
    try:
        return builder.createCodeBlock(reader, locations)
    except ValueError:
        return None
