from .codeblock import (
    ArgumentConstant, ComputedConstant, ConstantValue, Load, LoadedConstant,
    Store
    )
from .codeblock_simplifier import CodeBlockSimplifier
from .expression import (
    Concatenation, IntLiteral, LShift, OrOperator, Slice, Truncation, unit
    )
from .expression_builder import BadExpression, createExpression
from .expression_parser import AssignmentNode, IdentifierNode
from .function import FunctionCall
from .storage import (
    IOReference, LocalReference, NamedValue, ReferencedValue, Storage, Variable,
    checkStorage
    )
from .types import IntType, Reference, unlimited

def decomposeConcat(storage):
    '''Iterates through the storage locations inside a concatenation.
    Each element is a pair of a Storage and an offset.
    '''
    if isinstance(storage, IntLiteral):
        pass
    elif isinstance(storage, (IOReference, ReferencedValue)):
        yield storage, 0
    elif isinstance(storage, Concatenation):
        for concatTerm, concatOffset in storage.iterWithOffset():
            for storage, offset in decomposeConcat(concatTerm):
                yield storage, concatOffset + offset
    else:
        raise ValueError('non-storage expression: %s' % storage)

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
            return self._importReference(self.globalContext[key])

    def _importReference(self, globalRef):
        if isinstance(globalRef, NamedValue):
            # While the initial call to this method happens when the name is
            # not found, there could be name matches on recursive calls.
            try:
                return self.localContext[globalRef.name]
            except KeyError:
                return self._addNamedReference(globalRef)
        elif isinstance(globalRef, Concatenation):
            return Concatenation(*(
                self._importReference(expr)
                for expr in globalRef.exprs
                ))
        else:
            return globalRef

    def _addNamedReference(self, ref):
        name = ref.name
        if name in self.localContext:
            raise ValueError('attempt to redefine "%s"' % name)
        # pylint: disable=protected-access
        rid = self.builder._emitReference(ref)
        refVal = ReferencedValue(rid, ref.type)
        self.localContext[name] = refVal
        return refVal

    def addVariable(self, name, typ):
        return self._addNamedReference(Variable(name, typ))

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
                        % code.references[load.rid].formatDecl(),
                        location=self.loadLocations[load.cid]
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

    def emitAssignment(self, lhs, rhs):
        '''Adds a node that stores a value in the referenced storage.
        '''
        rhsConst = self.emitCompute(rhs)
        for storage, offset in decomposeConcat(lhs):
            if isinstance(storage, ReferencedValue):
                rid = storage.rid
            elif isinstance(storage, IOReference):
                rid = self._emitReference(storage)
            else:
                assert False, storage
            self.emitStore(
                rid,
                self.emitCompute(Slice(rhsConst, offset, storage.width))
                )

    def _emitReference(self, storage):
        '''Adds a reference to the given storage, returning the reference ID.
        '''
        if not isinstance(storage, Storage):
            raise TypeError('expected Storage, got %s' % type(storage))
        if storage.width is unlimited:
            raise ValueError('storages must have fixed width')
        if isinstance(storage, IOReference):
            if not isinstance(storage.index, ConstantValue):
                raise TypeError('I/O index must be ConstantValue')
        rid = len(self.references)
        self.references.append(storage)
        return rid

    def _addNamedReference(self, ref):
        # pylint: disable=protected-access
        return self.context._addNamedReference(ref).rid

    def emitVariable(self, name, refType):
        return self._addNamedReference(Variable(name, refType))

    def emitLocalReference(self, name, refType):
        return self._addNamedReference(LocalReference(name, refType))

    def emitValueArgument(self, name, decl):
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
        rid = self.emitVariable(name, decl)
        self.nodes.insert(0, Store(cid, rid))

        return rid

    def emitIOReference(self, channel, index):
        indexConst = self.emitCompute(index)
        ioref = IOReference(channel, indexConst)
        return self._emitReference(ioref)

    def _handleError(self, msg):
        if self.reader is None:
            raise ValueError(msg)
        else:
            self.reader.error(msg)

    def constifyReferences(self, expr):
        '''Returns the given expression, with all reads of references replaced
        by loaded constants.
        '''
        subst = self.constifyIOIndices(expr)
        if subst is not None:
            expr = subst
        # pylint: disable=no-member
        # It seems the type inference ignores isinstance() and deduces the wrong
        # types, leading to false positives.
        if isinstance(expr, ReferencedValue):
            rid = expr.rid
            ref = self.references[rid]
            if isinstance(ref, Variable) and ref.name == 'ret':
                self._handleError('function return value "ret" is write-only')
            return self.emitLoad(rid)
        elif isinstance(expr, IOReference):
            return self.emitLoad(self._emitReference(expr))
        elif isinstance(expr, FunctionCall):
            func = expr.func
            argMap = {}
            errors = False
            for name, decl, value in expr.iterArgNameDeclValue():
                if isinstance(decl, IntType):
                    argMap[name] = self.emitCompute(
                        value.substitute(self.constifyReferences)
                        )
                elif isinstance(decl, Reference):
                    if checkStorage(value):
                        argMap[name] = value
                    else:
                        errors = True
                        self._handleError(
                            'non-storage argument "%s" for reference '
                            'argument "%s %s"' % (value, decl, name)
                            )
                else:
                    assert False, decl
            if errors:
                return None
            else:
                code = func.code
                if code is None:
                    # Missing body, probably because of earlier errors.
                    return None
                else:
                    return self.inlineBlock(code, argMap)
        else:
            return None

    def constifyIOIndices(self, expr):
        '''Returns the given expression, with all I/O indices replaced by
        computed constants.
        '''
        if isinstance(expr, IOReference):
            index = self.emitCompute(Truncation(
                expr.index.substitute(self.constifyReferences),
                expr.channel.addrType.width
                ))
            return IOReference(expr.channel, index)
        else:
            return None

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
                    if isinstance(storage, ReferencedValue):
                        newRid = storage.rid
                    elif isinstance(storage, IOReference):
                        newRid = len(references)
                        references.append(storage)
                    else:
                        assert False, storage
                    decomposed.append((newRid, offset))
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

def emitCodeFromStatements(reader, builder, statements):
    '''Creates a code block from the given statements.
    Returns a dictionary that maps each constant ID of a LoadedConstant to the
    reader location that constant was loaded at.
    '''

    context = builder.context
    for tree in statements:
        if isinstance(tree, AssignmentNode):
            try:
                lhs = createExpression(tree.lhs, context)
            except BadExpression as ex:
                reader.error(
                    'bad expression on left hand side of assignment: %s', ex,
                    location=ex.location
                    )
                continue
            if isinstance(lhs, IntLiteral):
                # Assigning to a literal inside a concatenation can be useful,
                # but assigning to only a literal is probably a mistake.
                reader.warning('assigning to literal has no effect')
            try:
                rhs = createExpression(tree.rhs, context)
            except BadExpression as ex:
                reader.error(
                    'bad expression on right hand side of assignment: %s', ex,
                    location=ex.location
                    )
                continue
        elif isinstance(tree, IdentifierNode) and tree.decl is not None:
            # Variable declaration.
            try:
                createExpression(tree, context)
            except BadExpression as ex:
                reader.error(str(ex), location=ex.location)
            # Don't evaluate the declared variable, since that would be counted
            # as a read from an uninitialized storage.
            continue
        else:
            lhs = None
            try:
                rhs = createExpression(tree, context)
            except BadExpression as ex:
                reader.error(
                    'bad expression in statement: %s', ex,
                    location=ex.location
                    )
                continue
            if rhs.type is not None:
                reader.warning('result is ignored')

        # Substitute LoadedConstants for all references, such that we have
        # a side-effect free version of the right hand side expression.
        rhsLoadedRefs = rhs.substitute(builder.constifyReferences)

        if lhs is not None:
            # Constify the I/O indices to force emission of all loads before
            # we emit any stores.
            lhsConstIndices = lhs.substitute(builder.constifyIOIndices)
            try:
                builder.emitAssignment(lhsConstIndices, rhsLoadedRefs)
            except ValueError as ex:
                reader.error('error on left hand side of assignment: %s', ex)
