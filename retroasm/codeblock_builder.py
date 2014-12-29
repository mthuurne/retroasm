from .codeblock import (
    ArgumentConstant, CodeBlock, ComputedConstant, ConstantValue, Load,
    LoadedConstant, Store
    )
from .codeblock_simplifier import CodeBlockSimplifier
from .expression import (
    Concatenation, IOReference, IntLiteral, LocalReference, LocalValue,
    NamedValue, Slice, Storage, Truncation, unit
    )
from .function import FunctionCall

class CodeBlockBuilder:

    def __init__(self):
        self.constants = []
        self.references = []
        self.nodes = []

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

    def createCodeBlock(self):
        '''Returns a CodeBlock object containing the items emitted so far.
        The state of the builder does not change.
        '''
        code = CodeBlockSimplifier(self.constants, self.references, self.nodes)
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
            return ConstantValue(constant)

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
        return ConstantValue(constant)

    def emitStore(self, rid, expr):
        '''Adds a node that stores a value in the referenced storage.
        '''
        constant = self.emitCompute(expr)
        self.nodes.append(Store(constant.cid, rid))

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

        cidMap = {}
        newCid = len(constants)
        for cid, const in code.constants.items():
            if isinstance(const, ArgumentConstant):
                cidMap[cid] = context[const.name].cid
            else:
                cidMap[cid] = newCid
                newCid += 1

        ridMap = {}
        for rid, ref in code.references.items():
            # Shallow copy is sufficient because references are immutable.
            ridMap[rid] = len(references)
            references.append(ref)

        for node in code.nodes:
            newCid = cidMap[node.cid]
            newRid = ridMap[node.rid]
            nodes.append(node.__class__(newCid, newRid))

        for cid, const in code.constants.items():
            if isinstance(const, ArgumentConstant):
                pass
            elif isinstance(const, ComputedConstant):
                def substCid(expr):
                    if isinstance(expr, ConstantValue):
                        return ConstantValue(constants[cidMap[expr.cid]])
                    else:
                        return None
                constants.append(ComputedConstant(
                    cidMap[const.cid],
                    const.expr.substitute(substCid)
                    ))
            elif isinstance(const, LoadedConstant):
                constants.append(LoadedConstant(
                    cidMap[const.cid],
                    ridMap[const.rid],
                    const.type
                    ))
            else:
                assert False, const

        for newRid in ridMap.values():
            ref = references[newRid]
            if isinstance(ref, IOReference):
                # Substitute index constants.
                # This cannot be done when originally copying the references
                # because at that time the constants haven't been added yet.
                references[newRid] = IOReference(
                    ref.channel,
                    ConstantValue(constants[cidMap[ref.index.cid]])
                    )

        if code.retCid is None:
            return unit
        else:
            return ConstantValue(constants[cidMap[code.retCid]])

def emitCodeFromAssignments(log, builder, assignments):
    '''Creates a code block from the given assignments.
    Returns a CodeBlock instance.
    '''

    nameToReference = {}
    def getReferenceID(storage):
        if isinstance(storage, IOReference):
            return builder.emitReference(storage)
        elif isinstance(storage, NamedValue):
            name = storage.name
            try:
                return nameToReference[name]
            except KeyError:
                rid = builder.emitReference(storage)
                nameToReference[name] = rid
                return rid
        else:
            assert False, storage

    def substituteReferences(expr):
        subst = substituteIOIndices(expr)
        if subst is not None:
            expr = subst
        # It seems the type inference ignores isistance() and deduces the wrong
        # types, leading to false positives.
        # pylint: disable=no-member
        if isinstance(expr, Storage):
            if isinstance(expr, NamedValue) and expr.name == 'ret':
                log.error('function return value "ret" is write-only')
            return builder.emitLoad(getReferenceID(expr))
        elif isinstance(expr, FunctionCall):
            func = expr.func
            argMap = {}
            for value, decl in expr.iterArgValuesAndDecl():
                if isinstance(decl, LocalValue):
                    argMap[decl.name] = builder.emitCompute(
                        value.substitute(substituteReferences)
                        )
                elif isinstance(decl, LocalReference):
                    log.error('reference arguments not implemented yet: '
                        '%s in %s' % (decl.name, func.name))
                else:
                    assert False, decl
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

    def decomposeConcat(storage, top=True):
        '''Iterates through the storage locations inside a concatenation.
        Each element is a pair of a Storage and an offset.
        '''
        if isinstance(storage, IntLiteral):
            # Assigning to a literal as part of a concatenation can be useful,
            # but assigning to only a literal is probably a mistake.
            if top:
                log.warning('assigning to literal has no effect')
        elif isinstance(storage, Storage):
            yield storage, 0
        elif isinstance(storage, Concatenation):
            for concatTerm, concatOffset in storage.iterWithOffset():
                for storage, offset in decomposeConcat(concatTerm, False):
                    yield storage, concatOffset + offset
        else:
            log.error(
                'cannot assign to an arithmetical expression: %s', storage
                )

    for lhs, rhs in assignments:
        # Substitute LoadedConstants for all references, such that we have
        # a side-effect free version of the right hand side expression.
        rhsLoadedRefs = rhs.substitute(substituteReferences)

        if lhs is None:
            if rhsLoadedRefs.type is not None:
                log.warning('result is ignored')
        else:
            # Constify the I/O indices to force emission of all loads before
            # we emit any stores.
            lhsConstIndices = lhs.substitute(substituteIOIndices)

            rhsConst = builder.emitCompute(rhsLoadedRefs)
            for storage, offset in decomposeConcat(lhsConstIndices):
                rid = getReferenceID(storage)
                builder.emitStore(rid, Slice(rhsConst, offset, storage.width))
