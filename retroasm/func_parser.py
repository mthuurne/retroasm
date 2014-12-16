from .expression import (
    Concatenation, Expression, IOReference, IntLiteral, IntType, LocalReference,
    NamedValue, Register, Slice, Storage
    )

from inspect import signature
from itertools import count

class Constant:
    '''Definition of a local constant value.
    '''
    __slots__ = ('_cid', '_type')

    cid = property(lambda self: self._cid)
    type = property(lambda self: self._type)

    def __init__(self, cid, intType):
        if not isinstance(cid, int):
            raise TypeError('constant ID must be int, got %s' % type(cid))
        if not isinstance(intType, IntType):
            raise TypeError('type must be IntType, got %s' % type(intType))
        self._cid = cid
        self._type = intType

    def __repr__(self):
        return 'Constant(%d, %s)' % (self._cid, repr(self._type))

    def __str__(self):
        return '%s C%d' % (self._type, self._cid)

class ComputedConstant(Constant):
    '''A constant defined by evaluating an expression.
    '''
    __slots__ = ('_expr',)

    expr = property(lambda self: self._expr)

    def __init__(self, cid, expr):
        self._expr = Expression.checkInstance(expr)
        Constant.__init__(self, cid, expr.type)

    def __repr__(self):
        return 'ComputedConstant(%d, %s)' % (self._cid, repr(self._expr))

    def __str__(self):
        return '%s = %s' % (super().__str__(), self._expr)

class LoadedConstant(Constant):
    '''A constant defined by loading a value from a storage location.
    '''
    __slots__ = ('_load',)

    load = property(lambda self: self._load)
    rid = property(lambda self: self._load.rid)

    def __init__(self, load, refType):
        if not isinstance(load, Load):
            raise TypeError('expected Load node, got %s' % type(load))
        Constant.__init__(self, load.cid, refType)
        self._load = load

    def __repr__(self):
        return 'LoadedConstant(%s, %s)' % (repr(self._load), repr(self.type))

    def __str__(self):
        return '%s <- R%s' % (super().__str__(), self._load.rid)

class Node:
    '''Base class for nodes.
    '''
    __slots__ = ()

class AccessNode(Node):
    '''Base class for Load and Store.
    '''
    __slots__ = ('_cid', '_rid')

    cid = property(lambda self: self._cid)
    rid = property(lambda self: self._rid)

    def __init__(self, cid, rid):
        if not isinstance(cid, int):
            raise TypeError('constant ID must be int, got %s' % type(cid))
        if not isinstance(rid, int):
            raise TypeError('reference ID must be int, got %s' % type(rid))
        self._cid = cid
        self._rid = rid

    def __repr__(self):
        return '%s(%d, %d)' % (self.__class__.__name__, self._cid, self._rid)

class Load(AccessNode):
    '''A node that loads a value from a storage location.
    '''
    __slots__ = ()

    def __str__(self):
        return 'load C%d from R%d' % (self._cid, self._rid)

class Store(AccessNode):
    '''A node that stores a value into a storage location.
    '''
    __slots__ = ()

    def __str__(self):
        return 'store C%d in R%d' % (self._cid, self._rid)

class ConstantValue(Expression):
    '''A synthetic constant containing an intermediate value.
    '''
    __slots__ = ('_constant',)

    constant = property(lambda self: self._constant)
    cid = property(lambda self: self._constant.cid)

    def __init__(self, constant):
        if not isinstance(constant, Constant):
            raise TypeError('expected Constant object, got %s' % type(constant))
        Expression.__init__(self, constant.type)
        self._constant = constant

    def _ctorargs(self, *exprs, **kwargs):
        cls = self.__class__
        if exprs:
            raise ValueError('%s does not take expression args' % cls.__name__)
        kwargs.setdefault('constant', self._constant)
        return signature(cls).bind(**kwargs)

    def __str__(self):
        return 'C%d' % self.cid

    def _equals(self, other):
        return self._constant is other._constant

    def _complexity(self):
        const = self._constant
        if isinstance(const, ComputedConstant):
            return 1 + const.expr._complexity()
        elif isinstance(const, LoadedConstant):
            return 2
        else:
            assert False, const

    def simplify(self):
        const = self._constant
        if isinstance(const, ComputedConstant):
            return const.expr.simplify()
        elif isinstance(const, LoadedConstant):
            return self
        else:
            assert False, const

class Function:

    def __init__(self, name, args, code):
        self.name = name
        self.args = args
        self.code = code

    def __repr__(self):
        return 'Function(%s, %s, %s)' % (
            repr(self.name), repr(self.args), repr(self.code)
            )

    def __str__(self):
        return 'func %s(%s)' % (
            self.name,
            ', '.join(arg.formatDecl() for arg in self.args.values())
            )

    def dump(self):
        print(str(self))
        self.code.dump()

class CodeBlock:

    def __init__(self, constants, references, nodes):
        self.constants = constants
        self.references = references
        self.nodes = nodes

    def verify(self):
        '''Performs consistency checks on the data in this code block.
        Raises AssertionError if an inconsistency is found.
        '''
        # Check that cids are unique.
        cids = set()
        for const in self.constants:
            cid = const.cid
            assert cid not in cids, const
            cids.add(cid)

        # Check that cids and rids in nodes are valid.
        numRefs = len(self.references)
        for node in self.nodes:
            assert node.cid in cids, node
            assert 0 <= node.rid < numRefs, node

        # Check that each loaded constant belongs to exactly one Load node.
        cidsFromLoadedConstants = set(
            const.cid
            for const in self.constants
            if isinstance(const, LoadedConstant)
            )
        cidsFromLoadNodes = set()
        for node in self.nodes:
            if isinstance(node, Load):
                cid = node.cid
                assert cid not in cidsFromLoadNodes, node
                cidsFromLoadNodes.add(cid)
        assert cidsFromLoadNodes == cidsFromLoadedConstants, (
            cidsFromLoadedConstants, cidsFromLoadNodes
            )

        # Check that cids in expressions are valid.
        def checkUsage(expr):
            if isinstance(expr, ConstantValue):
                assert expr.cid in cids
        for const in self.constants:
            if isinstance(const, ComputedConstant):
                const.expr.substitute(checkUsage)

        # Check that cids in I/O references are valid.
        for ref in self.references:
            if isinstance(ref, IOReference):
                assert ref.index.cid in cids

    def dump(self):
        '''Prints this code block on stdout.
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
            else:
                assert False, const
        print('    references:')
        for i, ref in enumerate(self.references):
            print('        %-4s R%-2d = %s' % ('%s&' % ref.type, i, ref))
        print('    nodes:')
        for node in self.nodes:
            print('        %s' % node)

    def simplify(self):
        '''Attempt to simplify the code block as much as possible.
        '''
        self.simplifyConstants()

    def simplifyConstants(self):
        newConsts = []
        for const in self.constants:
            if isinstance(const, ComputedConstant):
                expr = const.expr.simplify()
                if isinstance(expr, ConstantValue):
                    # This constant is equal to another constant, so replace
                    # its use in load/store nodes.
                    oldCid = const.cid
                    newCid = expr.cid
                    for i, node in enumerate(self.nodes):
                        if node.cid == oldCid:
                            self.nodes[i] = node.__class__(newCid, node.rid)
                else:
                    newConsts.append(ComputedConstant(const.cid, expr))
            elif isinstance(const, LoadedConstant):
                newConsts.append(const)
            else:
                assert False, const

        self.constants = newConsts
        self.removeUnusedConstants()

    def removeUnusedConstants(self):
        while True:
            cidsInUse = set()
            # Mark constants used in computations.
            def checkUsage(expr):
                if isinstance(expr, ConstantValue):
                    cidsInUse.add(expr.cid)
            for const in self.constants:
                if isinstance(const, ComputedConstant):
                    const.expr.substitute(checkUsage)
            # Mark constants used in stores.
            for node in self.nodes:
                if isinstance(node, Store):
                    cidsInUse.add(node.cid)
            # Mark constants used in references.
            for ref in self.references:
                if isinstance(ref, IOReference):
                    cidsInUse.add(ref.index.cid)

            if len(cidsInUse) < len(self.constants):
                newConsts = []
                for const in self.constants:
                    if const.cid in cidsInUse:
                        newConsts.append(const)
                    elif isinstance(const, LoadedConstant):
                        for i, node in enumerate(self.nodes):
                            if node.cid == const.cid:
                                assert isinstance(node, Load), node
                                storage = self.references[node.rid]
                                if storage.canLoadHaveSideEffect():
                                    # Keep Load node.
                                    newConsts.append(const)
                                else:
                                    # Remove the corresponding Load node as well.
                                    del self.nodes[i]
                                break
                        else:
                            assert False, const
                self.constants = newConsts
            else:
                assert len(cidsInUse) == len(self.constants)
                break

class CodeBlockBuilder:

    def __init__(self):
        self.constants = []
        self.references = []
        self.nodes = []

    def createCodeBlock(self):
        '''Returns a CodeBlock object containing the items emitted so far.
        The state of the builder does not change.
        '''
        return CodeBlock(self.constants, self.references, self.nodes)

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
        constant = LoadedConstant(load, refType)
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
        rid = len(self.references)
        self.references.append(storage)
        return rid

def emitCodeFromAssignments(log, builder, assignments):
    '''Creates a code block from the given assignments.
    Returns a CodeBlock instance.
    '''

    nameToReference = {}
    def getReferenceID(storage):
        if isinstance(storage, IOReference):
            assert isinstance(storage.index, ConstantValue), storage
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
        if isinstance(expr, Storage):
            return builder.emitLoad(getReferenceID(expr))
        else:
            return None

    def substituteIOIndices(expr):
        if isinstance(expr, IOReference):
            index = builder.emitCompute(expr.index.substitute(substituteReferences))
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
        rhsConst = builder.emitCompute(rhs.substitute(substituteReferences))

        # Constify the I/O indices to force emission of all loads before
        # we emit any stores.
        lhsConstIndices = lhs.substitute(substituteIOIndices)

        for storage, offset in decomposeConcat(lhsConstIndices):
            rid = getReferenceID(storage)
            builder.emitStore(rid, Slice(rhsConst, offset, storage.width))

def createFunc(log, name, args, assignments):
    builder = CodeBlockBuilder()
    emitCodeFromAssignments(log, builder, assignments)
    code = builder.createCodeBlock()
    code.simplifyConstants()
    return Function(name, args, code)
