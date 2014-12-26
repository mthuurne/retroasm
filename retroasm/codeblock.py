from .expression import Expression, IOReference, IntType, LocalValue, Register

from collections import OrderedDict, defaultdict
from inspect import signature

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
        self._expr = Expression.checkScalar(expr)
        Constant.__init__(self, cid, expr.type)

    def __repr__(self):
        return 'ComputedConstant(%d, %s)' % (self._cid, repr(self._expr))

    def __str__(self):
        return '%s = %s' % (super().__str__(), self._expr)

class ArgumentConstant(Constant):
    '''A constant passed into a code block as an argument.
    '''
    __slots__ = ('_name',)

    name = property(lambda self: self._name)

    def __init__(self, name, cid, argType):
        if not isinstance(name, str):
            raise TypeError('name must be a string, got %s' % type(name))
        Constant.__init__(self, cid, argType)
        self._name = name

    def __repr__(self):
        return 'ArgumentConstant(%s, %d, %s)' % (
            repr(self._name), self._cid, repr(self.type)
            )

    def __str__(self):
        return '%s :  %s' % (super().__str__(), self._name)

class LoadedConstant(Constant):
    '''A constant defined by loading a value from a storage location.
    '''
    __slots__ = ('_rid',)

    rid = property(lambda self: self._rid)

    def __init__(self, cid, rid, refType):
        if not isinstance(rid, int):
            raise TypeError('reference ID must be int, got %s' % type(rid))
        Constant.__init__(self, cid, refType)
        self._rid = rid

    def __repr__(self):
        return 'LoadedConstant(%d, %d, %s)' % (
            self._cid, self._rid, repr(self.type)
            )

    def __str__(self):
        return '%s <- R%s' % (super().__str__(), self._rid)

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
        # pylint: disable=protected-access
        return self._constant is other._constant

    def _complexity(self):
        const = self._constant
        if isinstance(const, ComputedConstant):
            # pylint: disable=protected-access
            return 1 + const.expr._complexity()
        elif isinstance(const, (ArgumentConstant, LoadedConstant)):
            return 2
        else:
            assert False, const

    def simplify(self):
        const = self._constant
        if isinstance(const, ComputedConstant):
            return const.expr.simplify()
        elif isinstance(const, (ArgumentConstant, LoadedConstant)):
            return self
        else:
            assert False, const

class CodeBlock:

    def __init__(self, constants, references, nodes):
        constantsDict = OrderedDict()
        for const in constants:
            cid = const.cid
            if cid in constantsDict:
                raise ValueError('duplicate constant ID: %d' % cid)
            constantsDict[cid] = const
        self.constants = constantsDict
        self.references = OrderedDict(enumerate(references))
        self.nodes = nodes
        self.retCid = None

    def verify(self):
        '''Performs consistency checks on the data in this code block.
        Raises AssertionError if an inconsistency is found.
        '''
        # Check that cid keys match the value's cid.
        for cid, const in self.constants.items():
            assert isinstance(const, Constant), const
            assert const.cid == cid, const
        cids = self.constants.keys()

        # Check that cids and rids in nodes are valid.
        for node in self.nodes:
            assert node.cid in cids, node
            assert node.rid in self.references, node

        # Check that each loaded constant belongs to exactly one Load node.
        cidsFromLoadedConstants = set(
            cid
            for cid, const in self.constants.items()
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
        for const in self.constants.values():
            if isinstance(const, ComputedConstant):
                const.expr.substitute(checkUsage)

        # Check that cids in I/O references are valid.
        for ref in self.references.values():
            if isinstance(ref, IOReference):
                assert ref.index.cid in cids

    def dump(self):
        '''Prints this code block on stdout.
        '''
        print('    constants:')
        for const in self.constants.values():
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
        if self.retCid is not None:
            print('        %-4s ret =  C%d' % (
                self.constants[self.retCid].type, self.retCid
                ))
        print('    references:')
        for rid, ref in self.references.items():
            print('        %-4s R%-2d = %s' % ('%s&' % ref.type, rid, ref))
        print('    nodes:')
        for node in self.nodes:
            print('        %s' % node)

    def simplify(self):
        '''Attempt to simplify the code block as much as possible.
        '''
        while True:
            changed = False
            changed |= self.simplifyConstants()
            changed |= self.removeUnusedReferences()
            changed |= self.removeDuplicateReferences()
            changed |= self.removeRedundantNodes()
            if not changed:
                break

    def simplifyConstants(self):
        changed = False
        constants = self.constants

        for cid in list(constants.keys()):
            const = constants[cid]
            if isinstance(const, ComputedConstant):
                expr = const.expr.simplify()
                if isinstance(expr, ConstantValue):
                    # This constant is equal to another constant.
                    self.replaceConstant(cid, expr.constant)
                    changed = True
                elif expr is not const.expr:
                    # Wrap the simplified expression into a new constant
                    # with the same cid.
                    self.replaceConstant(cid, ComputedConstant(cid, expr))
                    changed = True

        constsGrouped = defaultdict(list)
        for const in constants.values():
            if isinstance(const, ComputedConstant):
                constsGrouped[repr(const.expr)].append(const)
        for similar in constsGrouped.values():
            # We call them "similar" and check equality to be on the safe side,
            # but in practice they're identical.
            i = len(similar) - 1
            while i > 0:
                # The first equal constant is the replacement for the others.
                iexpr = similar[i].expr
                j = 0
                while True:
                    if similar[j].expr == iexpr:
                        self.replaceConstant(similar[i].cid, similar[j])
                        changed = True
                        break
                    j += 1
                    if j >= i:
                        break
                i -= 1

        while self.removeUnusedConstants():
            changed = True
        return changed

    def replaceConstant(self, oldCid, newConst):
        constants = self.constants
        newCid = newConst.cid
        if oldCid == newCid:
            constants[oldCid] = newConst
        else:
            del constants[oldCid]
            # Replace constant in other constants' expressions.
            def substCid(sexpr):
                # pylint: disable=cell-var-from-loop
                if isinstance(sexpr, ConstantValue) and sexpr.cid == oldCid:
                    return ConstantValue(newConst)
                else:
                    return None
            for cid in list(constants.keys()):
                const = constants[cid]
                if isinstance(const, ComputedConstant):
                    newExpr = const.expr.substitute(substCid)
                    if newExpr is not const.expr:
                        constants[cid] = ComputedConstant(cid, newExpr)
            # Replace constant in references.
            references = self.references
            for rid in list(references.keys()):
                ref = references[rid]
                if isinstance(ref, IOReference) and ref.index.cid == oldCid:
                    references[rid] = IOReference(
                        ref.channel, ConstantValue(newConst)
                        )
            # Replace constant in nodes.
            nodes = self.nodes
            for i, node in enumerate(nodes):
                if node.cid == oldCid:
                    nodes[i] = node.__class__(newCid, node.rid)

    def removeUnusedConstants(self):
        '''Finds constants that are not used and removes them.
        Returns True if any constants were removed, False otherwise.
        '''
        constants = self.constants
        references = self.references
        cidsInUse = set()

        # Mark constants used in computations.
        def checkUsage(expr):
            if isinstance(expr, ConstantValue):
                cidsInUse.add(expr.cid)
        for const in constants.values():
            if isinstance(const, ComputedConstant):
                const.expr.substitute(checkUsage)
        # Mark constants used in stores and in loads with side effects.
        for node in self.nodes:
            if isinstance(node, Store):
                cidsInUse.add(node.cid)
            elif isinstance(node, Load):
                if references[node.rid].canLoadHaveSideEffect():
                    # We can't eliminate this load because it may have a useful
                    # side effect and we can't eliminate the constant because
                    # every load needs one, so pretend the constant is in use.
                    cidsInUse.add(node.cid)
        # Mark constants used in references.
        for ref in references.values():
            if isinstance(ref, IOReference):
                cidsInUse.add(ref.index.cid)
        # Mark constant that contains return value.
        retCid = self.retCid
        if retCid is not None:
            cidsInUse.add(retCid)

        if len(cidsInUse) < len(constants):
            cids = constants.keys()
            assert cidsInUse.issubset(cids), cidsInUse
            for cid in cids - cidsInUse:
                const = constants[cid]
                if isinstance(const, (ComputedConstant, ArgumentConstant)):
                    del constants[cid]
                elif isinstance(const, LoadedConstant):
                    # Remove both constant and its Load node.
                    del constants[cid]
                    for i, node in enumerate(self.nodes):
                        if node.cid == cid:
                            assert isinstance(node, Load), node
                            del self.nodes[i]
                            break
                    else:
                        assert False, const
                else:
                    assert False, const
            return True
        else:
            assert len(cidsInUse) == len(constants)
            return False

    def removeUnusedReferences(self):
        '''Removes references that are not used by any load/store node.
        '''
        unusedRids = set(self.references.keys())
        for node in self.nodes:
            unusedRids.discard(node.rid)
        for rid in unusedRids:
            del self.references[rid]
        return bool(unusedRids)

    def removeDuplicateReferences(self):
        '''Removes references that are obvious duplicates of other references.
        Note that non-obvious duplicates (aliases) can remain.
        '''
        references = self.references

        # Figure out which references are duplicates.
        duplicates = {}
        registerNameToRid = {}
        channelNameToIndices = defaultdict(list)
        for rid, ref in references.items():
            if isinstance(ref, Register):
                name = ref.name
                replacement = registerNameToRid.get(name)
                if replacement is None:
                    registerNameToRid[name] = rid
                else:
                    duplicates[rid] = replacement
            elif isinstance(ref, IOReference):
                cid = ref.index.cid
                indices = channelNameToIndices[ref.channel.name]
                for rid2, index2 in indices:
                    if index2.cid == cid:
                        duplicates[rid] = rid2
                        break
                else:
                    indices.append((rid, ref.index))

        # Remove the duplicates.
        if duplicates:
            nodes = self.nodes
            for i, node in enumerate(nodes):
                replacement = duplicates.get(node.rid)
                if replacement is not None:
                    nodes[i] = node.__class__(node.cid, replacement)
            for rid, replacement in duplicates.items():
                del references[rid]
            return True
        else:
            return False

    def removeRedundantNodes(self):
        changed = False
        constants = self.constants
        references = self.references
        nodes = self.nodes

        currentValueCids = {}
        i = 0
        while i < len(nodes):
            node = nodes[i]
            rid = node.rid
            valueCid = currentValueCids.get(rid)
            storage = references[rid]
            if isinstance(node, Load):
                if valueCid is not None:
                    # Re-use earlier loaded value.
                    constants[node.cid] = ComputedConstant(
                        node.cid, ConstantValue(constants[valueCid])
                        )
                    changed = True
                    if not storage.canLoadHaveSideEffect():
                        del nodes[i]
                        continue
                elif storage.isLoadConsistent():
                    # Remember loaded value.
                    currentValueCids[rid] = node.cid
            elif isinstance(node, Store):
                if valueCid == node.cid:
                    # Value is rewritten.
                    if not storage.canStoreHaveSideEffect():
                        changed = True
                        del nodes[i]
                        continue
                elif storage.isSticky():
                    # Remember stored value.
                    currentValueCids[rid] = node.cid
                # Remove values for references that might be aliases.
                for rid2 in list(currentValueCids.keys()):
                    if rid != rid2 and storage.mightBeSame(references[rid2]):
                        # However, if the store wouldn't alter the value,
                        # there is no need to remove it.
                        if currentValueCids[rid2] != node.cid:
                            del currentValueCids[rid2]
            i += 1

        # Remove stores for which the value is overwritten before it is loaded.
        willBeOverwritten = set()
        i = len(nodes) - 1
        while i >= 0:
            node = nodes[i]
            rid = node.rid
            if not references[rid].canStoreHaveSideEffect():
                if isinstance(node, Load):
                    willBeOverwritten.discard(rid)
                elif isinstance(node, Store):
                    if rid in willBeOverwritten:
                        changed = True
                        del nodes[i]
                    else:
                        willBeOverwritten.add(rid)
            i -= 1

        # Since local values do not suffer from aliasing, all reads after the
        # first and all writes before the last will have been eliminated by
        # the code above. The final load can be replaced by a constant.
        # And since local values cease to exist at the end of a block, the
        # final store can be removed.
        i = 0
        localValuesLoaded = set()
        localValuesStored = set()
        while i < len(nodes):
            node = nodes[i]
            rid = node.rid
            ref = references[rid]
            if isinstance(ref, LocalValue):
                if isinstance(node, Load):
                    assert rid not in localValuesLoaded, rid
                    localValuesLoaded.add(rid)
                    changed = True
                    del nodes[i]
                    # Replace LoadedConstant by ArgumentConstant.
                    cid = node.cid
                    constants[cid] = ArgumentConstant(ref.name, cid, ref.type)
                    continue
                elif isinstance(node, Store):
                    assert rid not in localValuesStored, rid
                    localValuesStored.add(rid)
                    changed = True
                    del nodes[i]
                    if ref.name == 'ret':
                        assert self.retCid is None, self.retCid
                        self.retCid = node.cid
                    continue
            i += 1

        return changed
