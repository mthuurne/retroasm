from .codeblock import (
    CodeBlock, ComputedConstant, ConstantValue, Load, LoadedConstant,
    SingleReference, Store, inlineConstants
    )
from .expression_simplifier import simplifyExpression
from .storage import IOStorage, Variable
from .types import maskForWidth

from collections import defaultdict

class CodeBlockSimplifier(CodeBlock):

    storages = property(CodeBlock._gatherStorages)

    def freeze(self):
        '''Change the type of this object from CodeBlockSimplifier to CodeBlock,
        to indicate that no further modifications are intended.
        '''
        self.__class__ = CodeBlock

    def simplify(self):
        '''Attempt to simplify the code block as much as possible.
        '''
        while True:
            changed = False
            changed |= self.inlineConstants()
            changed |= self.simplifyConstants()
            changed |= self.simplifyExpressions()
            changed |= self.removeRedundantNodes()
            if not changed:
                break
        assert self.verify() is None

    def inlineConstants(self):
        '''Replace each ConstantValue that contains the constant ID of
        a ComputedConstant by the expression of that constant.
        '''
        changed = False
        constants = self.constants

        def substConstExpr(expr):
            if isinstance(expr, ConstantValue):
                const = constants[expr.cid]
                if isinstance(const, ComputedConstant):
                    return const.expr
            return None
        for cid in list(constants.keys()):
            const = constants[cid]
            if isinstance(const, ComputedConstant):
                newExpr = const.expr.substitute(substConstExpr)
                if newExpr is not const.expr:
                    constants[cid] = ComputedConstant(cid, newExpr)

        while self.removeUnusedConstants():
            changed = True
        return changed

    def simplifyConstants(self):
        changed = False
        constants = self.constants

        for cid in list(constants.keys()):
            const = constants[cid]
            if isinstance(const, ComputedConstant):
                expr = simplifyExpression(const.expr)
                if isinstance(expr, ConstantValue):
                    # This constant is equal to another constant.
                    self.replaceConstant(cid, expr.cid)
                    changed = True
                elif expr is not const.expr:
                    # Wrap the simplified expression into a new constant
                    # with the same cid.
                    constants[cid] = ComputedConstant(cid, expr)
                    changed = True

        constsGrouped = defaultdict(list)
        for const in constants.values():
            if isinstance(const, ComputedConstant):
                constsGrouped[repr(const.expr)].append(const.cid)
        for similar in constsGrouped.values():
            # We call them "similar" and check equality to be on the safe side,
            # but in practice they're identical.
            i = len(similar) - 1
            while i > 0:
                # The first equal constant is the replacement for the others.
                icid = similar[i]
                iexpr = constants[icid].expr
                j = 0
                while True:
                    jcid = similar[j]
                    if constants[jcid].expr == iexpr:
                        self.replaceConstant(icid, jcid)
                        changed = True
                        break
                    j += 1
                    if j >= i:
                        break
                i -= 1

        while self.removeUnusedConstants():
            changed = True
        return changed

    def replaceConstant(self, oldCid, newCid):
        assert oldCid != newCid, newCid
        constants = self.constants
        del constants[oldCid]

        def substCid(sexpr):
            if isinstance(sexpr, ConstantValue) and sexpr.cid == oldCid:
                return ConstantValue(newCid, sexpr.mask)
            else:
                return None
        def substStorage(storage):
            if isinstance(storage, IOStorage):
                index = storage.index
                newIndex = index.substitute(substCid)
                if newIndex is not index:
                    return IOStorage(storage.channel, newIndex)
            return storage

        # Replace constant in other constants' expressions.
        for cid in list(constants.keys()):
            const = constants[cid]
            if isinstance(const, ComputedConstant):
                newExpr = const.expr.substitute(substCid)
                if newExpr is not const.expr:
                    constants[cid] = ComputedConstant(cid, newExpr)
            elif isinstance(const, LoadedConstant):
                storage = const.storage
                newStorage = substStorage(storage)
                if newStorage is not storage:
                    constants[cid] = LoadedConstant(cid, newStorage)

        # Replace constant in nodes.
        nodes = self.nodes
        for i, node in enumerate(nodes):
            expr = node.expr
            newExpr = expr.substitute(substCid)
            storage = node.storage
            newStorage = substStorage(storage)
            if newExpr is not expr or newStorage is not storage:
                nodes[i] = node.clone(expr=newExpr, storage=newStorage)

    def removeUnusedConstants(self):
        '''Finds constants that are not used and removes them.
        Returns True if any constants were removed, False otherwise.
        '''
        constants = self.constants
        cidsInUse = set()

        # Mark constants used in computations.
        for const in constants.values():
            if isinstance(const, ComputedConstant):
                for value in const.expr.iterInstances(ConstantValue):
                    cidsInUse.add(value.cid)
        # Mark constants used in stores and in loads with side effects.
        for node in self.nodes:
            if isinstance(node, Store):
                for value in node.expr.iterInstances(ConstantValue):
                    cidsInUse.add(value.cid)
            elif isinstance(node, Load):
                if node.storage.canLoadHaveSideEffect():
                    # We can't eliminate this load because it may have a useful
                    # side effect and we can't eliminate the constant because
                    # every load needs one, so pretend the constant is in use.
                    for value in node.expr.iterInstances(ConstantValue):
                        cidsInUse.add(value.cid)

        # Mark constants used in storages.
        for storage in self.storages:
            if isinstance(storage, IOStorage):
                for value in storage.index.iterInstances(ConstantValue):
                    cidsInUse.add(value.cid)

        if len(cidsInUse) < len(constants):
            cids = constants.keys()
            assert cidsInUse.issubset(cids), cidsInUse
            for cid in cids - cidsInUse:
                const = constants[cid]
                if isinstance(const, ComputedConstant):
                    del constants[cid]
                elif isinstance(const, LoadedConstant):
                    # Remove both constant and its Load node.
                    del constants[cid]
                    for i, node in enumerate(self.nodes):
                        if isinstance(node, Load) and node.expr.cid == cid:
                            del self.nodes[i]
                            break
                    else:
                        assert False, const
                else:
                    assert False, const
            return True
        else:
            assert len(cidsInUse) == len(constants), (cidsInUse, constants)
            return False

    def updateRetRef(self, singleRefUpdater, fixedValueUpdater):
        '''Updates the returned reference, if any.
        The updater arguments should be functions that, given a reference of
        the respective type, return a simplified version, or the same reference
        if no simplification was possible.
        Returns True iff the returned reference was updated.
        '''
        retRef = self.retRef
        if retRef is None:
            return False

        def checkChange(ref, func):
            newRef = func(ref)
            if newRef is not ref:
                changed[0] = True
            return newRef

        changed = [False]
        retRef = retRef.clone(
            lambda ref, func=singleRefUpdater: checkChange(ref, func),
            lambda ref, func=fixedValueUpdater: checkChange(ref, func)
            )
        if changed[0]:
            self.retRef = retRef
        return changed[0]

    def simplifyExpressions(self):
        changed = False
        nodes = self.nodes

        def simplifyStorage(storage):
            if isinstance(storage, IOStorage):
                index = storage.index
                newIndex = simplifyExpression(
                    inlineConstants(index, self.constants)
                    )
                if newIndex is not index:
                    return IOStorage(storage.channel, newIndex)
            return storage

        for i, node in enumerate(nodes):
            # Simplify stored expressions.
            if isinstance(node, Store):
                expr = node.expr
                newExpr = simplifyExpression(
                    inlineConstants(expr, self.constants)
                    )
                if newExpr is not expr:
                    changed = True
                    nodes[i] = node.clone(expr=newExpr)

            # Simplify I/O indices.
            storage = node.storage
            newStorage = simplifyStorage(storage)
            if newStorage is not storage:
                changed = True
                nodes[i] = node.clone(storage=newStorage)
                if isinstance(node, Load):
                    assert isinstance(node.expr, ConstantValue), node.expr
                    cid = node.expr.cid
                    const = self.constants[cid]
                    assert isinstance(const, LoadedConstant), const
                    assert const.storage == storage
                    self.constants[cid] = LoadedConstant(cid, newStorage)

        # Update returned reference.
        def simplifySingleRef(ref):
            storage = simplifyStorage(ref.storage)
            return ref if storage is ref.storage else SingleReference(
                self, storage, ref.type
                )
        def simplifyFixedValue(ref):
            expr = simplifyExpression(ref.expr)
            return ref if expr is ref.expr else FixedValue(expr, ref.type)
        changed |= self.updateRetRef(simplifySingleRef, simplifyFixedValue)

        return changed

    def removeRedundantNodes(self):
        changed = False
        constants = self.constants
        nodes = self.nodes

        # Remove redundant loads and stores by keeping track of the current
        # value of storages.
        currentValues = {}
        loadReplacements = {}
        def substStorage(storage):
            if isinstance(storage, IOStorage):
                index = storage.index
                newIndex = index.substitute(loadReplacements.get)
                if newIndex is not index:
                    return IOStorage(storage.channel, newIndex)
            return storage
        i = 0
        while i < len(nodes):
            node = nodes[i]
            expr = node.expr
            storage = substStorage(node.storage)
            value = currentValues.get(storage)
            if isinstance(node, Load):
                if value is not None:
                    # Use known value instead of loading it.
                    cid = expr.cid
                    constants[cid] = ComputedConstant(cid, value)
                    changed = True
                    loadReplacements[expr] = value
                    if not storage.canLoadHaveSideEffect():
                        del nodes[i]
                        continue
                elif storage.isLoadConsistent():
                    # Remember loaded value.
                    currentValues[storage] = expr
            elif isinstance(node, Store):
                expr = expr.substitute(loadReplacements.get)
                if value == expr:
                    # Current value is rewritten.
                    if not storage.canStoreHaveSideEffect():
                        changed = True
                        del nodes[i]
                        continue
                elif storage.isSticky():
                    # Remember stored value.
                    currentValues[storage] = expr
                # Remove values for storages that might be aliases.
                for storage2 in list(currentValues.keys()):
                    if storage != storage2 and storage.mightBeSame(storage2):
                        # However, if the store wouldn't alter the value,
                        # there is no need to remove it.
                        if currentValues[storage2] != expr:
                            del currentValues[storage2]
            # Replace node if storage or expression got updated.
            if node.storage is not storage or node.expr is not expr:
                changed = True
                nodes[i] = node.clone(storage=storage, expr=expr)
                if isinstance(node, Load):
                    # Update storage in LoadedConstant as well.
                    cid = expr.cid
                    if isinstance(constants[cid], LoadedConstant):
                        constants[cid] = LoadedConstant(cid, storage)
            i += 1

        # Update returned reference.
        def replaceSingleRef(ref):
            storage = substStorage(ref.storage)
            return ref if storage is ref.storage else SingleReference(
                self, storage, ref.type
                )
        def replaceFixedValue(ref):
            expr = ref.expr.substitute(loadReplacements.get)
            return ref if expr is ref.expr else FixedValue(expr, ref.type)
        changed |= self.updateRetRef(replaceSingleRef, replaceFixedValue)

        # Remove stores for which the value is overwritten before it is loaded.
        # Variable loads were already eliminated by the code above and since
        # variables cease to exist at the end of a block, all variable stores
        # are at this point considered redundant.
        willBeOverwritten = set()
        i = len(nodes) - 1
        while i >= 0:
            node = nodes[i]
            storage = node.storage
            if not storage.canStoreHaveSideEffect():
                if isinstance(node, Load):
                    assert not (
                        isinstance(storage, Variable) and storage.scope == 1
                        ), storage
                    willBeOverwritten.discard(storage)
                elif isinstance(node, Store):
                    if storage in willBeOverwritten or (
                            isinstance(storage, Variable) and
                            storage.scope == 1 and
                            storage.name != 'ret'
                            ):
                        changed = True
                        del nodes[i]
                    willBeOverwritten.add(storage)
            i -= 1

        return changed
