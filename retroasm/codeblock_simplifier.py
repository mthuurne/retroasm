from .codeblock import (
    CodeBlock, ConstantValue, Load, LoadedConstant, SingleReference, Store
    )
from .expression_simplifier import simplifyExpression
from .storage import IOStorage, Variable
from .types import maskForWidth

from collections import defaultdict

class CodeBlockSimplifier(CodeBlock):

    expressions = property(CodeBlock._gatherExpressions)
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
            changed |= self.simplifyExpressions()
            changed |= self.removeRedundantNodes()
            changed |= self.removeUnusedConstants()
            if not changed:
                break
        assert self.verify() is None

    def removeUnusedConstants(self):
        '''Finds constants that are not used and removes them.
        Returns True if any constants were removed, False otherwise.
        '''
        constants = self.constants
        cidsInUse = set()

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
                if isinstance(const, LoadedConstant):
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

    def simplifyExpressions(self):
        changed = False
        nodes = self.nodes

        def simplifyStorage(storage):
            if isinstance(storage, IOStorage):
                index = storage.index
                newIndex = simplifyExpression(index)
                if newIndex is not index:
                    return IOStorage(storage.channel, newIndex)
            return storage

        for node in nodes:
            # Simplify stored expressions.
            if isinstance(node, Store):
                expr = node.expr
                newExpr = simplifyExpression(expr)
                if newExpr is not expr:
                    changed = True
                    node.expr = newExpr

            # Simplify I/O indices.
            storage = node.storage
            newStorage = simplifyStorage(storage)
            if newStorage is not storage:
                changed = True
                node.storage = newStorage
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
        i = 0
        while i < len(nodes):
            node = nodes[i]
            storage = node.storage
            value = currentValues.get(storage)
            if isinstance(node, Load):
                if value is not None:
                    # Use known value instead of loading it.
                    loadReplacements[node.expr] = value
                    if not storage.canLoadHaveSideEffect():
                        changed = True
                        del nodes[i]
                        del constants[node.expr.cid]
                        continue
                elif storage.isLoadConsistent():
                    # Remember loaded value.
                    currentValues[storage] = node.expr
            elif isinstance(node, Store):
                expr = node.expr
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
            i += 1
        if loadReplacements:
            # Compute the transitive closure of the replacements, to avoid
            # inserting a replacement expression that itself contains
            # expressions that should be replaced.
            for key, expr in loadReplacements.items():
                while True:
                    newExpr = expr.substitute(loadReplacements.get)
                    if newExpr is expr:
                        break
                    expr = newExpr
                loadReplacements[key] = expr
            # Apply load replacement.
            changed |= self.updateExpressions(loadReplacements.get)

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
