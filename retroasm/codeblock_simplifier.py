from .codeblock import CodeBlock, Load, LoadedValue, Store
from .expression_simplifier import simplifyExpression
from .reference import FixedValue, SingleReference
from .storage import Variable

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
            changed |= self._updateExpressions(simplifyExpression)
            changed |= self.removeRedundantNodes()
            if not changed:
                break

        # Removal of unused loads will not enable any other simplifications.
        self.removeUnusedLoads()

        assert self.verify() is None

    def removeRedundantNodes(self):
        changed = False
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
            changed |= self._updateExpressions(loadReplacements.get)

        # Determine which local variables are part of the returned reference.
        # Unlike other local variables, we can't eliminate these.
        retVars = set()
        retRef = self.retRef
        if retRef is not None:
            for storage in retRef.iterStorages():
                if isinstance(storage, Variable) and storage.scope == 1:
                    retVars.add(storage)

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
                            storage not in retVars
                            ):
                        changed = True
                        del nodes[i]
                    willBeOverwritten.add(storage)
            i -= 1

        return changed

    def removeUnusedLoads(self):
        '''Remove side-effect-free loads of which the LoadedValue is unused.
        '''
        nodes = self.nodes

        # Keep track of how often each LoadedValue is used.
        useCounts = defaultdict(int)
        def updateCounts(expr, delta=1):
            for loaded in expr.iterInstances(LoadedValue):
                useCounts[loaded] += delta

        # Compute initial use counts.
        for node in nodes:
            if isinstance(node, Store):
                updateCounts(node.expr)
            for expr in node.storage.iterExpressions():
                updateCounts(expr)
        retRef = self.retRef
        if retRef is not None:
            for expr in retRef.iterExpressions():
                updateCounts(expr)

        # Remove unnecesary Loads.
        for i in range(len(nodes) - 1,  -1, -1):
            node = nodes[i]
            if isinstance(node, Load):
                if useCounts[node.expr] == 0:
                    storage = node.storage
                    if not storage.canLoadHaveSideEffect():
                        del nodes[i]
                        # Update useCounts, so we can remove earlier Loads that
                        # became unused because the Load we just removed was
                        # the sole user of their LoadedValue.
                        for expr in storage.iterExpressions():
                            updateCounts(expr, -1)
