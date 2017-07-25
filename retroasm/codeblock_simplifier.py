from .codeblock import CodeBlock, Load, LoadedValue, Store
from .expression_simplifier import simplifyExpression
from .reference import FixedValue
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
        # Peform initial simplification of all expressions.
        # This allows removeRedundantNodes() to only simplify expressions when
        # it changes them.
        self._updateExpressions(simplifyExpression)

        # This mainly collapses incremental updates to variables.
        self.removeRedundantNodes()

        # Removal of unused stores might make some loads unused.
        self.removeUnusedStores()

        # Removal of unused loads will not enable any other simplifications.
        self.removeUnusedLoads()

        assert self.verify() is None

    def removeRedundantNodes(self):
        nodes = self.nodes

        loadReplacements = {}
        def replaceLoadedValues(expr):
            newExpr = expr.substitute(loadReplacements.get)
            if newExpr is not expr:
                newExpr = simplifyExpression(newExpr)
            return newExpr

        # Remove redundant loads and stores by keeping track of the current
        # value of storages.
        currentValues = {}
        i = 0
        while i < len(nodes):
            node = nodes[i]
            storage = node.storage

            # Apply load replacements to storage.
            if loadReplacements:
                newStorage = storage.substituteExpressions(replaceLoadedValues)
                if newStorage is not storage:
                    node.storage = storage = newStorage

            value = currentValues.get(storage)
            if isinstance(node, Load):
                if value is not None:
                    # Use known value instead of loading it.
                    loadReplacements[node.expr] = value
                    if not storage.canLoadHaveSideEffect():
                        del nodes[i]
                        continue
                elif storage.isLoadConsistent():
                    # Remember loaded value.
                    currentValues[storage] = node.expr
            elif isinstance(node, Store):
                expr = node.expr

                # Apply load replacements to stored expression.
                if loadReplacements:
                    newExpr = expr.substitute(replaceLoadedValues)
                    if newExpr is not expr:
                        node.expr = expr = newExpr

                if value == expr:
                    # Current value is rewritten.
                    if not storage.canStoreHaveSideEffect():
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

        # Apply load replacements to returned reference.
        if loadReplacements:
            retRef = self.retRef
            if retRef is not None:
                newRef = retRef.substitute(expressionFunc=replaceLoadedValues)
                if newRef is not retRef:
                    self.retRef = newRef

    def removeUnusedStores(self):
        '''Remove side-effect-free stores that will be overwritten or that
        write a variable that will go out of scope.
        '''
        nodes = self.nodes

        # Determine which local variables are part of the returned reference.
        # Unlike other local variables, we can't eliminate these.
        retVars = set()
        retRef = self.retRef
        if retRef is not None:
            for storage in retRef.bits.iterStorages():
                if isinstance(storage, Variable) and storage.scope == 1:
                    retVars.add(storage)

        # Remove stores for which the value is overwritten before it is loaded.
        # Local variable loads were already eliminated by removeRedundantNodes()
        # and since variables cease to exist at the end of a block, all local
        # variable stores are at this point considered redundant, unless the
        # variable is part of the returned reference.
        willBeOverwritten = set()
        for i in range(len(nodes) - 1,  -1, -1):
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
                        del nodes[i]
                    willBeOverwritten.add(storage)

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
            for expr in retRef.bits.iterExpressions():
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
