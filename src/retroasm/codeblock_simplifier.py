from __future__ import annotations

from typing import AbstractSet, DefaultDict

from .codeblock import CodeBlock, Load, LoadedValue, Store
from .expression import Expression
from .expression_simplifier import simplifyExpression
from .reference import FixedValue
from .storage import Storage, Variable


class CodeBlockSimplifier(CodeBlock):

    @property
    def expressions(self) -> AbstractSet[Expression]:
        return self._gatherExpressions()

    @property
    def storages(self) -> AbstractSet[Storage]:
        return self._gatherStorages()

    def freeze(self) -> None:
        '''Change the type of this object from CodeBlockSimplifier to CodeBlock,
        to indicate that no further modifications are intended.
        '''
        self.__class__ = CodeBlock # type: ignore

    def simplify(self) -> None:
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

        assert self.verify()

    def removeRedundantNodes(self) -> None:
        nodes = self.nodes

        loadReplacements: dict[Expression, Expression] = {}
        def replaceLoadedValues(expr: Expression) -> Expression:
            newExpr = expr.substitute(loadReplacements.get)
            if newExpr is not expr:
                newExpr = simplifyExpression(newExpr)
            return newExpr

        # Remove redundant loads and stores by keeping track of the current
        # value of storages.
        currentValues: dict[Storage, Expression] = {}
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

        # Fixate variables and apply load replacements in returned bit strings.
        returned = self.returned
        for i, retBits in enumerate(returned):
            def fixateVariables(storage: Storage) -> FixedValue | None:
                if isinstance(storage, Variable) and storage.scope == 1:
                    return FixedValue(currentValues[storage], storage.width)
                else:
                    return None
            newBits = retBits.substitute(fixateVariables, replaceLoadedValues)
            if newBits is not retBits:
                returned[i] = newBits

    def removeUnusedStores(self) -> None:
        '''Remove side-effect-free stores that will be overwritten or that
        write a variable that will go out of scope.
        '''
        nodes = self.nodes

        # Remove stores for which the value is overwritten before it is loaded.
        # Local variable loads were already eliminated by removeRedundantNodes()
        # and since variables cease to exist at the end of a block, all local
        # variable stores are at this point considered redundant, unless the
        # variable is part of the returned bit string.
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
                            isinstance(storage, Variable) and storage.scope == 1
                            ):
                        del nodes[i]
                    willBeOverwritten.add(storage)

    def removeUnusedLoads(self) -> None:
        '''Remove side-effect-free loads of which the LoadedValue is unused.
        '''
        nodes = self.nodes

        # Keep track of how often each LoadedValue is used.
        useCounts = DefaultDict[LoadedValue, int](int)
        def updateCounts(expr: Expression, delta: int = 1) -> None:
            for loaded in expr.iterInstances(LoadedValue):
                useCounts[loaded] += delta

        # Compute initial use counts.
        for node in nodes:
            if isinstance(node, Store):
                updateCounts(node.expr)
            for expr in node.storage.iterExpressions():
                updateCounts(expr)
        for retBits in self.returned:
            for expr in retBits.iterExpressions():
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
