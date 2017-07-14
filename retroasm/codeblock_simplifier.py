from .codeblock import (
    CodeBlock, FixedValue, Load, LoadedValue, SingleReference, Store
    )
from .expression_simplifier import simplifyExpression
from .storage import Variable

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
            changed |= self.removeUnusedLoads()
            if not changed:
                break
        assert self.verify() is None

    def simplifyExpressions(self):
        changed = False
        nodes = self.nodes

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
            newStorage = storage.substituteExpressions(simplifyExpression)
            if newStorage is not storage:
                changed = True
                node.storage = newStorage

        # Update returned reference.
        def simplifySingleRef(ref):
            storage = ref.storage.substituteExpressions(simplifyExpression)
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

    def removeUnusedLoads(self):
        '''Remove side-effect-free loads of which the LoadedValue is unused.
        Returns True iff any loads were removed.
        '''
        # Find all LoadedValues that are used.
        valuesInUse = {
            loaded
            for expr in self.expressions
            for loaded in expr.iterInstances(LoadedValue)
            }

        # Remove unused Loads.
        nodes = self.nodes
        changed = False
        i = 0
        while i < len(nodes):
            node = nodes[i]
            if isinstance(node, Load):
                if node.expr not in valuesInUse:
                    if not node.storage.canLoadHaveSideEffect():
                        changed = True
                        del nodes[i]
                        continue
            i += 1
        return changed
