from .codeblock import (
    ArgumentConstant, CodeBlock, ComputedConstant, ConstantValue, FixedValue,
    Load, LoadedConstant, SingleReference, Store
    )
from .expression_simplifier import simplifyExpression
from .storage import IOStorage, Variable
from .types import maskForWidth

from collections import defaultdict

class CodeBlockSimplifier(CodeBlock):

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
            changed |= self.removeUnusedStorages()
            changed |= self.removeDuplicateStorages()
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

        # Replace constant in other constants' expressions.
        def substCid(sexpr):
            if isinstance(sexpr, ConstantValue) and sexpr.cid == oldCid:
                return ConstantValue(newCid, sexpr.mask)
            else:
                return None
        for cid in list(constants.keys()):
            const = constants[cid]
            if isinstance(const, ComputedConstant):
                newExpr = const.expr.substitute(substCid)
                if newExpr is not const.expr:
                    constants[cid] = ComputedConstant(cid, newExpr)

        # Replace constant in storages.
        storages = self.storages
        for sid in list(storages.keys()):
            storage = storages[sid]
            if isinstance(storage, IOStorage):
                index = storage.index
                if index.cid == oldCid:
                    storages[sid] = IOStorage(
                        storage.channel, ConstantValue(newCid, index.mask)
                        )

        # Replace constant in nodes.
        nodes = self.nodes
        for i, node in enumerate(nodes):
            if node.cid == oldCid:
                nodes[i] = node.clone(cid=newCid)

        # Replace constant in return value.
        if isinstance(self.retRef, FixedValue) and self.retRef.cid == oldCid:
            self.retRef = FixedValue(self, newCid, self.retRef.type)

    def removeUnusedConstants(self):
        '''Finds constants that are not used and removes them.
        Returns True if any constants were removed, False otherwise.
        '''
        constants = self.constants
        storages = self.storages
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
                if storages[node.sid].canLoadHaveSideEffect():
                    # We can't eliminate this load because it may have a useful
                    # side effect and we can't eliminate the constant because
                    # every load needs one, so pretend the constant is in use.
                    cidsInUse.add(node.cid)
        # Mark constants used in storages.
        for storage in storages.values():
            if isinstance(storage, IOStorage):
                cidsInUse.add(storage.index.cid)
        # Mark constants used in return value.
        if isinstance(self.retRef, FixedValue):
            cidsInUse.add(self.retRef.cid)

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
            assert len(cidsInUse) == len(constants), (cidsInUse, constants)
            return False

    def removeUnusedStorages(self):
        '''Removes storages that are not used by any load/store node.
        '''
        unusedSids = set(self.storages.keys())
        for node in self.nodes:
            unusedSids.discard(node.sid)
        retRef = self.retRef
        if retRef is not None:
            unusedSids.difference_update(retRef.iterSIDs())
        for sid in unusedSids:
            del self.storages[sid]
        return bool(unusedSids)

    def removeDuplicateStorages(self):
        '''Removes storages that are obvious duplicates of other storages.
        Note that non-obvious duplicates (aliases) can remain.
        '''
        storages = self.storages

        # Figure out which storages are duplicates.
        duplicates = {}
        globalNameToSid = {}
        channelNameToIndices = defaultdict(list)
        for sid, storage in storages.items():
            if isinstance(storage, Variable) and storage.scope == 0:
                name = storage.name
                replacement = globalNameToSid.get(name)
                if replacement is None:
                    globalNameToSid[name] = sid
                else:
                    duplicates[sid] = replacement
            elif isinstance(storage, IOStorage):
                cid = storage.index.cid
                indices = channelNameToIndices[storage.channel.name]
                for sid2, index2 in indices:
                    if index2.cid == cid:
                        duplicates[sid] = sid2
                        break
                else:
                    indices.append((sid, storage.index))

        # Remove the duplicates.
        if duplicates:
            nodes = self.nodes
            for i, node in enumerate(nodes):
                replacement = duplicates.get(node.sid)
                if replacement is not None:
                    nodes[i] = node.clone(sid=replacement)
            for cid, const in self.constants.items():
                if isinstance(const, LoadedConstant):
                    sid = const.sid
                    replacement = duplicates.get(sid)
                    if replacement is not None:
                        self.constants[cid] = LoadedConstant(cid, replacement)
            for sid, replacement in duplicates.items():
                del storages[sid]
            return True
        else:
            return False

    def removeRedundantNodes(self):
        changed = False
        constants = self.constants
        storages = self.storages
        nodes = self.nodes

        # Remove redundant loads and stores by keeping track of the current
        # value of storages.
        currentValues = {}
        i = 0
        while i < len(nodes):
            node = nodes[i]
            cid = node.cid
            sid = node.sid
            storage = storages[sid]
            value = currentValues.get(sid)
            if isinstance(node, Load):
                if value is not None:
                    # Re-use earlier loaded value.
                    constants[cid] = ComputedConstant(cid, value)
                    changed = True
                    if not storage.canLoadHaveSideEffect():
                        del nodes[i]
                        continue
                elif storage.isLoadConsistent():
                    # Remember loaded value.
                    currentValues[sid] = ConstantValue(
                        cid, maskForWidth(storage.width)
                        )
            elif isinstance(node, Store):
                if value is not None and value.cid == cid:
                    # Value is rewritten.
                    if not storage.canStoreHaveSideEffect():
                        changed = True
                        del nodes[i]
                        continue
                elif storage.isSticky():
                    # Remember stored value.
                    currentValues[sid] = ConstantValue(
                        cid, maskForWidth(storage.width)
                        )
                # Remove values for storages that might be aliases.
                for sid2 in list(currentValues.keys()):
                    if sid != sid2 and storage.mightBeSame(storages[sid2]):
                        # However, if the store wouldn't alter the value,
                        # there is no need to remove it.
                        if currentValues[sid2].cid != cid:
                            del currentValues[sid2]
            i += 1

        # Remove stores for which the value is overwritten before it is loaded.
        # Variable loads were already eliminated by the code above and since
        # variables cease to exist at the end of a block, all variable stores
        # are at this point considered redundant.
        willBeOverwritten = set()
        i = len(nodes) - 1
        while i >= 0:
            node = nodes[i]
            sid = node.sid
            storage = storages[sid]
            if not storage.canStoreHaveSideEffect():
                if isinstance(node, Load):
                    assert not (
                        isinstance(storage, Variable) and storage.scope == 1
                        ), storage
                    willBeOverwritten.discard(sid)
                elif isinstance(node, Store):
                    if isinstance(storage, Variable) and storage.scope == 1 \
                            and storage.name == 'ret':
                        if sid not in willBeOverwritten:
                            width = storage.width
                            assert self.retRef is None, self.retRef
                            self.retRef = FixedValue(
                                self, node.cid, storage.type
                                )
                    if sid in willBeOverwritten or (
                            isinstance(storage, Variable) and storage.scope == 1
                            ):
                        changed = True
                        del nodes[i]
                    willBeOverwritten.add(sid)
            i -= 1

        return changed
