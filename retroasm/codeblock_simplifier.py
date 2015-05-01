from .codeblock import (
    ArgumentConstant, CodeBlock, ComputedConstant, ConstantValue, Load,
    LoadedConstant, Store
    )
from .expression_simplifier import simplifyExpression
from .storage import FixedValue, IOReference, Register, Variable
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
            changed |= self.removeUnusedReferences()
            changed |= self.removeDuplicateReferences()
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

        # Replace constant in references.
        references = self.references
        for rid in list(references.keys()):
            ref = references[rid]
            if isinstance(ref, IOReference):
                index = ref.index
                if index.cid == oldCid:
                    references[rid] = IOReference(
                        ref.channel, ConstantValue(newCid, index.mask)
                        )

        # Replace constant in nodes.
        nodes = self.nodes
        for i, node in enumerate(nodes):
            if node.cid == oldCid:
                nodes[i] = node.clone(cid=newCid)

        # Replace return constant.
        if self.retCid == oldCid:
            self.retCid = newCid

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
            if isinstance(ref, FixedValue):
                cidsInUse.add(ref.cid)
            elif isinstance(ref, IOReference):
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
                    nodes[i] = node.clone(rid=replacement)
            for cid, const in self.constants.items():
                if isinstance(const, LoadedConstant):
                    rid = const.rid
                    replacement = duplicates.get(rid)
                    if replacement is not None:
                        self.constants[cid] = LoadedConstant(cid, replacement)
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

        # Remove redundant loads and stores by keeping track of the current
        # value of storages.
        currentValues = {}
        i = 0
        while i < len(nodes):
            node = nodes[i]
            cid = node.cid
            rid = node.rid
            storage = references[rid]
            if isinstance(storage, FixedValue):
                value = ConstantValue(storage.cid, maskForWidth(storage.width))
            else:
                value = currentValues.get(rid)
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
                    currentValues[rid] = ConstantValue(
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
                    currentValues[rid] = ConstantValue(
                        cid, maskForWidth(storage.width)
                        )
                # Remove values for references that might be aliases.
                for rid2 in list(currentValues.keys()):
                    if rid != rid2 and storage.mightBeSame(references[rid2]):
                        # However, if the store wouldn't alter the value,
                        # there is no need to remove it.
                        if currentValues[rid2].cid != cid:
                            del currentValues[rid2]
            i += 1

        # Remove stores for which the value is overwritten before it is loaded.
        # Variable loads were already eliminated by the code above and since
        # variables cease to exist at the end of a block, all variable stores
        # are at this point considered redundant.
        willBeOverwritten = set()
        i = len(nodes) - 1
        while i >= 0:
            node = nodes[i]
            rid = node.rid
            storage = references[rid]
            if not storage.canStoreHaveSideEffect():
                if isinstance(node, Load):
                    assert not isinstance(storage, Variable), storage
                    willBeOverwritten.discard(rid)
                elif isinstance(node, Store):
                    if rid in willBeOverwritten \
                            or isinstance(storage, (FixedValue, Variable)):
                        changed = True
                        del nodes[i]
                        if isinstance(storage, Variable) \
                                and storage.name == 'ret':
                            assert self.retCid is None, self.retCid
                            self.retCid = node.cid
                    else:
                        willBeOverwritten.add(rid)
            i -= 1

        return changed
