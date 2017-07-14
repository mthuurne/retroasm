from .expression import (
    AddOperator, AndOperator, Expression, IntLiteral, LShift, LVShift,
    OrOperator, RVShift, SignExtension, XorOperator, optSlice, truncate
    )
from .expression_simplifier import simplifyExpression
from .linereader import InputLocation
from .storage import Storage, Variable
from .types import IntType, maskForWidth, unlimited
from .utils import checkType, const_property

class Node:
    '''Base class for nodes.
    '''
    __slots__ = ()

class AccessNode(Node):
    '''Base class for Load and Store.
    '''
    __slots__ = ('_expr', '_storage', '_location')

    expr = property(lambda self: self._expr)
    storage = property(lambda self: self._storage)
    location = property(lambda self: self._location)

    def __init__(self, expr, storage, location):
        self._expr = expr
        self._storage = storage
        self._location = None if location is None else checkType(
            location, InputLocation, 'location'
            )

    @expr.setter
    def expr(self, expr):
        self._expr = Expression.checkScalar(expr)

    @storage.setter
    def storage(self, storage):
        self._storage = checkType(storage, Storage, 'storage')

    def clone(self):
        '''Create a clone of this node.
        '''
        raise NotImplementedError

class Load(AccessNode):
    '''A node that loads a value from a storage location.
    '''
    __slots__ = ()

    def __init__(self, storage, location=None):
        checkType(storage, Storage, 'storage')
        expr = LoadedValue(self, maskForWidth(storage.width))
        AccessNode.__init__(self, expr, storage, location)

    def __repr__(self):
        return 'Load(%r, %r)' % (self._storage, self._location)

    def __str__(self):
        return 'load from %s' % self._storage

    @AccessNode.expr.setter
    def expr(self, expr):
        raise AttributeError('Cannot change expression for Load nodes')

    def clone(self):
        return Load(self._storage, self._location)

class Store(AccessNode):
    '''A node that stores a value into a storage location.
    '''
    __slots__ = ()

    def __init__(self, expr, storage, location=None):
        AccessNode.__init__(
            self,
            Expression.checkScalar(expr),
            checkType(storage, Storage, 'storage'),
            location
            )

    def __repr__(self):
        return 'Store(%r, %r, %r)' % (self._expr, self._storage, self._location)

    def __str__(self):
        return 'store %s in %s' % (self._expr, self._storage)

    def clone(self):
        return Store(self._expr, self._storage, self._location)

class ArgumentValue(Expression):
    '''A value passed into a code block as an argument.
    '''
    __slots__ = ('_name', '_mask')

    name = property(lambda self: self._name)
    mask = property(lambda self: self._mask)

    def __init__(self, name, mask):
        Expression.__init__(self)
        self._name = checkType(name, str, 'name')
        self._mask = checkType(mask, int, 'mask')

    def _ctorargs(self):
        return self._name, self._mask

    def __repr__(self):
        return 'ArgumentValue(%r, %d)' % (self._name, self._mask)

    def __str__(self):
        return self._name

    def _equals(self, other):
        # pylint: disable=protected-access
        return self._name == other._name

class LoadedValue(Expression):
    '''A value loaded from a storage location.
    '''
    __slots__ = ('_load', '_mask')

    load = property(lambda self: self._load)
    mask = property(lambda self: self._mask)

    def __init__(self, load, mask):
        Expression.__init__(self)
        self._load = checkType(load, Load, 'load node')
        self._mask = checkType(mask, int, 'mask')

    def _ctorargs(self):
        return self._load, self._mask

    def __repr__(self):
        return 'LoadedValue(%r, %d)' % (self._load, self._mask)

    def __str__(self):
        return 'load(%s)' % self._load.storage

    def _equals(self, other):
        # pylint: disable=protected-access
        return self._load is other._load

def identical(ref):
    '''Clone function that returns the passed reference as-is.
    This is used as the default clone function in Reference.clone().
    '''
    return ref

class Reference:
    '''Abstract base class for references.
    '''
    __slots__ = ('_type',)

    type = property(lambda self: self._type)
    width = property(lambda self: self._type.width)

    def __init__(self, typ):
        self._type = checkType(typ, IntType, 'value type')

    def iterExpressions(self):
        '''Iterates through the expressions contained in this reference.
        '''
        raise NotImplementedError

    def iterStorages(self):
        '''Iterates through the storages accessed through this reference.
        '''
        raise NotImplementedError

    def clone(self, singleRefCloner=identical, fixedValueCloner=identical):
        '''Returns a deep copy of this reference, in which each SingleReference
        is passed to the singleRefCloner function and replaced by the Reference
        returned by that function, as well as each FixedValue passed to the
        fixedValueCloner function and replaced by its return value.
        If the copy would be identical to the cloned reference, the original
        object is returned instead of a copy. Since References are immutable,
        this optimization is safe.
        '''
        raise NotImplementedError

    def updateStorageExpressions(self, substFunc):
        '''Returns a deep copy of this reference, in which each IOStorage index
        and FixedValue expression is passed to the given substitution function.
        See Expression.substitute() for details about the substitution function.
        If the copy would be identical to the cloned reference, the original
        object is returned instead of a copy. Since References are immutable,
        this optimization is safe.
        '''
        def updateSingleRef(ref):
            storage = ref.storage
            newStorage = storage.substituteExpressions(substFunc)
            if newStorage is storage:
                return ref
            else:
                return SingleReference(ref.block, newStorage, ref.type)

        def updateFixedValue(ref):
            expr = ref.expr
            newExpr = expr.substitute(substFunc)
            if newExpr is expr:
                return ref
            else:
                return FixedValue(newExpr, ref.type)

        return self.clone(updateSingleRef, updateFixedValue)

    def emitLoad(self, location):
        '''Emits load nodes for loading a typed value from the referenced
        storage(s).
        Returns the loaded value as an Expression.
        '''
        value = self._emitLoadBits(location)

        # Apply sign extension, if necessary.
        typ = self._type
        if typ.signed:
            width = typ.width
            if width is not unlimited:
                return SignExtension(value, width)
        return value

    def _emitLoadBits(self, location):
        '''Emits load nodes for loading a bit string from the referenced
        storage(s).
        Returns the value of the bit string as an Expression.
        '''
        raise NotImplementedError

    def emitStore(self, value, location):
        '''Emits store nodes for storing a value into the referenced storage(s).
        '''
        self._emitStoreBits(truncate(value, self.width), location)

    def _emitStoreBits(self, value, location):
        '''Emits store nodes for storing a bit string into the referenced
        storage(s).
        '''
        raise NotImplementedError

class FixedValue(Reference):
    '''A reference that always reads as the same value and ignores writes.
    '''
    __slots__ = ('_expr',)

    expr = property(lambda self: self._expr)

    def __init__(self, expr, typ):
        Reference.__init__(self, typ)
        self._expr = Expression.checkScalar(expr)

    def __repr__(self):
        return 'FixedValue(%r, %r)' % (self._expr, self._type)

    def __str__(self):
        return str(self._expr)

    def iterExpressions(self):
        yield self._expr

    def iterStorages(self):
        return iter(())

    def clone(self, singleRefCloner=identical, fixedValueCloner=identical):
        return fixedValueCloner(self)

    def _emitLoadBits(self, location):
        return self._expr

    def _emitStoreBits(self, value, location):
        pass

class SingleReference(Reference):
    __slots__ = ('_block', '_storage')

    block = property(lambda self: self._block)
    storage = property(lambda self: self._storage)

    def __init__(self, block, storage, typ):
        Reference.__init__(self, typ)
        self._block = block
        self._storage = checkType(storage, Storage, 'storage')

    def __repr__(self):
        return 'SingleReference(%r, %r, %r)' % (
            self._block, self._storage, self._type
            )

    def __str__(self):
        return str(self._storage)

    def iterExpressions(self):
        return self._storage.iterExpressions()

    def iterStorages(self):
        yield self._storage

    def clone(self, singleRefCloner=identical, fixedValueCloner=identical):
        return singleRefCloner(self)

    def _emitLoadBits(self, location):
        return self._block.emitLoadBits(self._storage, location)

    def _emitStoreBits(self, value, location):
        self._block.emitStoreBits(self._storage, value, location)

class ConcatenatedReference(Reference):
    __slots__ = ('_refs',)

    def __init__(self, *refs):
        '''Creates a concatenation of the given references, in order from least
        to most significant.
        '''
        width = 0
        for ref in refs:
            if width is unlimited:
                raise ValueError(
                    'unlimited width is only allowed on most significant '
                    'storage'
                    )
            checkType(ref, Reference, 'reference')
            width += ref.width
        typ = IntType(width, width != 0 and refs[-1].type.signed)
        Reference.__init__(self, typ)
        self._refs = refs

    def __repr__(self):
        return 'ConcatenatedReference(%s)' % ', '.join(
            repr(ref) for ref in self._refs
            )

    def __str__(self):
        return '(%s)' % ' ; '.join(str(ref) for ref in reversed(self._refs))

    def __iter__(self):
        return iter(self._refs)

    def iterExpressions(self):
        for ref in self._refs:
            yield from ref.iterExpressions()

    def iterStorages(self):
        for ref in self._refs:
            yield from ref.iterStorages()

    def clone(self, singleRefCloner=identical, fixedValueCloner=identical):
        changed = False
        refs = []
        for ref in self._refs:
            clone = ref.clone(singleRefCloner, fixedValueCloner)
            refs.append(clone)
            changed |= clone is not ref
        return ConcatenatedReference(*refs) if changed else self

    def _emitLoadBits(self, location):
        terms = []
        offset = 0
        for ref in self._refs:
            value = ref._emitLoadBits(location)
            terms.append(value if offset == 0 else LShift(value, offset))
            offset += ref.width
        return OrOperator(*terms)

    def _emitStoreBits(self, value, location):
        offset = 0
        for ref in self._refs:
            width = ref.width
            valueSlice = optSlice(value, offset, width)
            ref._emitStoreBits(valueSlice, location)
            offset += width

class SlicedReference(Reference):
    __slots__ = ('_ref', '_offset')

    ref = property(lambda self: self._ref)
    offset = property(lambda self: self._offset)

    def __init__(self, ref, offset, width):
        '''Creates a bitwise slice of the given reference.
        '''
        self._ref = checkType(ref, Reference, 'reference')

        offset = simplifyExpression(Expression.checkScalar(offset))
        # Some invalid offsets can only be detected upon use, but others we
        # can detect on definition and rejecting them early is likely helpful
        # towards the user.
        if isinstance(offset, IntLiteral) and offset.value < 0:
            raise ValueError('slice offset must not be negative')
        self._offset = offset

        if width is unlimited:
            typ = IntType.int
        else:
            width = simplifyExpression(Expression.checkScalar(width))
            if isinstance(width, IntLiteral):
                typ = IntType.u(width.value)
            else:
                raise ValueError('slice width cannot be determined')
        Reference.__init__(self, typ)

    def __repr__(self):
        return 'SlicedReference(%r, %r, %s)' % (
            self._ref, self._offset, self.width
            )

    def __str__(self):
        offset = self._offset
        width = self.width
        if isinstance(offset, IntLiteral):
            offsetVal = offset.value
            return '%s[%s:%s]' % (
                self._ref,
                '' if offsetVal == 0 else offsetVal,
                '' if width is unlimited else offsetVal + width
                )
        else:
            if width is unlimited:
                end = ''
            else:
                end = AddOperator(offset, IntLiteral(width))
            return '%s[%s:%s]' % (self._ref, offset, end)

    def iterExpressions(self):
        return self._ref.iterExpressions()

    def iterStorages(self):
        return self._ref.iterStorages()

    def clone(self, singleRefCloner=identical, fixedValueCloner=identical):
        ref = self._ref
        clone = ref.clone(singleRefCloner, fixedValueCloner)
        if clone is ref:
            return self
        else:
            width = self.width
            if width is not unlimited:
                width = IntLiteral(width)
            return SlicedReference(clone, self._offset, width)

    def _emitLoadBits(self, location):
        # Load value from our reference.
        value = self._ref._emitLoadBits(location)

        # Slice the loaded value.
        return truncate(RVShift(value, self._offset), self.width)

    def _emitStoreBits(self, value, location):
        offset = self._offset
        width = self.width
        valueMask = LVShift(IntLiteral(maskForWidth(width)), offset)

        # Get mask and previous value of our reference.
        ref = self._ref
        fullMask = IntLiteral(maskForWidth(ref.width))
        prevValue = ref._emitLoadBits(location)

        # Combine previous value with new value.
        maskLit = AndOperator(fullMask, XorOperator(IntLiteral(-1), valueMask))
        combined = OrOperator(
            AndOperator(prevValue, maskLit),
            LVShift(value, offset)
            )

        combined = simplifyExpression(combined)
        self._ref._emitStoreBits(combined, location)

class CodeBlock:

    def __init__(self, nodes, retRef):
        clonedNodes = []
        valueMapping = {}
        for node in nodes:
            clone = node.clone()
            clonedNodes.append(clone)
            if isinstance(node, Load):
                valueMapping[node.expr] = clone.expr
        self.nodes = clonedNodes
        self.retRef = None if retRef is None else retRef.clone(
            lambda ref, code=self: SingleReference(code, ref.storage, ref.type)
            )
        self.updateExpressions(valueMapping.get)
        assert self.verify() is None

    def verify(self):
        '''Performs consistency checks on this code block.
        Raises AssertionError if an inconsistency is found.
        '''
        # Check that every LoadedValue has an associated Load node, which must
        # execute before the LoadedValue is used.
        loads = set()
        for node in self.nodes:
            # Check that all expected loads have occurred.
            if isinstance(node, Store):
                for value in node.expr.iterInstances(LoadedValue):
                    assert value.load in loads, value
            for expr in node.storage.iterExpressions():
                for value in expr.iterInstances(LoadedValue):
                    assert value.load in loads, value
            # Remember this load.
            if isinstance(node, Load):
                loads.add(node)
        # Check I/O indices in the returned reference.
        retRef = self.retRef
        if retRef is not None:
            for storage in retRef.iterStorages():
                for expr in storage.iterExpressions():
                    for value in expr.iterInstances(LoadedValue):
                        assert value.load in loads, value

    def dump(self):
        '''Prints this code block on stdout.
        '''
        print('    nodes:')
        for node in self.nodes:
            print('        %s (%s-bit)' % (node, node.storage.width))
        if self.retRef is not None:
            if not (isinstance(self.retRef, SingleReference) and
                    isinstance(self.retRef.storage, Variable) and
                    self.retRef.storage.name == 'ret'
                    ):
                print('    ret = %s' % self.retRef)

    def _gatherExpressions(self):
        '''A set of all expressions that are contained in this block.
        Only top-level expressions are included, not all subexpressions of
        those top-level expressions.
        '''
        expressions = set()
        for node in self.nodes:
            if isinstance(node, Store):
                expressions.add(node.expr)
            expressions.update(node.storage.iterExpressions())
        retRef = self.retRef
        if retRef is not None:
            expressions.update(retRef.iterExpressions())
        return expressions

    expressions = const_property(_gatherExpressions)

    def _gatherStorages(self):
        '''A set of all storages that are accessed or referenced by this block.
        '''
        storages = set()
        for node in self.nodes:
            storages.add(node.storage)
        retRef = self.retRef
        if retRef is not None:
            storages.update(retRef.iterStorages())
        return storages

    storages = const_property(_gatherStorages)

    def updateExpressions(self, substFunc):
        '''Calls the given substitution function with each expression in this
        code block. If the substitution function returns an expression, that
        expression replaces the original expression. If the substitution
        function returns None, the original expression is kept.
        Returns True iff any substitutions were made.
        '''
        changed = False

        for node in self.nodes:
            # Update indices for I/O storages.
            storage = node.storage
            newStorage = storage.substituteExpressions(substFunc)
            if newStorage is not storage:
                changed = True
                node.storage = newStorage

            # Update node with new expression.
            if isinstance(node, Store):
                expr = node.expr
                newExpr = expr.substitute(substFunc)
                if newExpr is not expr:
                    changed = True
                    node.expr = newExpr

        # Update returned reference.
        changed |= self.updateRetRefExpressions(substFunc)

        return changed

    def updateRetRefExpressions(self, substFunc):
        '''Updates expressions in the returned reference, if any.
        See Expression.substitute() for details about the substitution function.
        Returns True iff the returned reference was updated.
        '''
        retRef = self.retRef
        if retRef is None:
            return False

        newRef = retRef.updateStorageExpressions(substFunc)
        changed = newRef is not retRef
        if changed:
            self.retRef = newRef
        return changed
