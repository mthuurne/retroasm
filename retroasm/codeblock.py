from .expression import (
    AddOperator, AndOperator, Expression, IntLiteral, LShift, LVShift,
    OrOperator, RVShift, SignExtension, XorOperator, optSlice, truncate
    )
from .expression_simplifier import simplifyExpression
from .storage import IOStorage, Storage, Variable
from .types import IntType, maskForWidth, unlimited
from .utils import checkType

from collections import OrderedDict

class Constant:
    '''Definition of a local constant value.
    '''
    __slots__ = ('_cid',)

    cid = property(lambda self: self._cid)

    def __init__(self, cid):
        self._cid = checkType(cid, int, 'constant ID')

    def __repr__(self):
        return 'Constant(%d)' % self._cid

    def __str__(self):
        return 'int C%d' % self._cid

class ComputedConstant(Constant):
    '''A constant defined by evaluating an expression.
    '''
    __slots__ = ('_expr',)

    expr = property(lambda self: self._expr)

    def __init__(self, cid, expr):
        self._expr = Expression.checkScalar(expr)
        Constant.__init__(self, cid)

    def __repr__(self):
        return 'ComputedConstant(%d, %r)' % (self._cid, self._expr)

    def __str__(self):
        return '%s = %s' % (super().__str__(), self._expr)

class LoadedConstant(Constant):
    '''A constant defined by loading a value from a storage location.
    '''
    __slots__ = ('_storage',)

    storage = property(lambda self: self._storage)

    def __init__(self, cid, storage):
        self._storage = checkType(storage, Storage, 'storage')
        Constant.__init__(self, cid)

    def __repr__(self):
        return 'LoadedConstant(%d, %r)' % (self._cid, self._storage)

    def __str__(self):
        return '%s <- %s' % (super().__str__(), self._storage)

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

    def __init__(self, expr, storage, location=None):
        self._expr = checkType(expr, ConstantValue, 'expression')
        self._storage = checkType(storage, Storage, 'storage')
        self._location = location

    def __repr__(self):
        return '%s(%r, %r, %r)' % (
            self.__class__.__name__, self._expr, self._storage, self._location
            )

    def clone(self, expr=None, storage=None):
        '''Create a clone of this node, with optionally a different expression
        or SID. Since nodes are immutable, there is really no point in cloning
        unless the expression or SID is overridden, but it is allowed.
        '''
        if expr is None:
            expr = self._expr
        if storage is None:
            storage = self._storage
        return self.__class__(expr, storage, self._location)

class Load(AccessNode):
    '''A node that loads a value from a storage location.
    '''
    __slots__ = ()

    def __str__(self):
        return 'load %s from %s' % (self._expr, self._storage)

class Store(AccessNode):
    '''A node that stores a value into a storage location.
    '''
    __slots__ = ()

    def __str__(self):
        return 'store %s in %s' % (self._expr, self._storage)

class ConstantValue(Expression):
    '''A synthetic constant containing an intermediate value.
    '''
    __slots__ = ('_cid', '_mask')

    cid = property(lambda self: self._cid)
    mask = property(lambda self: self._mask)

    def __init__(self, cid, mask):
        Expression.__init__(self)
        self._cid = cid
        self._mask = mask

    def _ctorargs(self):
        return self._cid, self._mask

    def __str__(self):
        return 'C%d' % self._cid

    def _equals(self, other):
        # pylint: disable=protected-access
        return self._cid is other._cid

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

class Reference:
    '''Abstract base class for references.
    '''
    __slots__ = ('_type',)

    type = property(lambda self: self._type)
    width = property(lambda self: self._type.width)

    def __init__(self, typ):
        self._type = checkType(typ, IntType, 'value type')

    def iterStorages(self):
        '''Iterates through the storages accessed through this reference.
        '''
        raise NotImplementedError

    def clone(self, singleRefCloner, fixedValueCloner):
        '''Returns a deep copy of this reference, in which each SingleReference
        is passed to the singleRefCloner function and replaced by the Reference
        returned by that function, as well as each FixedValue passed to the
        fixedValueCloner function and replaced by its return value.
        '''
        raise NotImplementedError

    def emitLoad(self, location):
        '''Emits constants and load operations for loading a typed value from
        the referenced storage(s).
        Returns the value as an Expression.
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
        '''Emits constants and load operations for loading a bit string from
        the referenced storage(s).
        Returns the value of the bit string as an Expression.
        '''
        raise NotImplementedError

    def emitStore(self, value, location):
        '''Emits constants and store operations for storing a value into the
        referenced storage(s).
        '''
        self._emitStoreBits(truncate(value, self.width), location)

    def _emitStoreBits(self, value, location):
        '''Emits constants and store operations for storing a bit string into
        the referenced storage(s).
        '''
        raise NotImplementedError

class FixedValue(Reference):
    '''A reference that always reads as the same value and ignores writes.
    '''
    __slots__ = ('_block', '_cid', '_expr')

    cid = property(lambda self: self._cid)
    const = property(lambda self: self._block.constants[self._cid])

    def __init__(self, block, cid, typ):
        Reference.__init__(self, typ)
        self._block = block
        self._cid = checkType(cid, int, 'constant ID')
        self._expr = None

    def __repr__(self):
        return 'FixedValue(%r, %d, %r)' % (
            self._block, self._cid, self._type
            )

    def __str__(self):
        return str(self.const)

    def iterStorages(self):
        return iter(())

    def clone(self, singleRefCloner, fixedValueCloner):
        return fixedValueCloner(self)

    def _emitLoadBits(self, location):
        expr = self._expr
        if expr is None:
            expr = ConstantValue(self._cid, maskForWidth(self.width))
            self._expr = expr
        return expr

    def _emitStoreBits(self, value, location):
        pass

class SingleReference(Reference):
    __slots__ = ('_block', '_storage')

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

    def iterStorages(self):
        yield self._storage

    def clone(self, singleRefCloner, fixedValueCloner):
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

    def iterStorages(self):
        for ref in self._refs:
            yield from ref.iterStorages()

    def clone(self, singleRefCloner, fixedValueCloner):
        return ConcatenatedReference(*(
            ref.clone(singleRefCloner, fixedValueCloner) for ref in self._refs
            ))

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

    def iterStorages(self):
        return self._ref.iterStorages()

    def clone(self, singleRefCloner, fixedValueCloner):
        return SlicedReference(
            self._ref.clone(singleRefCloner, fixedValueCloner),
            self._offset, self.width
            )

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

    def __init__(self, constants, nodes):
        constantsDict = OrderedDict()
        for const in constants:
            cid = const.cid
            if cid in constantsDict:
                raise ValueError('duplicate constant ID: %d' % cid)
            constantsDict[cid] = const
        self.constants = constantsDict
        self.nodes = list(nodes)
        self.retRef = None
        assert self.verify() is None

    def verify(self):
        '''Performs consistency checks on the data in this code block.
        Raises AssertionError if an inconsistency is found.
        '''
        # Check that cid keys match the value's cid.
        for cid, const in self.constants.items():
            assert isinstance(const, Constant), const
            assert const.cid == cid, const
        cids = self.constants.keys()

        # Check that cids in nodes are valid.
        for node in self.nodes:
            for value in node.expr.iterInstances(ConstantValue):
                assert value.cid in cids, node

        # Check that each loaded constant belongs to exactly one Load node.
        cidsFromLoadedConstants = set(
            cid
            for cid, const in self.constants.items()
            if isinstance(const, LoadedConstant)
            )
        cidsFromLoadNodes = set()
        for node in self.nodes:
            if isinstance(node, Load):
                for value in node.expr.iterInstances(ConstantValue):
                    cid = value.cid
                    assert cid not in cidsFromLoadNodes, node
                    cidsFromLoadNodes.add(cid)
        assert cidsFromLoadNodes == cidsFromLoadedConstants, (
            cidsFromLoadedConstants, cidsFromLoadNodes
            )

        # Check that computed constants use valid subexpressions.
        for const in self.constants.values():
            if isinstance(const, ComputedConstant):
                for value in const.expr.iterInstances(ConstantValue):
                    assert value.cid in cids, value

        # Check that the return value cids are valid.
        retRef = self.retRef
        if retRef is not None:
            for storage in retRef.iterStorages():
                if isinstance(storage, IOStorage):
                    for value in storage.index.iterInstances(ConstantValue):
                        assert value.cid in cids, storage

    def dump(self):
        '''Prints this code block on stdout.
        '''
        print('    constants:')
        for const in self.constants.values():
            if isinstance(const, ComputedConstant):
                print('        C%-2d =  %s' % (const.cid, const.expr))
            elif isinstance(const, LoadedConstant):
                print('        C%-2d <- %s' % (const.cid, const.storage))
            else:
                assert False, const
        if self.retRef is not None:
            if not (isinstance(self.retRef, SingleReference) and
                    isinstance(self.retRef.storage, Variable) and
                    self.retRef.storage.name == 'ret'
                    ):
                print('    ret = %s' % self.retRef)
        print('    nodes:')
        for node in self.nodes:
            print('        %s (%s-bit)' % (node, node.storage.width))
