from .expression import (
    AndOperator, Expression, IntLiteral, LShift, OrOperator, SignExtension,
    optSlice, truncate
    )
from .storage import IOStorage, Storage, Variable
from .types import IntType, maskForWidth, unlimited
from .utils import checkType

from collections import OrderedDict
from inspect import signature

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

class ArgumentConstant(Constant):
    '''A constant passed into a code block as an argument.
    '''
    __slots__ = ('_name',)

    name = property(lambda self: self._name)

    def __init__(self, name, cid):
        self._name = checkType(name, str, 'name')
        Constant.__init__(self, cid)

    def __repr__(self):
        return 'ArgumentConstant(%r, %d)' % (self._name, self._cid)

    def __str__(self):
        return '%s :  %s' % (super().__str__(), self._name)

class LoadedConstant(Constant):
    '''A constant defined by loading a value from a storage location.
    '''
    __slots__ = ('_sid',)

    sid = property(lambda self: self._sid)

    def __init__(self, cid, sid):
        self._sid = checkType(sid, int, 'storage ID')
        Constant.__init__(self, cid)

    def __repr__(self):
        return 'LoadedConstant(%d, %d)' % (self._cid, self._sid)

    def __str__(self):
        return '%s <- S%s' % (super().__str__(), self._sid)

class Node:
    '''Base class for nodes.
    '''
    __slots__ = ()

class AccessNode(Node):
    '''Base class for Load and Store.
    '''
    __slots__ = ('_cid', '_sid', '_location')

    cid = property(lambda self: self._cid)
    sid = property(lambda self: self._sid)
    location = property(lambda self: self._location)

    def __init__(self, cid, sid, location=None):
        self._cid = checkType(cid, int, 'constant ID')
        self._sid = checkType(sid, int, 'storage ID')
        self._location = location

    def __repr__(self):
        return '%s(%d, %d, %r)' % (
            self.__class__.__name__, self._cid, self._sid, self._location
            )

    def clone(self, cid=None, sid=None):
        '''Create a clone of this node, with optionally a different CID or SID.
        Since nodes are immutable, there is really no point in cloning unless
        the CID or SID is overridden, but it is allowed.
        '''
        if cid is None:
            cid = self._cid
        if sid is None:
            sid = self._sid
        return self.__class__(cid, sid, self._location)

class Load(AccessNode):
    '''A node that loads a value from a storage location.
    '''
    __slots__ = ()

    def __str__(self):
        return 'load C%d from S%d' % (self._cid, self._sid)

class Store(AccessNode):
    '''A node that stores a value into a storage location.
    '''
    __slots__ = ()

    def __str__(self):
        return 'store C%d in S%d' % (self._cid, self._sid)

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

    def _ctorargs(self, *exprs, **kwargs):
        cls = self.__class__
        if exprs:
            raise ValueError('%s does not take expression args' % cls.__name__)
        kwargs.setdefault('cid', self._cid)
        kwargs.setdefault('mask', self._mask)
        return signature(cls).bind(**kwargs)

    def __str__(self):
        return 'C%d' % self._cid

    def _equals(self, other):
        # pylint: disable=protected-access
        return self._cid is other._cid

def _sliceRef(decomposed, index, width):
    if index < 0:
        raise ValueError('slice index must not be negative: %d' % index)
    if width < 0:
        raise ValueError('slice width must not be negative: %d' % width)
    offset = 0
    for sid, subStart, subWidth in decomposed:
        # Clip slice indices to substorage range.
        start = max(index, offset)
        end = min(index + width, offset + subWidth)
        if start < end:
            yield sid, subStart + start - offset, end - start
        offset += subWidth

class Reference:
    '''Abstract base class for references.
    '''
    __slots__ = ('_type',)

    type = property(lambda self: self._type)
    width = property(lambda self: self._type.width)

    def __init__(self, typ):
        self._type = checkType(typ, IntType, 'value type')

    def iterSIDs(self):
        '''Iterates through the IDs of the storages accessed through this
        reference.
        '''
        raise NotImplementedError

    def clone(self, singleRefCloner):
        '''Returns a deep copy of this reference, in which each SingleReference
        is passed to the singleRefCloner function and replaced by the Reference
        returned by that function.
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
        return str(self._block.constants[self._cid])

    def iterSIDs(self):
        return iter(())

    def clone(self, singleRefCloner):
        return self

    def _emitLoadBits(self, location):
        expr = self._expr
        if expr is None:
            expr = ConstantValue(self._cid, maskForWidth(self.width))
            self._expr = expr
        return expr

    def _emitStoreBits(self, value, location):
        pass

class SingleReference(Reference):
    __slots__ = ('_block', '_sid')

    sid = property(lambda self: self._sid)

    def __init__(self, block, sid, typ):
        Reference.__init__(self, typ)
        self._block = block
        self._sid = checkType(sid, int, 'storage ID')

    def __repr__(self):
        return 'SingleReference(%r, %d, %r)' % (
            self._block, self._sid, self._type
            )

    def __str__(self):
        return str(self._block.storages[self._sid])

    def iterSIDs(self):
        yield self._sid

    def clone(self, singleRefCloner):
        return singleRefCloner(self)

    def _emitLoadBits(self, location):
        return self._block.emitLoadBits(self._sid, location)

    def _emitStoreBits(self, value, location):
        self._block.emitStoreBits(self._sid, value, location)

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

    def iterSIDs(self):
        for ref in self._refs:
            yield from ref.iterSIDs()

    def clone(self, singleRefCloner):
        return ConcatenatedReference(*(
            ref.clone(singleRefCloner) for ref in self._refs
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
        checkType(offset, int, 'slice offset')
        if offset < 0:
            raise ValueError('slice offset must not be negative: %d' % offset)

        if width is not unlimited:
            checkType(width, int, 'slice width')
        if width < 0:
            raise ValueError('slice width must not be negative: %d' % width)

        self._ref = checkType(ref, Reference, 'reference')
        self._offset = offset
        typ = IntType.int if width is unlimited else IntType.u(width)
        Reference.__init__(self, typ)

    def __repr__(self):
        return 'SlicedReference(%r, %d, %s)' % (
            self._ref, self._offset, self.width
            )

    def __str__(self):
        offset = self._offset
        width = self.width
        return '%s[%s:%s]' % (
            self._ref,
            '' if offset == 0 else offset,
            '' if width is unlimited else offset + width
            )

    def iterSIDs(self):
        return self._ref.iterSIDs()

    def clone(self, singleRefCloner):
        return SlicedReference(
            self._ref.clone(singleRefCloner), self._offset, self.width
            )

    def _emitLoadBits(self, location):
        value = self._ref._emitLoadBits(location)
        return optSlice(value, self._offset, self.width)

    def _emitStoreBits(self, value, location):
        offset = self._offset
        width = self.width
        valueMask = maskForWidth(width) << offset

        # Get mask and previous value of our reference.
        ref = self._ref
        fullMask = maskForWidth(ref.width)
        prevValue = ref._emitLoadBits(location)

        # Combine previous value with new value.
        maskLit = IntLiteral(fullMask & ~valueMask)
        combined = OrOperator(
            AndOperator(prevValue, maskLit),
            LShift(value, offset)
            )

        self._ref._emitStoreBits(combined, location)

class CodeBlock:

    def __init__(self, constants, storages, nodes):
        constantsDict = OrderedDict()
        for const in constants:
            cid = const.cid
            if cid in constantsDict:
                raise ValueError('duplicate constant ID: %d' % cid)
            constantsDict[cid] = const
        self.constants = constantsDict
        self.storages = OrderedDict(enumerate(storages))
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

        # Check that cids and sids in nodes are valid.
        for node in self.nodes:
            assert node.cid in cids, node
            assert node.sid in self.storages, node

        # Check that each loaded constant belongs to exactly one Load node.
        cidsFromLoadedConstants = set(
            cid
            for cid, const in self.constants.items()
            if isinstance(const, LoadedConstant)
            )
        cidsFromLoadNodes = set()
        for node in self.nodes:
            if isinstance(node, Load):
                cid = node.cid
                assert cid not in cidsFromLoadNodes, node
                cidsFromLoadNodes.add(cid)
        assert cidsFromLoadNodes == cidsFromLoadedConstants, (
            cidsFromLoadedConstants, cidsFromLoadNodes
            )

        # Check that loaded constants use valid sids.
        for const in self.constants.values():
            if isinstance(const, LoadedConstant):
                assert const.sid in self.storages, const

        # Check that computed constants use valid subexpressions.
        def checkUsage(expr):
            assert not isinstance(expr, Storage), expr
            if isinstance(expr, ConstantValue):
                assert expr.cid in cids, expr
        for const in self.constants.values():
            if isinstance(const, ComputedConstant):
                const.expr.substitute(checkUsage)

        # Check that cids in storages are valid.
        for storage in self.storages.values():
            if isinstance(storage, IOStorage):
                assert storage.index.cid in cids, storage

        # Check that the return value cid/sids are valid.
        if self.retRef is not None:
            for sid in self.retRef.iterSIDs():
                assert sid in self.storages, sid

    def dump(self):
        '''Prints this code block on stdout.
        '''
        print('    constants:')
        for const in self.constants.values():
            if isinstance(const, ComputedConstant):
                print('        C%-2d =  %s' % (const.cid, const.expr))
            elif isinstance(const, LoadedConstant):
                print('        C%-2d <- S%d' % (const.cid, const.sid))
            elif isinstance(const, ArgumentConstant):
                print('        C%-2d :  %s' % (const.cid, const.name))
            else:
                assert False, const
        print('    storages:')
        for sid, storage in self.storages.items():
            print('        S%-2d : %s  (%d-bit)' % (sid, storage, storage.width))
        if self.retRef is not None:
            assert isinstance(self.retRef, SingleReference), self.retRef
            storage = self.storages[self.retRef.sid]
            if not (isinstance(storage, Variable) and storage.name == 'ret'):
                print('        ret = %s' % self.retRef)
        print('    nodes:')
        for node in self.nodes:
            print('        %s' % node)
