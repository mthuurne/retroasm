from .expression import Expression
from .storage import FixedValue, IOStorage, Storage, sliceStorage
from .types import IntType, unlimited
from .utils import checkType

from collections import OrderedDict
from inspect import signature
from itertools import chain

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
        return 'ComputedConstant(%d, %s)' % (self._cid, repr(self._expr))

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
        return 'ArgumentConstant(%s, %d)' % (repr(self._name), self._cid)

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
        return '%s(%d, %d, %s)' % (
            self.__class__.__name__, self._cid, self._sid, repr(self._location)
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

class BoundReference:
    __slots__ = ('_decomposed', '_type')

    type = property(lambda self: self._type)
    width = property(lambda self: self._type.width)

    @classmethod
    def single(cls, typ, sid):
        return cls(typ, ((sid, 0, typ.width),))

    def __init__(self, typ, decomposed):
        self._type = checkType(typ, IntType, 'value type')
        self._decomposed = tuple(decomposed)
        totalWidth = 0
        for sid_, index_, width in self._decomposed:
            if totalWidth is unlimited:
                raise ValueError(
                    'unlimited width is only allowed on most significant '
                    'storage'
                    )
            totalWidth += width
        if totalWidth != typ.width:
            raise ValueError(
                'combined storages are %d bits wide, '
                'while value type is %d bits wide'
                % (totalWidth, typ.width)
                )

    def __repr__(self):
        return 'BoundReference((%s))' % ', '.join(
            repr(storageSlice) for storageSlice in self._decomposed
            )

    def __iter__(self):
        return iter(self._decomposed)

    def present(self, storages):
        return ' ; '.join(
            '%s[%s:%s]' % (
                storages[sid],
                '' if index == 0 else index,
                '' if width is unlimited else index + width
                )
            for sid, index, width in self._decomposed
            )

    def concat(self, other):
        '''Return a new BoundReference instance that is the concatenation of
        this one as the least significant part and the given BoundReference
        as the most significant part.
        '''
        assert isinstance(self.type, IntType)
        assert isinstance(other.type, IntType)
        newType = IntType(self.width + other.width)
        return self.__class__(newType, chain(self, other))

    def slice(self, index, width):
        '''Return a new BoundReference instance that is a slice of this one.
        '''
        decomposed = tuple(sliceStorage(self._decomposed, index, width))
        width = sum(width for sid_, index_, width in decomposed)
        return self.__class__(IntType(width), decomposed)

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
            if isinstance(storage, FixedValue):
                assert storage.cid in cids, storage
            elif isinstance(storage, IOStorage):
                assert storage.index.cid in cids, storage

        # Check that the return value sids are valid.
        if self.retRef is not None:
            for sid, index_, width_ in self.retRef:
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
            print('        ret = %s' % self.retRef.present(self.storages))
        print('    nodes:')
        for node in self.nodes:
            print('        %s' % node)
