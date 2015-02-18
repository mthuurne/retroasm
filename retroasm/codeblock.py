from .expression import Expression
from .storage import FixedValue, IOReference, Storage
from .types import IntType
from .utils import checkType

from collections import OrderedDict
from inspect import signature

class Constant:
    '''Definition of a local constant value.
    '''
    __slots__ = ('_cid', '_type')

    cid = property(lambda self: self._cid)
    type = property(lambda self: self._type)

    def __init__(self, cid, intType):
        self._cid = checkType(cid, int, 'constant ID')
        self._type = checkType(intType, IntType, 'constant type')

    def __repr__(self):
        return 'Constant(%d, %s)' % (self._cid, repr(self._type))

    def __str__(self):
        return '%s C%d' % (self._type, self._cid)

class ComputedConstant(Constant):
    '''A constant defined by evaluating an expression.
    '''
    __slots__ = ('_expr',)

    expr = property(lambda self: self._expr)

    def __init__(self, cid, expr):
        self._expr = Expression.checkScalar(expr)
        Constant.__init__(self, cid, expr.type)

    def __repr__(self):
        return 'ComputedConstant(%d, %s)' % (self._cid, repr(self._expr))

    def __str__(self):
        return '%s = %s' % (super().__str__(), self._expr)

class ArgumentConstant(Constant):
    '''A constant passed into a code block as an argument.
    '''
    __slots__ = ('_name',)

    name = property(lambda self: self._name)

    def __init__(self, name, cid, argType):
        self._name = checkType(name, str, 'name')
        Constant.__init__(self, cid, argType)

    def __repr__(self):
        return 'ArgumentConstant(%s, %d, %s)' % (
            repr(self._name), self._cid, repr(self.type)
            )

    def __str__(self):
        return '%s :  %s' % (super().__str__(), self._name)

class LoadedConstant(Constant):
    '''A constant defined by loading a value from a storage location.
    '''
    __slots__ = ('_rid',)

    rid = property(lambda self: self._rid)

    def __init__(self, cid, rid, refType):
        self._rid = checkType(rid, int, 'reference ID')
        Constant.__init__(self, cid, refType)

    def __repr__(self):
        return 'LoadedConstant(%d, %d, %s)' % (
            self._cid, self._rid, repr(self.type)
            )

    def __str__(self):
        return '%s <- R%s' % (super().__str__(), self._rid)

class Node:
    '''Base class for nodes.
    '''
    __slots__ = ()

class AccessNode(Node):
    '''Base class for Load and Store.
    '''
    __slots__ = ('_cid', '_rid', '_location')

    cid = property(lambda self: self._cid)
    rid = property(lambda self: self._rid)
    location = property(lambda self: self._location)

    def __init__(self, cid, rid, location=None):
        self._cid = checkType(cid, int, 'constant ID')
        self._rid = checkType(rid, int, 'reference ID')
        self._location = location

    def __repr__(self):
        return '%s(%d, %d, %s)' % (
            self.__class__.__name__, self._cid, self._rid, repr(self._location)
            )

    def clone(self, cid=None, rid=None):
        '''Create a clone of this node, with optionally a different CID or RID.
        Since nodes are immutable, there is really no point in cloning unless
        the CID or RID is overridden, but it is allowed.
        '''
        if cid is None:
            cid = self._cid
        if rid is None:
            rid = self._rid
        return self.__class__(cid, rid, self._location)

class Load(AccessNode):
    '''A node that loads a value from a storage location.
    '''
    __slots__ = ()

    def __str__(self):
        return 'load C%d from R%d' % (self._cid, self._rid)

class Store(AccessNode):
    '''A node that stores a value into a storage location.
    '''
    __slots__ = ()

    def __str__(self):
        return 'store C%d in R%d' % (self._cid, self._rid)

class ConstantValue(Expression):
    '''A synthetic constant containing an intermediate value.
    '''
    __slots__ = ('_cid',)

    cid = property(lambda self: self._cid)

    def __init__(self, cid, typ):
        Expression.__init__(self, typ)
        self._cid = cid

    def _ctorargs(self, *exprs, **kwargs):
        cls = self.__class__
        if exprs:
            raise ValueError('%s does not take expression args' % cls.__name__)
        kwargs.setdefault('cid', self._cid)
        kwargs.setdefault('typ', self.type)
        return signature(cls).bind(**kwargs)

    def __str__(self):
        return 'C%d' % self._cid

    def _equals(self, other):
        # pylint: disable=protected-access
        return self._cid is other._cid

class CodeBlock:

    def __init__(self, constants, references, nodes):
        constantsDict = OrderedDict()
        for const in constants:
            cid = const.cid
            if cid in constantsDict:
                raise ValueError('duplicate constant ID: %d' % cid)
            constantsDict[cid] = const
        self.constants = constantsDict
        self.references = OrderedDict(enumerate(references))
        self.nodes = list(nodes)
        self.retCid = None
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

        # Check that cids and rids in nodes are valid.
        for node in self.nodes:
            assert node.cid in cids, node
            assert node.rid in self.references, node

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

        # Check that loaded constants use valid rids.
        for const in self.constants.values():
            if isinstance(const, LoadedConstant):
                assert const.rid in self.references, const

        # Check that computed constants use valid subexpressions.
        def checkUsage(expr):
            assert not isinstance(expr, Storage), expr
            if isinstance(expr, ConstantValue):
                assert expr.cid in cids
        for const in self.constants.values():
            if isinstance(const, ComputedConstant):
                const.expr.substitute(checkUsage)

        # Check that cids in storage references are valid.
        for ref in self.references.values():
            if isinstance(ref, FixedValue):
                assert ref.cid in cids
            elif isinstance(ref, IOReference):
                assert ref.index.cid in cids

        # Check that the return value cid is valid.
        assert self.retCid is None or self.retCid in cids, self.retCid

    def dump(self):
        '''Prints this code block on stdout.
        '''
        print('    constants:')
        for const in self.constants.values():
            if isinstance(const, ComputedConstant):
                print('        %-4s C%-2d =  %s' % (
                    const.type, const.cid, const.expr
                    ))
            elif isinstance(const, LoadedConstant):
                print('        %-4s C%-2d <- R%d' % (
                    const.type, const.cid, const.rid
                    ))
            elif isinstance(const, ArgumentConstant):
                print('        %-4s C%-2d :  %s' % (
                    const.type, const.cid, const.name
                    ))
            else:
                assert False, const
        if self.retCid is not None:
            print('        %-4s ret =  C%d' % (
                self.constants[self.retCid].type, self.retCid
                ))
        print('    references:')
        for rid, ref in self.references.items():
            print('        %-4s R%-2d = %s' % ('%s&' % ref.type, rid, ref))
        print('    nodes:')
        for node in self.nodes:
            print('        %s' % node)
