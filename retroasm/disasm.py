from .analysis import PlaceholderRole
from .codeblock import ArgumentValue, FixedValue, LoadedValue
from .expression import IntLiteral
from .expression_simplifier import simplifyExpression
from .mode import EncodeMatch, MatchPlaceholder, ValuePlaceholder
from .storage import Variable
from .types import IntType
from .utils import checkType

class InstructionMatch:
    '''A decoded instruction at a particular address.
    '''

    def __init__(self, encodeMatch, pcAddr):
        self._match = checkType(encodeMatch, EncodeMatch, 'encode match')
        self._pcAddr = checkType(pcAddr, int, 'program counter')

    def __repr__(self):
        return 'InstructionMatch(%r, 0x%x)' % (self._match, self._pcAddr)

    def _substMapping(self, expr):
        if isinstance(expr, ArgumentValue):
            return IntLiteral(self._match[expr.name])
        if isinstance(expr, LoadedValue):
            storage = expr.load.storage
            if isinstance(storage, Variable) and storage.name == 'pc':
                return IntLiteral(self._pcAddr)
        return None

    def iterMnemonic(self, match=None):
        '''Yields a mnemonic representation of this match.
        '''
        if match is None:
            match = self._match
        entry = match.entry
        for mnemElem in entry.mnemonic:
            if isinstance(mnemElem, str):
                yield mnemElem
            elif isinstance(mnemElem, int):
                yield mnemElem, IntType.int, frozenset()
            elif isinstance(mnemElem, MatchPlaceholder):
                subMatch = match[mnemElem.name]
                for subElem in self.iterMnemonic(subMatch):
                    if isinstance(subElem, str):
                        yield subElem
                    else:
                        value, typ, roles = subElem
                        yield value, typ, roles | mnemElem.roles
            elif isinstance(mnemElem, ValuePlaceholder):
                name = mnemElem.name
                typ = mnemElem.type
                code = mnemElem.code
                if code is None:
                    # Value was decoded.
                    value = match[name]
                    if typ.signed:
                        width = typ.width
                        if value >= 1 << (width - 1):
                            value -= 1 << width
                    yield value, typ, mnemElem.roles
                else:
                    # Value is computed.
                    ref = mnemElem.code.retRef
                    # TODO: While the documentation says we do support
                    #       defining references in the context, the
                    #       parse code rejects "<type>&".
                    assert isinstance(ref, FixedValue), ref
                    expr = simplifyExpression(
                        ref.expr.substitute(self._substMapping)
                        )
                    if isinstance(expr, IntLiteral):
                        yield expr.value, typ, mnemElem.roles
                    else:
                        # TODO: Is this a bug? A definition error?
                        #       Or can it happen normally?
                        yield name
            else:
                assert False, mnemElem

class Disassembler:

    def __init__(self, instrSet):
        self._instrSet = instrSet
        self._decoded = {}
        self._codeAddrs = set()
        self._dataAddrs = set()

    def disassemble(self, fetcher, startAddr):
        '''Disassemble instructions from the given fetcher.
        The fetched data is assumed to be code for the given instruction set,
        to be executed at the given address.
        '''
        decoder = self._instrSet.decoder
        numBytes = fetcher.numBytes

        decoded = self._decoded
        codeAddrs = self._codeAddrs
        dataAddrs = self._dataAddrs
        addr = startAddr
        while fetcher[0] is not None:
            # TODO: Implement prefix support.
            flags = frozenset()
            match = decoder.tryDecode(fetcher, flags)
            encodedLength = 1 if match is None else match.encodedLength
            postAddr = addr + encodedLength * numBytes
            if match is None:
                decoded[addr] = fetcher[0]
            else:
                decoded[addr] = instr = InstructionMatch(match, postAddr)
                for mnemElem in instr.iterMnemonic():
                    if not isinstance(mnemElem, str):
                        value, typ_, roles = mnemElem
                        if PlaceholderRole.code_addr in roles:
                            codeAddrs.add(value)
                        if PlaceholderRole.data_addr in roles:
                            dataAddrs.add(value)
            fetcher = fetcher.advance(encodedLength)
            addr = postAddr

    def formatAsm(self, formatter):
        decoded = self._decoded
        codeAddrs = self._codeAddrs
        dataAddrs = self._dataAddrs
        instrSet = self._instrSet

        addrWidth = instrSet.addrWidth
        encType = IntType.u(instrSet.encodingWidth)

        labels = {}
        dataLabelFormat = 'data_{:0%dx}' % ((addrWidth + 3) // 4)
        for addr in dataAddrs:
            labels[addr] = dataLabelFormat.format(addr)
        codeLabelFormat = 'code_{:0%dx}' % ((addrWidth + 3) // 4)
        for addr in codeAddrs:
            labels[addr] = codeLabelFormat.format(addr)

        for addr in sorted(decoded.keys()):
            label = labels.get(addr)
            if label is not None:
                print(formatter.formatLabel(label))
            match = decoded[addr]
            if isinstance(match, InstructionMatch):
                print(formatter.formatMnemonic(match.iterMnemonic(), labels))
            else:
                print(formatter.formatData(match, encType))
