from .expression import IntLiteral
from .mode import MatchPlaceholder, PlaceholderRole, ValuePlaceholder
from .types import IntType, unlimited

def iterMnemonic(match, values):
    '''Yields a mnemonic representation of the given match.
    '''
    for mnemElem in match.entry.mnemonic:
        if isinstance(mnemElem, str):
            yield mnemElem
        elif isinstance(mnemElem, int):
            yield mnemElem, IntType.int, frozenset()
        elif isinstance(mnemElem, MatchPlaceholder):
            name = mnemElem.name
            for subElem in iterMnemonic(match[name], values[name]):
                if isinstance(subElem, str):
                    yield subElem
                else:
                    value, typ, subRoles = subElem
                    roles = frozenset() # TODO: Re-implement.
                    yield value, typ, subRoles | roles
        elif isinstance(mnemElem, ValuePlaceholder):
            name = mnemElem.name
            typ = mnemElem.type
            code = mnemElem.code
            value = values[name]
            if code is None:
                # Value was decoded.
                assert isinstance(value, IntLiteral), repr(value)
                roles = frozenset() # TODO: Re-implement.
                yield value.value, typ, roles
            else:
                # Value is computed.
                if isinstance(value, IntLiteral):
                    roles = frozenset() # TODO: Re-implement.
                    yield value.value, typ, roles
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
        instrSet = self._instrSet
        decoder = instrSet.decoder
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
                values = {}
                pcVal = IntLiteral(postAddr)
                match.entry.semantics.buildMatch(match, values, pcVal)
                decoded[addr] = instr = (match, values)
                for mnemElem in iterMnemonic(*instr):
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
        encWidth = instrSet.encodingWidth
        encType = IntType.int if encWidth is unlimited else IntType.u(encWidth)

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
            instr = decoded[addr]
            if isinstance(instr, int):
                print(formatter.formatData(instr, encType))
            else:
                print(formatter.formatMnemonic(iterMnemonic(*instr), labels))
