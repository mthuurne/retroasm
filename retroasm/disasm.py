from .analysis import PlaceholderRole
from .mode import EncodeMatch
from .types import IntType

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
            decoded[addr] = fetcher[0] if match is None else match
            encodedLength = 1 if match is None else match.encodedLength
            fetcher = fetcher.advance(encodedLength)
            addr += encodedLength * numBytes
            if match is not None:
                match.setPC(addr)
                for mnemElem in match.iterMnemonic():
                    if not isinstance(mnemElem, str):
                        value, typ, roles = mnemElem
                        if PlaceholderRole.code_addr in roles:
                            codeAddrs.add(value)
                        if PlaceholderRole.data_addr in roles:
                            dataAddrs.add(value)

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
            if isinstance(match, EncodeMatch):
                print(formatter.formatMnemonic(match.iterMnemonic(), labels))
            else:
                print(formatter.formatData(match, encType))
