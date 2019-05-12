from .codeblock_builder import SemanticsCodeBlockBuilder
from .expression import IntLiteral
from .instrset import flagsSetByCode
from .mode import ModeMatch
from .reference import FixedValue, Reference
from .types import IntType, unlimited

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
        decodePrefix = instrSet.prefixDecodeFunc
        prefixMapping = instrSet.prefixMapping
        prefixInitCode = prefixMapping.initCode
        flagForVar = prefixMapping.flagForVar
        numBytes = fetcher.numBytes
        encWidth = instrSet.encodingWidth
        encType = IntType.int if encWidth is unlimited else IntType.u(encWidth)

        decoded = self._decoded
        codeAddrs = self._codeAddrs
        dataAddrs = self._dataAddrs
        addr = startAddr
        while fetcher[0] is not None:
            # Decode prefixes.
            prefixBuilder = None
            while True:
                prefix = decodePrefix(fetcher)
                if prefix is None:
                    break
                if prefixBuilder is None:
                    prefixBuilder = SemanticsCodeBlockBuilder()
                    prefixBuilder.inlineBlock(prefixInitCode)
                prefixBuilder.inlineBlock(prefix.semantics)
                fetcher = fetcher.advance(prefix.encoding.encodedLength)
            if prefixBuilder is None:
                flags = frozenset()
            else:
                prefixCode = prefixBuilder.createCodeBlock(())
                flags = frozenset(
                    flagForVar[storage]
                    for storage in flagsSetByCode(prefixCode)
                    )

            # Decode instruction.
            decoder = instrSet.getDecoder(flags)
            encMatch = decoder.tryDecode(fetcher)
            encodedLength = 1 if encMatch is None else encMatch.encodedLength
            postAddr = addr + encodedLength * numBytes
            if encMatch is None:
                bits = FixedValue(IntLiteral(fetcher[0]), encWidth)
                decoded[addr] = Reference(bits, encType)
            else:
                pcVal = IntLiteral(postAddr)
                match = ModeMatch.fromEncodeMatch(encMatch, pcVal)
                decoded[addr] = match
            fetcher = fetcher.advance(encodedLength)
            addr = postAddr

    def formatAsm(self, formatter):
        decoded = self._decoded
        codeAddrs = self._codeAddrs
        dataAddrs = self._dataAddrs
        instrSet = self._instrSet
        addrWidth = instrSet.addrWidth

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
            if isinstance(match, Reference):
                print(formatter.formatData(match))
            else:
                print(formatter.formatMnemonic(match.mnemonic, labels))
