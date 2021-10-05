from typing import AbstractSet, MutableSet, Union, cast

from .asm_formatter import Formatter
from .codeblock_builder import SemanticsCodeBlockBuilder
from .expression import IntLiteral
from .fetch import ImageFetcher
from .instrset import InstructionSet, flagsSetByCode
from .mode import ModeMatch
from .reference import FixedValue, Reference
from .types import IntType, unlimited


class Disassembler:
    def __init__(self, instrSet: InstructionSet):
        self._instrSet = instrSet
        self._decoded: dict[int, Union[Reference, ModeMatch]] = {}
        self._codeAddrs: MutableSet[int] = set()
        self._dataAddrs: MutableSet[int] = set()

    def disassemble(self, fetcher: ImageFetcher, startAddr: int) -> None:
        """Disassemble instructions from the given fetcher.
        The fetched data is assumed to be code for the given instruction set,
        to be executed at the given address.
        """
        instrSet = self._instrSet
        pc = cast(Reference, instrSet.globalNamespace["pc"])
        decodePrefix = instrSet.prefixDecodeFunc
        prefixMapping = instrSet.prefixMapping
        prefixInitCode = prefixMapping.initCode
        flagForVar = prefixMapping.flagForVar
        numBytes = fetcher.numBytes
        encWidth = instrSet.encodingWidth
        assert encWidth is not None
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
                encLen = prefix.encoding.encodedLength
                assert encLen is not None, prefix
                fetcher = fetcher.advance(encLen)
            if prefixBuilder is None:
                flags: AbstractSet[str] = frozenset()
            else:
                prefixCode = prefixBuilder.createCodeBlock(())
                flags = frozenset(
                    flagForVar[storage] for storage in flagsSetByCode(prefixCode)
                )

            # Decode instruction.
            decoder = instrSet.getDecoder(flags)
            encMatch = decoder.tryDecode(fetcher)
            encodedLength = 1 if encMatch is None else encMatch.encodedLength
            postAddr = addr + encodedLength * numBytes
            if encMatch is None:
                value = fetcher[0]
                if value is None:
                    break
                bits = FixedValue(IntLiteral(value), encWidth)
                decoded[addr] = Reference(bits, encType)
            else:
                match = ModeMatch.fromEncodeMatch(encMatch)
                decoded[addr] = match.substPC(pc, IntLiteral(postAddr))
            fetcher = fetcher.advance(encodedLength)
            addr = postAddr

    def formatAsm(self, formatter: Formatter) -> None:
        decoded = self._decoded
        codeAddrs = self._codeAddrs
        dataAddrs = self._dataAddrs
        instrSet = self._instrSet
        assert isinstance(instrSet.addrWidth, int)
        addrDigits = (instrSet.addrWidth + 3) // 4

        labels = {}
        dataLabelFormat = f"data_{{:0{addrDigits:d}x}}"
        for addr in dataAddrs:
            labels[addr] = dataLabelFormat.format(addr)
        codeLabelFormat = f"code_{{:0{addrDigits:d}x}}"
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
