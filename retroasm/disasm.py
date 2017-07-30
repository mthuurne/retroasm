from .analysis import PlaceholderRole
from .codeblock_builder import SemanticsCodeBlockBuilder
from .codeblock_simplifier import CodeBlockSimplifier
from .expression import IntLiteral
from .expression_simplifier import simplifyExpression
from .mode import MatchPlaceholder, ValuePlaceholder
from .reference import FixedValue, Reference
from .types import IntType

def buildMatch(match, builder, values):
    '''Adds the semantics of an EncodeMatch to the given code block builder
    and the placeholder values to the given 'values' mapping. In that mapping,
    mode placeholders are represented by a nested mapping.
    Returns the returned bit string of the match's semantics.
    '''
    entry = match.entry

    args = {}
    for name, placeholder in entry.placeholders.items():
        if isinstance(placeholder, MatchPlaceholder):
            values[name] = subValues = {}
            args[name] = buildMatch(match[name], builder, subValues)
        elif isinstance(placeholder, ValuePlaceholder):
            typ = placeholder.type
            placeholderCode = placeholder.code
            if placeholderCode is None:
                argBits = FixedValue(IntLiteral(match[name]), typ.width)
            else:
                argBits = builder.inlineBlock(placeholderCode, args.__getitem__)
            args[name] = argBits
            code = CodeBlockSimplifier(builder.nodes, argBits)
            code.simplify()
            valBits = code.retBits
            # Note that FixedValue doesn't actually emit a Load node; the reason
            # to use Reference.emitLoad() here is to apply sign extension.
            assert isinstance(valBits, FixedValue), valBits
            valRef = Reference(valBits, typ)
            values[name] = simplifyExpression(valRef.emitLoad(builder, None))
        else:
            assert False, placeholder

    return builder.inlineBlock(entry.semantics, args.__getitem__)

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
                    value, typ, roles = subElem
                    yield value, typ, roles | mnemElem.roles
        elif isinstance(mnemElem, ValuePlaceholder):
            name = mnemElem.name
            typ = mnemElem.type
            code = mnemElem.code
            value = values[name]
            if code is None:
                # Value was decoded.
                assert isinstance(value, IntLiteral), repr(value)
                yield value.value, typ, mnemElem.roles
            else:
                # Value is computed.
                if isinstance(value, IntLiteral):
                    yield value.value, typ, mnemElem.roles
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
        pcRef = instrSet.globalNamespace['pc']
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
                builder = SemanticsCodeBlockBuilder()
                pcRef.emitStore(builder, IntLiteral(postAddr), None)
                values = {}
                buildMatch(match, builder, values)
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
            instr = decoded[addr]
            if isinstance(instr, int):
                print(formatter.formatData(instr, encType))
            else:
                print(formatter.formatMnemonic(iterMnemonic(*instr), labels))
