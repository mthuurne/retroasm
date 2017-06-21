from .mode import EncodeMatch, PlaceholderRole
from .types import IntType

def disassemble(instrSet, fetcher, startAddr, formatter):
    '''Disassemble instructions from the given fetcher.
    The fetched data is assumed to be code for the given instruction set,
    to be executed at the given address.
    '''
    decoder = instrSet.decoder
    numBytes = fetcher.numBytes
    encType = IntType.u(numBytes * 8)

    decoded = {}
    codeAddrs = set()
    dataAddrs = set()
    addr = startAddr
    while fetcher[0] is not None:
        match = decoder.tryDecode(fetcher)
        decoded[addr] = fetcher[0] if match is None else match
        encodedLength = 1 if match is None else match.encodedLength
        fetcher = fetcher.advance(encodedLength)
        addr += encodedLength * numBytes
        if match is not None:
            for mnemElem in match.iterMnemonic(addr):
                if not isinstance(mnemElem, str):
                    value, typ, roles = mnemElem
                    if PlaceholderRole.code_addr in roles:
                        codeAddrs.add(value)
                    if PlaceholderRole.data_addr in roles:
                        dataAddrs.add(value)

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
        if isinstance(match, EncodeMatch):
            print(formatter.formatMnemonic(
                match.iterMnemonic(addr + match.encodedLength * numBytes),
                labels
                ))
        else:
            print(formatter.formatData(match, encType))
