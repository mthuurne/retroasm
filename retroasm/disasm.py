def disassemble(instrSet, image, offset, addr, size):
    '''Disassemble the bytes in the given image with the given offset and size.
    The bytes are assumed to be code for the given instruction set, executed at
    the given address.
    '''
    print('Disasm from %04X (%04X)' % (addr, offset))
