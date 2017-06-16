from .mode import ModeTable
from .utils import const_property

class InstructionSet(ModeTable):
    '''Contains all definitions for a processor's instruction set.
    '''

    def __init__(self, encWidth, auxEncWidth, globalNamespace, instructions):
        if auxEncWidth not in (encWidth, None):
            raise ValueError(
                'Auxiliary encoding width must be None or equal to base '
                'encoding width %s, got %s instead' % (encWidth, auxEncWidth)
                )
        ModeTable.__init__(self, encWidth, auxEncWidth, instructions)
        self._globalNamespace = globalNamespace

    @property
    def addrWidth(self):
        '''The width of the program counter, in bits.
        '''
        return self._globalNamespace['pc'].width

    @const_property
    def instructionNames(self):
        '''A set containing the instruction names (operations).
        '''
        names = set()
        for instr in self._entries:
            name = instr.mnemonic[0]
            if isinstance(name, str):
                names.add(name)
        return names
