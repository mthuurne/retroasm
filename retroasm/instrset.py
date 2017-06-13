from .mode import ModeTable

class InstructionSet(ModeTable):
    '''Contains all definitions for a processor's instruction set.
    '''

    def __init__(self, encWidth, auxEncWidth, instructions):
        if auxEncWidth not in (encWidth, None):
            raise ValueError(
                'Auxiliary encoding width must be None or equal to base '
                'encoding width %s, got %s instead' % (encWidth, auxEncWidth)
                )
        ModeTable.__init__(self, encWidth, auxEncWidth, instructions)
        self._instructionNames = None

    @property
    def instructionNames(self):
        '''A set containing the instruction names (operations).
        '''
        names = self._instructionNames
        if names is None:
            names = set()
            for instr in self._entries:
                name = instr.mnemonic[0]
                if isinstance(name, str):
                    names.add(name)
            self._instructionNames = frozenset(names)
        return names
