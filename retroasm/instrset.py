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
        self._instructionNames = instructionNames = []

        for instr in self._entries:
            instrName = instr.mnemonic[0]
            if isinstance(instrName, str):
                instructionNames.append(instrName)

    def iterInstructionNames(self):
        return iter(self._instructionNames)
