from .mode import ModeTable

class InstructionSet(ModeTable):
    '''Contains all definitions for a processor's instruction set.
    '''

    def __init__(self, instructions):
        ModeTable.__init__(self)
        self._instructionNames = []

        for instr in instructions:
            self.addEntry(instr)

    def addEntry(self, entry):
        super().addEntry(entry)

        instrName = entry.mnemonic[0]
        if isinstance(instrName, str):
            self._instructionNames.append(instrName)

    def iterInstructionNames(self):
        return iter(self._instructionNames)
