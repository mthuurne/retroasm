from .codeblock import CodeBlock
from .codeblock_builder import SemanticsCodeBlockBuilder
from .reference import BitString, SingleStorage
from .storage import ArgStorage
from .utils import checkType

from collections import OrderedDict

class CodeTemplate:
    '''A container for a code block which contains placeholders that will be
    filled in later.
    '''

    def __init__(self, code, placeholders, pcBits=None):
        self.code = checkType(code, CodeBlock, 'code block')
        self.placeholders = checkType(placeholders, OrderedDict, 'placeholders')
        self.pcBits = checkType(
            pcBits, (BitString, type(None)), 'program counter'
            )

    def rename(self, nameMap):
        '''Returns a new CodeTemplate, in which all placeholder names are
        substituted by their value in the given mapping.
        '''
        code = self.code
        argMap = {
            storage.name: SingleStorage(
                storage.__class__(nameMap[storage.name], storage.width)
                )
            for storage in code.storages
            if isinstance(storage, ArgStorage)
            }
        builder = SemanticsCodeBlockBuilder()
        builder.inlineBlock(code, argMap.__getitem__)
        return CodeTemplate(
            builder.createCodeBlock(()),
            OrderedDict(
                (nameMap[name], value)
                for name, value in self.placeholders.items()
                ),
            self.pcBits
            )
