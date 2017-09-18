from .codeblock import CodeBlock
from .reference import BitString
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
