from __future__ import annotations

from collections import OrderedDict
from typing import TYPE_CHECKING, Mapping, Optional

from .codeblock import CodeBlock
from .codeblock_builder import SemanticsCodeBlockBuilder
from .reference import BitString, SingleStorage
from .storage import ArgStorage
from .utils import checkType

if TYPE_CHECKING:
    from .mode import Placeholder, ModeEntry


class CodeTemplate:
    '''A container for a code block which contains placeholders that will be
    filled in later.
    '''

    def __init__(self,
                 code: Optional[CodeBlock],
                 placeholders: OrderedDict[str, Placeholder],
                 pcBits: BitString
                 ):
        self.code = checkType(code, (CodeBlock, type(None)), 'code block')
        self.placeholders = placeholders
        self.pcBits = checkType(pcBits, BitString, 'program counter')

    def fillPlaceholder(self, name: str, entry: ModeEntry) -> CodeTemplate:
        '''Returns a new CodeTemplate, which is a copy of this one but with
        the match placeholder of the given name replaced by the semantics
        of the given mode entry.
        '''
        placeholders = self.placeholders.copy()
        placeholders.pop(name)

        code = self.code
        if code is None:
            newCode = None
        else:
            fillCode = entry.semantics.code
            # TODO: Verify that this is indeed never called without fillCode.
            assert fillCode is not None
            # TODO: Support fillCode semantics with side effects.
            assert len(fillCode.nodes) == 0, entry

            def argFetcher(argName: str) -> Optional[BitString]:
                # https://github.com/python/mypy/issues/2608
                assert fillCode is not None
                return fillCode.returned[0] if argName == name else None
            builder = SemanticsCodeBlockBuilder()
            returned = builder.inlineBlock(code, argFetcher)
            newCode = builder.createCodeBlock(returned)

        return CodeTemplate(newCode, placeholders, self.pcBits)

    def rename(self, nameMap: Mapping[str, str]) -> CodeTemplate:
        '''Returns a new CodeTemplate, in which all placeholder names are
        substituted by their value in the given mapping.
        '''
        code = self.code
        if code is None:
            newCode = None
        else:
            argMap = {
                storage.name: SingleStorage(
                    storage.__class__(nameMap[storage.name], storage.width)
                    )
                for storage in code.storages
                if isinstance(storage, ArgStorage)
                }
            builder = SemanticsCodeBlockBuilder()
            builder.inlineBlock(code, argMap.__getitem__)
            newCode = builder.createCodeBlock(())

        return CodeTemplate(
            newCode,
            OrderedDict(
                (nameMap[name], value)
                for name, value in self.placeholders.items()
                ),
            self.pcBits
            )
