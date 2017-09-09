from .codeblock import CodeBlock
from .codeblock_builder import SemanticsCodeBlockBuilder
from .codeblock_simplifier import CodeBlockSimplifier
from .expression import IntLiteral
from .expression_simplifier import simplifyExpression
from .mode import MatchPlaceholder, ValuePlaceholder
from .reference import BitString, FixedValue, decodeInt
from .utils import checkType

from collections import OrderedDict

class MatchFiller:

    def fill(self, match, builder, args):
        values = {}
        code = match.entry.semantics.buildMatch(match, values)
        return code, values

class DecodedValueFiller:

    def __init__(self, typ):
        self._type = typ

    def fill(self, match, builder, args):
        encoded = IntLiteral(match)
        typ = self._type
        argBits = FixedValue(encoded, typ.width)
        code = CodeBlock((), (argBits,))
        value = simplifyExpression(decodeInt(encoded, typ))
        return code, value

class ComputedValueFiller:

    def __init__(self, typ, code):
        self._type = typ
        self._code = code

    def fill(self, match, builder, args):
        returned = builder.inlineBlock(self._code, args.__getitem__)
        code = CodeBlockSimplifier(builder.nodes, returned)
        code.simplify()
        valBits, = code.returned
        assert isinstance(valBits, FixedValue), valBits
        value = simplifyExpression(decodeInt(valBits.expr, self._type))
        return code, value

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

        # Instantiate fillers that will insert actual values in placeholder
        # spaces.
        self.fillers = fillers = []
        for name, placeholder in self.placeholders.items():
            if isinstance(placeholder, MatchPlaceholder):
                filler = MatchFiller()
            elif isinstance(placeholder, ValuePlaceholder):
                typ = placeholder.type
                placeholderCode = placeholder.code
                if placeholderCode is None:
                    filler = DecodedValueFiller(typ)
                else:
                    filler = ComputedValueFiller(typ, placeholderCode)
            else:
                assert False, placeholder
            fillers.append((name, filler))

    def buildMatch(self, match, values, pcVal=None):
        '''Builds a code block that defines the semantics of an EncodeMatch and
        adds the placeholder values to the given 'values' mapping. In that
        mapping, mode placeholders are represented by a nested mapping.
        Returns the code block.
        '''
        builder = SemanticsCodeBlockBuilder()
        if pcVal is not None:
            pcBits = self.pcBits
            if pcBits is not None:
                pcBits.emitStore(builder, pcVal, None)
        args = {}
        for name, filler in self.fillers:
            try:
                decoded = match[name]
            except KeyError:
                decoded = None
            code, value = filler.fill(decoded, builder, args)
            argBits, = builder.inlineBlock(code, lambda name: None)
            args[name] = argBits
            values[name] = value
        returned = builder.inlineBlock(self.code, args.__getitem__)
        code = CodeBlockSimplifier(builder.nodes, returned)
        code.simplify()
        return code
