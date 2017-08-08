from .codeblock import ArgumentValue, CodeBlock, Store
from .codeblock_builder import SemanticsCodeBlockBuilder
from .codeblock_simplifier import CodeBlockSimplifier
from .expression import IntLiteral
from .expression_simplifier import simplifyExpression
from .mode import MatchPlaceholder, PlaceholderRole, ValuePlaceholder
from .reference import FixedValue, decodeInt
from .storage import IOStorage, Variable
from .utils import checkType

from collections import OrderedDict

def iterBranchAddrs(code, pc):
    '''Yields the expressions written to the PC register by the given code
    block.
    '''
    for node in code.nodes:
        if isinstance(node, Store) and node.storage is pc:
            yield node.expr

def determinePlaceholderRoles(semantics, placeholders, pc):
    '''Analyze semantics to figure out the roles of placeholders.
    While this won't be sufficient to determine the role of all literal values,
    it can handle a few common cases reliably and efficiently.
    '''

    if pc is not None:
        # Mark placeholders written to the program counter as code addresses.
        for expr in iterBranchAddrs(semantics, pc):
            if isinstance(expr, ArgumentValue):
                placeholder = placeholders[expr.name]
                placeholder.addRole(PlaceholderRole.code_addr)

    # Mark placeholders used as memory indices as data addresses.
    for node in semantics.nodes:
        storage = node.storage
        if isinstance(storage, IOStorage) and storage.channel.name == 'mem':
            index = storage.index
            if isinstance(index, ArgumentValue):
                placeholder = placeholders[index.name]
                placeholder.addRole(PlaceholderRole.data_addr)

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
        code = CodeBlock((), argBits)
        value = simplifyExpression(decodeInt(encoded, typ))
        return code, value

class ComputedValueFiller:

    def __init__(self, typ, code):
        self._type = typ
        self._code = code

    def fill(self, match, builder, args):
        argBits = builder.inlineBlock(self._code, args.__getitem__)
        code = CodeBlockSimplifier(builder.nodes, argBits)
        code.simplify()
        valBits = code.retBits
        assert isinstance(valBits, FixedValue), valBits
        value = simplifyExpression(decodeInt(valBits.expr, self._type))
        return code, value

class CodeTemplate:
    '''A container for a code block which contains placeholders that will be
    filled in later.
    '''

    def __init__(self, code, placeholders, pcVar=None):
        self.code = checkType(code, CodeBlock, 'code block')
        self.placeholders = checkType(placeholders, OrderedDict, 'placeholders')
        self.pcVar = checkType(pcVar, (Variable, type(None)), 'program counter')

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

        # Perform some basic analysis.
        determinePlaceholderRoles(code, placeholders, pcVar)

    def buildMatch(self, match, values, pcVal=None):
        '''Builds a code block that defines the semantics of an EncodeMatch and
        adds the placeholder values to the given 'values' mapping. In that
        mapping, mode placeholders are represented by a nested mapping.
        Returns the code block.
        '''
        builder = SemanticsCodeBlockBuilder()
        if pcVal is not None:
            pcVar = self.pcVar
            if pcVar is not None:
                builder.emitStoreBits(pcVar, pcVal, None)
        args = {}
        for name, filler in self.fillers:
            try:
                decoded = match[name]
            except KeyError:
                decoded = None
            code, value = filler.fill(decoded, builder, args)
            argBits = builder.inlineBlock(code, lambda name: None)
            args[name] = argBits
            values[name] = value
        retBits = builder.inlineBlock(self.code, args.__getitem__)
        code = CodeBlockSimplifier(builder.nodes, retBits)
        code.simplify()
        return code
