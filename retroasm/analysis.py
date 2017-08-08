from .codeblock import ArgumentValue, CodeBlock, Store
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

class CodeTemplate:
    '''A container for a code block which contains placeholders that will be
    filled in later.
    '''

    def __init__(self, code, placeholders, pcVar=None):
        self.code = checkType(code, CodeBlock, 'code block')
        self.placeholders = checkType(placeholders, OrderedDict, 'placeholders')
        self.pcVar = checkType(pcVar, (Variable, type(None)), 'program counter')

        # Perform some basic analysis.
        determinePlaceholderRoles(code, placeholders, pcVar)

    def buildMatch(self, match, builder, values):
        '''Adds the semantics of an EncodeMatch to the given code block builder
        and the placeholder values to the given 'values' mapping. In that
        mapping, mode placeholders are represented by a nested mapping.
        Returns the returned bit string of the match's semantics.
        '''
        args = {}
        for name, placeholder in self.placeholders.items():
            if isinstance(placeholder, MatchPlaceholder):
                values[name] = subValues = {}
                subMatch = match[name]
                args[name] = subMatch.entry.semantics.buildMatch(
                    subMatch, builder, subValues
                    )
            elif isinstance(placeholder, ValuePlaceholder):
                typ = placeholder.type
                placeholderCode = placeholder.code
                if placeholderCode is None:
                    argBits = FixedValue(IntLiteral(match[name]), typ.width)
                else:
                    argBits = builder.inlineBlock(
                        placeholderCode, args.__getitem__
                        )
                args[name] = argBits
                code = CodeBlockSimplifier(builder.nodes, argBits)
                code.simplify()
                valBits = code.retBits
                assert isinstance(valBits, FixedValue), valBits
                values[name] = simplifyExpression(decodeInt(valBits.expr, typ))
            else:
                assert False, placeholder

        return builder.inlineBlock(self.code, args.__getitem__)
