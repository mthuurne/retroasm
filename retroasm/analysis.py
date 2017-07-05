from .codeblock import ArgumentValue, Store
from .storage import IOStorage, Variable

from enum import Enum

PlaceholderRole = Enum('PlaceholderRole', ( # pylint: disable=invalid-name
    'code_addr', 'data_addr'
    ))

def iterBranchAddrs(code):
    '''Yields the expressions written to the PC register by the given code
    block.
    '''
    for node in code.nodes:
        if isinstance(node, Store):
            storage = node.storage
            if isinstance(storage, Variable) and storage.name == 'pc':
                yield node.expr

def determinePlaceholderRoles(semantics, placeholders):
    '''Analyze semantics to figure out the roles of placeholders.
    While this won't be sufficient to determine the role of all literal values,
    it can handle a few common cases reliably and efficiently.
    '''

    # Mark placeholders written to the program counter as code addresses.
    for expr in iterBranchAddrs(semantics):
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
