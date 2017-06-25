from .codeblock import ArgumentConstant, Store
from .storage import IOStorage, Variable

from enum import Enum

PlaceholderRole = Enum('PlaceholderRole', ( # pylint: disable=invalid-name
    'code_addr', 'data_addr'
    ))

def iterBranchAddrs(code):
    '''Yields the constants written to the PC register by the given code block.
    '''
    pcSids = tuple(
        sid
        for sid, storage in code.storages.items()
        if isinstance(storage, Variable) and storage.name == 'pc'
        )
    if len(pcSids) != 0:
        pcSid, = pcSids
        for node in code.nodes:
            if isinstance(node, Store) and node.sid == pcSid:
                yield code.constants[node.cid]

def determinePlaceholderRoles(semantics, placeholders):
    '''Analyze semantics to figure out the roles of placeholders.
    While this won't be sufficient to determine the role of all literal values,
    it can handle a few common cases reliably and efficiently.
    '''

    # Mark placeholders written to the program counter as code addresses.
    for const in iterBranchAddrs(semantics):
        if isinstance(const, ArgumentConstant):
            placeholder = placeholders[const.name]
            placeholder.addRole(PlaceholderRole.code_addr)

    # Mark placeholders used as memory indices as data addresses.
    for sid, storage in semantics.storages.items():
        if isinstance(storage, IOStorage) and storage.channel.name == 'mem':
            const = semantics.constants[storage.index.cid]
            if isinstance(const, ArgumentConstant):
                placeholder = placeholders[const.name]
                placeholder.addRole(PlaceholderRole.data_addr)
