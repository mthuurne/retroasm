from .codeblock import ArgumentConstant, Store
from .storage import IOStorage, Variable

from enum import Enum

PlaceholderRole = Enum('PlaceholderRole', ( # pylint: disable=invalid-name
    'code_addr', 'data_addr'
    ))

def determinePlaceholderRoles(semantics, placeholders):
    '''Analyze semantics to figure out the roles of placeholders.
    While this won't be sufficient to determine the role of all literal values,
    it can handle a few common cases reliably and efficiently.
    '''
    constants = semantics.constants
    storages = semantics.storages

    # Find storage ID for program counter, if any.
    pcSids = tuple(
        sid
        for sid, storage in storages.items()
        if isinstance(storage, Variable) and storage.name == 'pc'
        )
    if len(pcSids) != 0:
        pcSid, = pcSids
        # Mark placeholders written to the program counter as code addresses.
        for node in semantics.nodes:
            if isinstance(node, Store) and node.sid == pcSid:
                const = constants[node.cid]
                if isinstance(const, ArgumentConstant):
                    placeholder = placeholders[const.name]
                    placeholder.addRole(PlaceholderRole.code_addr)

    # Mark placeholders used as memory indices as data addresses.
    for sid, storage in storages.items():
        if isinstance(storage, IOStorage) and storage.channel.name == 'mem':
            const = constants[storage.index.cid]
            if isinstance(const, ArgumentConstant):
                placeholder = placeholders[const.name]
                placeholder.addRole(PlaceholderRole.data_addr)
