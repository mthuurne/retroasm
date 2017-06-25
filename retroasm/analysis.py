from .codeblock import ArgumentConstant, Store
from .storage import Variable

from enum import Enum

PlaceholderRole = Enum('PlaceholderRole', ( # pylint: disable=invalid-name
    'code_addr', 'data_addr'
    ))

def determinePlaceholderRoles(semantics, placeholders):
    '''Analyze semantics to figure out the roles of placeholders.
    While this won't be sufficient to determine the role of all literal values,
    it can handle a few common cases reliably and efficiently.
    '''

    # Find storage ID for program counter, if any.
    pcSids = tuple(
        sid
        for sid, storage in semantics.storages.items()
        if isinstance(storage, Variable) and storage.name == 'pc'
        )
    if len(pcSids) != 0:
        pcSid, = pcSids
        # Mark placeholders written to the program counter as code addresses.
        for node in semantics.nodes:
            if isinstance(node, Store) and node.sid == pcSid:
                const = semantics.constants[node.cid]
                if isinstance(const, ArgumentConstant):
                    placeholder = placeholders[const.name]
                    placeholder.addRole(PlaceholderRole.code_addr)
