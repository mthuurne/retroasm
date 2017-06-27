from .codeblock import ArgumentValue, ComputedConstant, ConstantValue, Store
from .storage import IOStorage, Variable

from enum import Enum

PlaceholderRole = Enum('PlaceholderRole', ( # pylint: disable=invalid-name
    'code_addr', 'data_addr'
    ))

def inlineConstants(expr, constants):
    '''Inline all ConstantValues in the given expression.
    Constant IDs are looked up in the given constants collection.
    '''
    def subst(expr):
        if isinstance(expr, ConstantValue):
            const = constants[expr.cid]
            if isinstance(const, ComputedConstant):
                return const.expr.substitute(subst)
        return None
    return expr.substitute(subst)

def iterBranchAddrs(code):
    '''Yields the expressions written to the PC register by the given code
    block.
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
                yield inlineConstants(node.expr, code.constants)

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
    for sid, storage in semantics.storages.items():
        if isinstance(storage, IOStorage) and storage.channel.name == 'mem':
            index = inlineConstants(storage.index, semantics.constants)
            if isinstance(index, ArgumentValue):
                placeholder = placeholders[index.name]
                placeholder.addRole(PlaceholderRole.data_addr)