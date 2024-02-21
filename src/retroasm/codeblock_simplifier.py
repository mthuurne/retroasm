from __future__ import annotations

from collections import defaultdict

from .codeblock import (
    AccessNode,
    Load,
    LoadedValue,
    Store,
    update_expressions_in_block,
    verify_loads,
)
from .expression import Expression
from .expression_simplifier import simplify_expression
from .reference import BitString
from .storage import Storage


def simplify_block(nodes: list[AccessNode], returned: list[BitString]) -> None:
    """Attempt to simplify the given code block as much as possible."""

    # Peform initial simplification of all expressions.
    # This allows remove_redundant_nodes() to only simplify expressions when
    # it changes them.
    update_expressions_in_block(nodes, returned, simplify_expression)

    # This mainly collapses incremental updates to variables.
    _remove_redundant_nodes(nodes, returned)

    # Removal of unused stores might make some loads unused.
    _remove_unused_stores(nodes)

    # Removal of unused loads will not enable any other simplifications.
    _remove_unused_loads(nodes, returned)

    assert verify_loads(nodes, returned)


def _remove_redundant_nodes(nodes: list[AccessNode], returned: list[BitString]) -> None:
    load_replacements: dict[Expression, Expression] = {}

    def replace_loaded_values(expr: Expression) -> Expression:
        new_expr = expr.substitute(load_replacements.get)
        if new_expr is not expr:
            new_expr = simplify_expression(new_expr)
        return new_expr

    # Remove redundant loads and stores by keeping track of the current
    # value of storages.
    current_values: dict[Storage, Expression] = {}
    i = 0
    while i < len(nodes):
        node = nodes[i]
        storage = node.storage

        # Apply load replacements to storage.
        if load_replacements:
            new_storage = storage.substitute_expressions(replace_loaded_values)
            if new_storage is not storage:
                node.storage = storage = new_storage

        value = current_values.get(storage)
        match node:
            case Load(expr=expr):
                if value is not None:
                    # Use known value instead of loading it.
                    load_replacements[expr] = value
                    if not storage.can_load_have_side_effect():
                        del nodes[i]
                        continue
                elif storage.is_load_consistent():
                    # Remember loaded value.
                    current_values[storage] = expr
            case Store(expr=expr):
                # Apply load replacements to stored expression.
                if load_replacements:
                    new_expr = expr.substitute(replace_loaded_values)
                    if new_expr is not expr:
                        node.expr = expr = new_expr

                if value == expr:
                    # Current value is rewritten.
                    if not storage.can_store_have_side_effect():
                        del nodes[i]
                        continue
                elif storage.is_sticky():
                    # Remember stored value.
                    current_values[storage] = expr

                # Remove values for storages that might be aliases.
                for storage2 in list(current_values.keys()):
                    if storage != storage2 and storage.might_be_same(storage2):
                        # However, if the store wouldn't alter the value,
                        # there is no need to remove it.
                        if current_values[storage2] != expr:
                            del current_values[storage2]
        i += 1

    # Apply load replacements in returned bit strings.
    for i, ret_bits in enumerate(returned):
        new_bits = ret_bits.substitute(expression_func=replace_loaded_values)
        if new_bits is not ret_bits:
            returned[i] = new_bits


def _remove_unused_stores(nodes: list[AccessNode]) -> None:
    """
    Remove side-effect-free stores that will be overwritten or that
    write a variable that will go out of scope.
    """

    # Remove stores for which the value is overwritten before it is loaded.
    # Local variable loads were already eliminated by remove_redundant_nodes()
    # and since variables cease to exist at the end of a block, all local
    # variable stores are at this point considered redundant, unless the
    # variable is part of the returned bit string.
    will_be_overwritten = set()
    for i in range(len(nodes) - 1, -1, -1):
        node = nodes[i]
        storage = node.storage
        if not storage.can_store_have_side_effect():
            match node:
                case Load():
                    will_be_overwritten.discard(storage)
                case Store():
                    if storage in will_be_overwritten:
                        del nodes[i]
                    will_be_overwritten.add(storage)


def _remove_unused_loads(nodes: list[AccessNode], returned: list[BitString]) -> None:
    """Remove side-effect-free loads of which the LoadedValue is unused."""

    # Keep track of how often each LoadedValue is used.
    use_counts = defaultdict[LoadedValue, int](int)

    def update_counts(expr: Expression, delta: int = 1) -> None:
        for loaded in expr.iter_instances(LoadedValue):
            use_counts[loaded] += delta

    # Compute initial use counts.
    for node in nodes:
        if isinstance(node, Store):
            update_counts(node.expr)
        for expr in node.storage.iter_expressions():
            update_counts(expr)
    for ret_bits in returned:
        for expr in ret_bits.iter_expressions():
            update_counts(expr)

    # Remove unnecesary Loads.
    for i in range(len(nodes) - 1, -1, -1):
        match nodes[i]:
            case Load(expr=expr, storage=storage):
                if use_counts[expr] == 0 and not storage.can_load_have_side_effect():
                    del nodes[i]
                    # Update use_counts, so we can remove earlier Loads that
                    # became unused because the Load we just removed was
                    # the sole user of their LoadedValue.
                    for expr in storage.iter_expressions():
                        update_counts(expr, -1)
