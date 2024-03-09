from __future__ import annotations

from collections import defaultdict

from .codeblock import (
    AccessNode,
    Load,
    LoadedValue,
    Store,
    update_expressions_in_bitstrings,
    update_expressions_in_nodes,
    verify_loads,
)
from .expression import Expression
from .expression_simplifier import simplify_expression
from .reference import BitString


def simplify_block(nodes: list[AccessNode], returned: list[BitString]) -> None:
    """Attempt to simplify the given code block as much as possible."""

    # Peform initial simplification of all expressions.
    # TODO: Can we move this to the builder altogether?
    update_expressions_in_nodes(nodes, simplify_expression)
    update_expressions_in_bitstrings(returned, simplify_expression)

    # With known-value loads removed, some prior stores to the same storages
    # may have become redundant as well.
    _remove_overwritten_stores(nodes)

    # Removal of unused loads will not enable any other simplifications.
    _remove_unused_loads(nodes, returned)

    assert verify_loads(nodes, returned)


def _remove_overwritten_stores(nodes: list[AccessNode]) -> None:
    """Remove side-effect-free stores that will be overwritten."""

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
                    else:
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
