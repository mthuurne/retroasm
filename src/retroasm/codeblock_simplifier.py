from __future__ import annotations

from collections import defaultdict

from .codeblock import (
    AccessNode,
    Load,
    LoadedValue,
    Store,
    verify_loads,
)
from .expression import Expression
from .expression_simplifier import simplify_expression
from .reference import BitString


def simplify_block(nodes: list[AccessNode], returned: list[BitString]) -> None:
    """Attempt to simplify the given code block as much as possible."""

    # Simplify returned expressions.
    # This can also help find additional unused loads, if a loaded value is dropped
    # during simplification because it doesn't affect the expression's value.
    _update_expressions_in_bitstrings(returned)

    # With known-value loads removed by the builder, some prior stores to the same
    # storages may have become redundant.
    _remove_overwritten_stores(nodes)

    # Removal of unused loads will not enable any other simplifications.
    _remove_unused_loads(nodes, returned)

    assert verify_loads(nodes, returned)


def _update_expressions_in_bitstrings(returned: list[BitString]) -> None:
    """Simplifies each expression in the given bit strings."""

    for i, ret_bits in enumerate(returned):
        returned[i] = ret_bits.substitute(expression_func=simplify_expression)


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
