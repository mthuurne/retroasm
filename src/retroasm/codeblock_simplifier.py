from __future__ import annotations

from collections import defaultdict

from .codeblock import Load, LoadedValue, Store, verify_loads
from .expression import Expression
from .reference import BitString


def simplify_block(operations: list[Load | Store], returned: list[BitString]) -> None:
    """Attempt to simplify the given code block as much as possible."""

    # Simplify returned expressions.
    # This can also help find additional unused loads, if a loaded value is dropped
    # during simplification because it doesn't affect the expression's value.
    _update_expressions_in_bitstrings(returned)

    # With known-value loads removed by the builder, some prior stores to the same
    # storages may have become redundant.
    _remove_overwritten_stores(operations)

    # Removal of unused loads will not enable any other simplifications.
    _remove_unused_loads(operations, returned)

    assert verify_loads(operations, returned)


def _update_expressions_in_bitstrings(returned: list[BitString]) -> None:
    """Simplifies each expression in the given bit strings."""

    for i, ret_bits in enumerate(returned):
        returned[i] = ret_bits.simplify()


def _remove_overwritten_stores(operations: list[Load | Store]) -> None:
    """Remove side-effect-free stores that will be overwritten."""

    will_be_overwritten = set()
    for i in range(len(operations) - 1, -1, -1):
        operation = operations[i]
        storage = operation.storage
        if not storage.can_store_have_side_effect():
            match operation:
                case Load():
                    will_be_overwritten.discard(storage)
                case Store():
                    if storage in will_be_overwritten:
                        del operations[i]
                    else:
                        will_be_overwritten.add(storage)


def _remove_unused_loads(operations: list[Load | Store], returned: list[BitString]) -> None:
    """Remove side-effect-free loads of which the LoadedValue is unused."""

    # Keep track of how often each LoadedValue is used.
    use_counts = defaultdict[LoadedValue, int](int)

    def update_counts(expr: Expression, delta: int = 1) -> None:
        for loaded in expr.iter_instances(LoadedValue):
            use_counts[loaded] += delta

    # Compute initial use counts.
    for operation in operations:
        if isinstance(operation, Store):
            update_counts(operation.expr)
        for expr in operation.storage.iter_expressions():
            update_counts(expr)
    for ret_bits in returned:
        for expr in ret_bits.iter_expressions():
            update_counts(expr)

    # Remove unnecesary Loads.
    for i in range(len(operations) - 1, -1, -1):
        match operations[i]:
            case Load(expr=expr, storage=storage):
                if use_counts[expr] == 0 and not storage.can_load_have_side_effect():
                    del operations[i]
                    # Update use_counts, so we can remove earlier Loads that
                    # became unused because the Load we just removed was
                    # the sole user of their LoadedValue.
                    for expr in storage.iter_expressions():
                        update_counts(expr, -1)
