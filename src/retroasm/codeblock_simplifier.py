from __future__ import annotations

from collections import defaultdict

from .codeblock import BasicBlock, Load, LoadedValue, Store
from .expression import Expression
from .expression_simplifier import simplify_expression
from .reference import FixedValue
from .storage import Storage, Variable


class CodeBlockSimplifier(BasicBlock):
    def freeze(self) -> None:
        """
        Change the type of this object from CodeBlockSimplifier to CodeBlock,
        to indicate that no further modifications are intended.
        """
        self.__class__ = BasicBlock  # type: ignore

    def simplify(self) -> None:
        """Attempt to simplify the code block as much as possible."""
        # Peform initial simplification of all expressions.
        # This allows remove_redundant_nodes() to only simplify expressions when
        # it changes them.
        self._update_expressions(simplify_expression)

        # This mainly collapses incremental updates to variables.
        self.remove_redundant_nodes()

        # Removal of unused stores might make some loads unused.
        self.remove_unused_stores()

        # Removal of unused loads will not enable any other simplifications.
        self.remove_unused_loads()

        assert self.verify()

    def remove_redundant_nodes(self) -> None:
        nodes = self.nodes

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

        # Fixate variables and apply load replacements in returned bit strings.
        returned = self.returned
        for i, ret_bits in enumerate(returned):

            def fixate_variables(storage: Storage) -> FixedValue | None:
                match storage:
                    case Variable(scope=scope, width=width) as storage if scope == 1:
                        return FixedValue(current_values[storage], width)
                    case _:
                        return None

            new_bits = ret_bits.substitute(fixate_variables, replace_loaded_values)
            if new_bits is not ret_bits:
                returned[i] = new_bits

    def remove_unused_stores(self) -> None:
        """
        Remove side-effect-free stores that will be overwritten or that
        write a variable that will go out of scope.
        """
        nodes = self.nodes

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
                        assert not (
                            isinstance(storage, Variable) and storage.scope == 1
                        ), storage
                        will_be_overwritten.discard(storage)
                    case Store():
                        if storage in will_be_overwritten or (
                            isinstance(storage, Variable) and storage.scope == 1
                        ):
                            del nodes[i]
                        will_be_overwritten.add(storage)

    def remove_unused_loads(self) -> None:
        """Remove side-effect-free loads of which the LoadedValue is unused."""
        nodes = self.nodes

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
        for ret_bits in self.returned:
            for expr in ret_bits.iter_expressions():
                update_counts(expr)

        # Remove unnecesary Loads.
        for i in range(len(nodes) - 1, -1, -1):
            match nodes[i]:
                case Load(expr=expr, storage=storage):
                    if (
                        use_counts[expr] == 0
                        and not storage.can_load_have_side_effect()
                    ):
                        del nodes[i]
                        # Update use_counts, so we can remove earlier Loads that
                        # became unused because the Load we just removed was
                        # the sole user of their LoadedValue.
                        for expr in storage.iter_expressions():
                            update_counts(expr, -1)
