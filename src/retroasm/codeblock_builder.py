from __future__ import annotations

from collections.abc import Callable, Iterable, Mapping, Sequence
from typing import NoReturn

from .codeblock import AccessNode, FunctionBody, Load, Store
from .codeblock_simplifier import simplify_block
from .expression import Expression
from .function import Function
from .parser.linereader import BadInput, InputLocation, LineReader
from .reference import BitString, FixedValue, SingleStorage, Variable, bad_reference
from .storage import ArgStorage, Storage


def no_args_to_fetch(name: str) -> NoReturn:
    """Argument fetcher that can be used when a function has no arguments."""
    raise ValueError(f'No arguments provided, "{name}" requested')


class CodeBlockBuilder:
    def __init__(self) -> None:
        super().__init__()
        self._variables: dict[str, Expression] = {}

    def dump(self) -> None:
        """Prints the current state of this code block builder on stdout."""

    def emit_load_bits(
        self, storage: Storage, location: InputLocation | None
    ) -> Expression:
        """
        Loads the value from the given storage by emitting a Load node on
        this builder.
        Returns an expression that represents the loaded value.
        """
        raise NotImplementedError

    def emit_store_bits(
        self, storage: Storage, value: Expression, location: InputLocation | None
    ) -> None:
        """
        Stores the value of the given expression in the given storage by
        emitting a Store node on this builder.
        """
        raise NotImplementedError

    def read_variable(self, name: str, location: InputLocation | None) -> Expression:
        try:
            return self._variables[name]
        except KeyError:
            raise BadInput(
                f'variable "{name}" is used before it is initialized', location
            ) from None

    def write_variable(
        self,
        name: str,
        value: Expression,
        location: InputLocation | None,  # pylint: disable=unused-argument
    ) -> None:
        self._variables[name] = value

    def inline_function_call(
        self,
        func: Function,
        arg_map: Mapping[str, BitString],
        location: InputLocation | None = None,
    ) -> BitString | None:
        """
        Inlines a call to the given function with the given arguments.
        All arguments should be passed as references: value arguments should
        have their expression wrapped in a FixedValue.
        If an argument value is None, that argument won't be substituted and
        can appear in the inlined body and in the returned reference.
        Returns a BitString containing the reference returned by the inlined
        function, or None if the function does not return anything.
        """
        raise NotImplementedError


class IllegalStateAccess(BadInput):
    """
    Raised when an operation is attempted that reads or writes state
    in a situation where that is not allowed.
    """

    def __init__(self, msg: str, location: InputLocation | None):
        locations: Sequence[InputLocation]
        if location is None:
            locations = ()
        else:
            locations = (location,)
        super().__init__(msg, *locations)


class StatelessCodeBlockBuilder(CodeBlockBuilder):
    """
    A CodeBlockBuilder that raises IllegalStateAccess when its users attempt
    touch any state, such as performing register access or I/O.
    """

    def emit_load_bits(
        self, storage: Storage, location: InputLocation | None
    ) -> Expression:
        raise IllegalStateAccess(f"attempt to read state: {storage}", location)

    def emit_store_bits(
        self, storage: Storage, value: Expression, location: InputLocation | None
    ) -> None:
        raise IllegalStateAccess(f"attempt to write state: {storage}", location)

    def inline_function_call(
        self,
        func: Function,
        arg_map: Mapping[str, BitString | None],
        location: InputLocation | None = None,
    ) -> BitString | None:
        # TODO: This is probably overly strict: calling a function that does
        #       not touch state should be fine.
        raise IllegalStateAccess("attempt to call function", location)


class SemanticsCodeBlockBuilder(CodeBlockBuilder):
    def __init__(self) -> None:
        super().__init__()
        self.nodes: list[AccessNode] = []

    def dump(self) -> None:
        for node in self.nodes:
            node.dump()
        super().dump()

    def create_code_block(
        self,
        returned: Iterable[BitString],
        log: LineReader | None = None,
        location: InputLocation | None = None,
    ) -> FunctionBody:
        """
        Returns a CodeBlock object containing the items emitted so far.
        The state of the builder does not change.
        The 'returned' sequence contains the bits strings that will be the
        returned values for the created block.
        Raises ValueError if this builder does not represent a valid code block.
        If a log is provided, errors are logged individually as well, using
        the given location if no specific location is known.
        """

        uninitialized_variables = set()

        def fixate_variable(var: Variable) -> FixedValue:
            try:
                value = self.read_variable(var.name, location)
            except BadInput as ex:
                if log:
                    log.error("%s", ex, location=ex.locations)
                uninitialized_variables.add(var.name)
            return FixedValue(value, var.width)

        # Fixate returned variables.
        returned = [bits.substitute(variable_func=fixate_variable) for bits in returned]

        if uninitialized_variables:
            raise ValueError(
                f"{len(uninitialized_variables)} variables were read before they "
                f"were initialized: {', '.join(uninitialized_variables)}"
            )

        nodes = self.nodes
        simplify_block(nodes, returned)

        return FunctionBody(nodes, returned)

    def emit_load_bits(
        self, storage: Storage, location: InputLocation | None
    ) -> Expression:
        load = Load(storage, location)
        self.nodes.append(load)
        return load.expr

    def emit_store_bits(
        self, storage: Storage, value: Expression, location: InputLocation | None
    ) -> None:
        self.nodes.append(Store(value, storage, location))

    def inline_function_call(
        self,
        func: Function,
        arg_map: Mapping[str, BitString],
        location: InputLocation | None = None,
    ) -> BitString | None:
        code = func.code
        if code is None:
            # Missing body, probably because of earlier errors.
            ret_type = func.ret_type
            return None if ret_type is None else bad_reference(ret_type).bits

        bad_args = arg_map.keys() - func.args.keys()
        if bad_args:
            raise KeyError("Non-existing arguments passed: " + ", ".join(bad_args))
        missing_args = func.args.keys() - arg_map.keys()
        if missing_args:
            raise KeyError("Missing values for arguments: " + ", ".join(missing_args))

        returned = self.inline_block(code, arg_map.__getitem__)
        if len(returned) == 1:
            return returned[0]
        else:
            assert len(returned) == 0, returned
            return None

    def inline_block(
        self,
        code: FunctionBody,
        arg_fetcher: Callable[[str], BitString] = no_args_to_fetch,
    ) -> list[BitString]:
        """
        Inlines another code block into this one.
        The given argument fetcher function, when called with an argument name,
        should return the bit string passed for that argument, or None if the
        argument should remain an argument in the inlined block.
        Returns a list of BitStrings containing the values returned by the
        inlined block.
        Raises ValueError if there is mismatch between arguments and the values
        fetched for them.
        """

        load_results: dict[Expression, Expression] = {}

        def import_expr(expr: Expression) -> Expression:
            return expr.substitute(load_results.get)

        def import_storage_uncached(storage: Storage) -> BitString:
            match storage:
                case ArgStorage(name=name) as arg:
                    try:
                        bits = arg_fetcher(name)
                    except Exception as ex:
                        raise ValueError(f"Error fetching argument: {ex}") from ex
                    if arg.width != bits.width:
                        raise ValueError(
                            f'Argument "{name}" is {arg.width} bits wide, '
                            f"but the fetched value is {bits.width} bits wide"
                        )
                    return bits
                case storage:
                    return SingleStorage(storage.substitute_expressions(import_expr))

        storage_cache: dict[Storage, BitString] = {}

        def import_storage(storage: Storage) -> BitString:
            """
            Returns a bit string containing the imported version of the given
            storage.
            """
            bits = storage_cache.get(storage)
            if bits is None:
                bits = import_storage_uncached(storage)
                storage_cache[storage] = bits
            return bits

        # Copy nodes.
        for node in code.nodes:
            bits = import_storage(node.storage)
            match node:
                case Load(expr=expr, location=location):
                    value = bits.emit_load(self, location)
                    load_results[expr] = value
                case Store(expr=expr, location=location):
                    new_expr = import_expr(expr)
                    bits.emit_store(self, new_expr, location)
                case node:
                    assert False, node

        # Determine return value.
        return [
            ret_bits.substitute(
                storage_func=import_storage, expression_func=import_expr
            )
            for ret_bits in code.returned
        ]
