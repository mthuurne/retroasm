from __future__ import annotations

from collections.abc import Callable, Iterable, Mapping, Sequence

from .codeblock import AccessNode, CodeBlock, Load, Store
from .codeblock_simplifier import CodeBlockSimplifier
from .expression import Expression
from .function import Function
from .linereader import BadInput, InputLocation, LineReader
from .reference import BitString, SingleStorage, bad_reference
from .storage import ArgStorage, Storage, Variable


class CodeBlockBuilder:
    def dump(self) -> None:
        """Prints the current state of this code block builder on stdout."""

    def emit_loadBits(
        self, storage: Storage, location: InputLocation | None
    ) -> Expression:
        """
        Loads the value from the given storage by emitting a Load node on
        this builder.
        Returns an expression that represents the loaded value.
        """
        raise NotImplementedError

    def emit_storeBits(
        self, storage: Storage, value: Expression, location: InputLocation | None
    ) -> None:
        """
        Stores the value of the given expression in the given storage by
        emitting a Store node on this builder.
        """
        raise NotImplementedError

    def inlineFunctionCall(
        self,
        func: Function,
        argMap: Mapping[str, BitString | None],
        location: InputLocation | None = None,
    ) -> BitString | None:
        """
        Inlines a call to the given function with the given arguments.
        All arguments should be passed as references: value arguments should
        have their expression wrapped in a FixedValue.
        Returns a BitString containing the value returned by the inlined
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

    def emit_loadBits(
        self, storage: Storage, location: InputLocation | None
    ) -> Expression:
        raise IllegalStateAccess(f"attempt to read state: {storage}", location)

    def emit_storeBits(
        self, storage: Storage, value: Expression, location: InputLocation | None
    ) -> None:
        raise IllegalStateAccess(f"attempt to write state: {storage}", location)

    def inlineFunctionCall(
        self,
        func: Function,
        argMap: Mapping[str, BitString | None],
        location: InputLocation | None = None,
    ) -> BitString | None:
        # TODO: This is probably overly strict: calling a function that does
        #       not touch state should be fine.
        raise IllegalStateAccess("attempt to call function", location)


class SemanticsCodeBlockBuilder(CodeBlockBuilder):
    def __init__(self) -> None:
        self.nodes: list[AccessNode] = []

    def dump(self) -> None:
        for node in self.nodes:
            node.dump()
        super().dump()

    def createCodeBlock(
        self,
        returned: Iterable[BitString],
        log: LineReader | None = None,
        location: InputLocation | None = None,
    ) -> CodeBlock:
        """
        Returns a CodeBlock object containing the items emitted so far.
        The state of the builder does not change.
        The 'returned' sequence contains the bits strings that will be the
        returned values for the created block.
        Raises ValueError if this builder does not represent a valid code block.
        If a log is provided, errors are logged individually as well, using
        the given location if no specific location is known.
        """
        code = CodeBlockSimplifier(self.nodes, returned)

        # Check for reading of uninitialized variables.
        ununitializedLoads = []
        initializedVariables: set[Variable] = set()
        for node in code.nodes:
            match node:
                case Load(storage=Variable(scope=scope) as var) if scope == 1:
                    if var not in initializedVariables:
                        ununitializedLoads.append(node)
                case Store(storage=Variable(scope=scope) as var) if scope == 1:
                    initializedVariables.add(var)
        if ununitializedLoads:
            if log is not None:
                for load in ununitializedLoads:
                    log.error(
                        "variable is read before it is initialized",
                        location=load.location or location,
                    )
            raise ValueError(
                f"code block reads {len(ununitializedLoads):d} "
                f"uninitialized variable(s)"
            )

        # Check for returning of uninitialized variables.
        for retBits in returned:
            for storage in retBits.iter_storages():
                match storage:
                    case Variable(scope=scope) as var if scope == 1:
                        if var not in initializedVariables:
                            msg = "code block returns uninitialized variable(s)"
                            if log is not None:
                                log.error(msg, location=location)
                            raise ValueError(msg)

        # Finalize code block.
        code.simplify()
        code.freeze()
        return code

    def emit_loadBits(
        self, storage: Storage, location: InputLocation | None
    ) -> Expression:
        load = Load(storage, location)
        self.nodes.append(load)
        return load.expr

    def emit_storeBits(
        self, storage: Storage, value: Expression, location: InputLocation | None
    ) -> None:
        self.nodes.append(Store(value, storage, location))

    def inlineFunctionCall(
        self,
        func: Function,
        argMap: Mapping[str, BitString | None],
        location: InputLocation | None = None,
    ) -> BitString | None:
        code = func.code
        if code is None:
            # Missing body, probably because of earlier errors.
            retType = func.retType
            return None if retType is None else bad_reference(retType).bits

        badArgs = argMap.keys() - func.args.keys()
        if badArgs:
            raise KeyError("Non-existing arguments passed: " + ", ".join(badArgs))
        missingArgs = func.args.keys() - argMap.keys()
        if missingArgs:
            raise KeyError("Missing values for arguments: " + ", ".join(missingArgs))

        returned = self.inlineBlock(code, argMap.__getitem__)
        if len(returned) == 1:
            return returned[0]
        else:
            assert len(returned) == 0, returned
            return None

    def inlineBlock(
        self,
        code: CodeBlock,
        argFetcher: Callable[[str], BitString | None] = lambda name: None,
    ) -> list[BitString]:
        """
        Inlines another code block into this one.
        The given argument fetcher function, when called with an argument name,
        should return the bit string passed for that argument, or None if the
        argument should remain an argument in the inlined block.
        Returns a list of BitStrings containing the values returned by the
        inlined block.
        """

        loadResults: dict[Expression, Expression] = {}

        def importExpr(expr: Expression) -> Expression:
            return expr.substitute(loadResults.get)

        def importStorageUncached(storage: Storage) -> BitString | SingleStorage:
            match storage:
                case ArgStorage(name=name) as arg:
                    bits = argFetcher(name)
                    if bits is None:
                        return SingleStorage(arg)
                    else:
                        assert arg.width == bits.width, (arg.width, bits.width)
                        return bits
                case storage:
                    return SingleStorage(storage.substitute_expressions(importExpr))

        storageCache: dict[Storage, BitString] = {}

        def importStorage(storage: Storage) -> BitString:
            """
            Returns a bit string containing the imported version of the given
            storage.
            """
            bits = storageCache.get(storage)
            if bits is None:
                bits = importStorageUncached(storage)
                storageCache[storage] = bits
            return bits

        # Copy nodes.
        for node in code.nodes:
            bits = importStorage(node.storage)
            match node:
                case Load(expr=expr, location=location):
                    value = bits.emit_load(self, location)
                    loadResults[expr] = value
                case Store(expr=expr, location=location):
                    newExpr = importExpr(expr)
                    bits.emit_store(self, newExpr, location)
                case node:
                    assert False, node

        # Determine return value.
        return [
            retBits.substitute(importStorage, importExpr) for retBits in code.returned
        ]
