from __future__ import annotations

from typing import Callable, Iterable, Mapping, NoReturn, Sequence, cast

from retroasm.codeblock import CodeBlock, Load, LoadedValue, Node, Store
from retroasm.codeblock_builder import (
    SemanticsCodeBlockBuilder,
    StatelessCodeBlockBuilder,
)
from retroasm.expression import Expression, IntLiteral
from retroasm.function import Function
from retroasm.linereader import InputLocation
from retroasm.namespace import (
    BuilderNamespace,
    GlobalNamespace,
    LocalNamespace,
    createIOReference,
)
from retroasm.reference import BitString, FixedValue, Reference, SingleStorage
from retroasm.storage import IOChannel, Storage
from retroasm.types import IntType, Segment, Width, mask_for_width

from .utils_segment import parse_segment


class SingleStorageReference(Reference):
    """
    Used for type annotations when a returned reference is guaranteed to wrap
    a bitstring containing a single storage.
    """

    bits: SingleStorage


def assertNodes(actualNodes: Sequence[Node], correctNodes: Iterable[Node]) -> None:
    correctNodes = tuple(correctNodes)
    loadMap: dict[LoadedValue, LoadedValue] = {}

    def loadSubst(expr: Expression) -> Expression | None:
        return loadMap.get(cast(LoadedValue, expr))

    assert len(actualNodes) == len(correctNodes)
    for i, (actual, correct) in enumerate(zip(actualNodes, correctNodes)):
        msg = "node %d of %d" % (i + 1, len(actualNodes))
        if isinstance(correct, Load):
            assert isinstance(actual, Load), msg
            loadMap[actual.expr] = correct.expr
        elif isinstance(correct, Store):
            assert isinstance(actual, Store), msg
            expr = actual.expr.substitute(loadSubst)
            assert expr == correct.expr, msg
        else:
            raise AssertionError(f"unknown node type: {correct.__class__.__name__}")
        assert actual.storage == correct.storage, msg


def getRetVal(code: CodeBlock) -> tuple[Expression, Width]:
    (retBits,) = code.returned
    assert isinstance(retBits, FixedValue)
    return retBits.expr, retBits.width


def assertRetVal(code: CodeBlock, value: int) -> None:
    expr, width = getRetVal(code)
    assert isinstance(expr, IntLiteral)
    assert expr.value & mask_for_width(width) == value


def _arg_fetch_fail(name: str) -> NoReturn:
    """Argument fetcher that fails the test when called."""
    assert False, name


class TestNamespace(LocalNamespace):

    parent: BuilderNamespace

    def __init__(self, parent: BuilderNamespace | None = None):
        if parent is None:
            globalBuilder = StatelessCodeBlockBuilder()
            parent = GlobalNamespace(globalBuilder)
        localBuilder = SemanticsCodeBlockBuilder()
        LocalNamespace.__init__(self, parent, localBuilder)

    def _parse_one(self, storage_str: str) -> tuple[Storage, Segment]:
        idx = storage_str.index("[")
        name = storage_str[:idx]
        slice_str = storage_str[idx:]
        ref = self[name]
        assert isinstance(ref, Reference), ref
        bits = ref.bits
        assert isinstance(bits, SingleStorage), bits
        return bits.storage, parse_segment(slice_str)

    def parse(self, *storage_slices: str) -> tuple[tuple[Storage, Segment], ...]:
        return tuple(self._parse_one(storage_str) for storage_str in storage_slices)

    def emit_load(self, ref: BitString | Reference) -> Expression:
        return ref.emit_load(self.builder, None)

    def emit_store(self, ref: BitString | Reference, expr: Expression) -> None:
        ref.emit_store(self.builder, expr, None)

    def addRegister(
        self, name: str, typ: IntType = IntType.u(8)
    ) -> SingleStorageReference:
        try:
            ref = self[name]
        except KeyError:
            # Insert register into global namespace.
            self.parent.addVariable(name, typ)
            ref = self[name]

        # Check that existing global namespace entry is this register.
        globalRef = self.parent[name]
        assert isinstance(globalRef, Reference), globalRef
        assert isinstance(globalRef.bits, SingleStorage), globalRef.bits
        assert typ is globalRef.type, globalRef
        reg = globalRef.bits.storage
        assert isinstance(ref, Reference), ref
        assert isinstance(ref.bits, SingleStorage), ref.bits
        assert ref.bits.storage is reg
        assert reg.width == typ.width

        return cast(SingleStorageReference, ref)

    def addIOStorage(
        self,
        channelName: str,
        index: Expression,
        elemType: IntType = IntType.u(8),
        addrType: IntType = IntType.u(16),
    ) -> SingleStorageReference:
        try:
            channel = self.parent[channelName]
        except KeyError:
            # Insert channel into global namespace.
            channel = IOChannel(channelName, elemType, addrType)
            self.parent.define(channelName, channel)
        else:
            # Check that existing global namespace entry is this channel.
            assert isinstance(channel, IOChannel), channel
            assert channel.elem_type is elemType, channel
            assert channel.addr_type is addrType, channel
        # Import channel from global namespace into local namespace.
        localChannel = self[channelName]
        assert localChannel is channel
        # Create I/O storage.
        ref = createIOReference(channel, index)
        return cast(SingleStorageReference, ref)

    def addArgument(
        self,
        name: str,
        typ: IntType = IntType.u(8),
        location: InputLocation | None = None,
    ) -> SingleStorageReference:
        ref = super().addArgument(name, typ, location)
        return cast(SingleStorageReference, ref)

    def addVariable(
        self,
        name: str,
        typ: IntType = IntType.u(8),
        location: InputLocation | None = None,
    ) -> SingleStorageReference:
        ref = super().addVariable(name, typ, location)
        return cast(SingleStorageReference, ref)

    def addRetReference(self, value: Reference) -> None:
        super().define("ret", value)

    def inlineBlock(
        self,
        code: CodeBlock,
        argFetcher: Callable[[str], BitString | None] = _arg_fetch_fail,
    ) -> list[BitString]:
        return self.builder.inlineBlock(code, argFetcher)

    def inlineFunctionCall(
        self,
        func: Function,
        argMap: Mapping[str, BitString | None],
        location: InputLocation | None = None,
    ) -> BitString | None:
        return self.builder.inlineFunctionCall(func, argMap, location)
