from __future__ import annotations

from collections.abc import Callable, Iterable, Mapping, Sequence
from typing import cast, override

from retroasm.codeblock import AccessNode, FunctionBody, Load, LoadedValue, Store
from retroasm.codeblock_builder import (
    SemanticsCodeBlockBuilder,
    no_args_to_fetch,
    returned_bits,
)
from retroasm.expression import Expression, IntLiteral
from retroasm.function import Function
from retroasm.input import ErrorCollector, InputLocation
from retroasm.namespace import GlobalNamespace, LocalNamespace
from retroasm.reference import BitString, FixedValue, Reference, SingleStorage, io_reference
from retroasm.storage import IOChannel, Storage
from retroasm.types import IntType, Segment, Width, mask_for_width

from ..utils_segment import parse_segment


class SingleStorageReference(Reference):
    """
    Used for type annotations when a returned reference is guaranteed to wrap
    a bitstring containing a single storage.
    """

    bits: SingleStorage


def assert_nodes(
    actual_nodes: Sequence[AccessNode], correct_nodes: Iterable[AccessNode]
) -> None:
    correct_nodes = tuple(correct_nodes)
    load_map: dict[LoadedValue, LoadedValue] = {}

    def load_subst(expr: Expression) -> Expression | None:
        return load_map.get(cast(LoadedValue, expr))

    assert len(actual_nodes) == len(correct_nodes)
    for i, (actual, correct) in enumerate(zip(actual_nodes, correct_nodes)):
        msg = "node %d of %d" % (i + 1, len(actual_nodes))
        if isinstance(correct, Load):
            assert isinstance(actual, Load), msg
            load_map[actual.expr] = correct.expr
        elif isinstance(correct, Store):
            assert isinstance(actual, Store), msg
            expr = actual.expr.substitute(load_subst)
            assert expr == correct.expr, msg
        else:
            raise AssertionError(f"unknown node type: {correct.__class__.__name__}")
        assert actual.storage == correct.storage, msg


def get_ret_val(code: FunctionBody) -> tuple[Expression, Width]:
    (ret_bits,) = code.returned
    assert isinstance(ret_bits, FixedValue)
    return ret_bits.expr, ret_bits.width


def assert_ret_val(code: FunctionBody, value: int) -> None:
    expr, width = get_ret_val(code)
    assert isinstance(expr, IntLiteral), expr
    assert expr.value & mask_for_width(width) == value, (expr.value, width)


class TestNamespace(LocalNamespace):
    parent: GlobalNamespace

    def __init__(self, parent: GlobalNamespace | None = None):
        super().__init__(parent or GlobalNamespace())
        self.builder = SemanticsCodeBlockBuilder()

    def create_code_block(
        self,
        ret_ref: Reference | None,
        collector: ErrorCollector | None = None,
        location: InputLocation | None = None,
    ) -> FunctionBody:
        return self.builder.create_code_block(returned_bits(ret_ref), collector, location)

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

    def add_register(self, name: str, typ: IntType = IntType.u(8)) -> SingleStorageReference:
        try:
            ref = self[name]
        except KeyError:
            # Insert register into global namespace.
            self.parent.add_register(name, typ)
            ref = self[name]

        # Check that existing global namespace entry is this register.
        global_ref = self.parent[name]
        assert isinstance(global_ref, Reference), global_ref
        assert isinstance(global_ref.bits, SingleStorage), global_ref.bits
        assert typ is global_ref.type, global_ref
        reg = global_ref.bits.storage
        assert isinstance(ref, Reference), ref
        assert isinstance(ref.bits, SingleStorage), ref.bits
        assert ref.bits.storage is reg
        assert reg.width == typ.width

        return cast(SingleStorageReference, ref)

    def add_io_storage(
        self,
        channel_name: str,
        index: Expression,
        elem_type: IntType = IntType.u(8),
        addr_type: IntType = IntType.u(16),
    ) -> SingleStorageReference:
        try:
            channel = self.parent[channel_name]
        except KeyError:
            # Insert channel into global namespace.
            channel = IOChannel(channel_name, elem_type, addr_type)
            self.parent.define(channel_name, channel)
        else:
            # Check that existing global namespace entry is this channel.
            assert isinstance(channel, IOChannel), channel
            assert channel.elem_type is elem_type, channel
            assert channel.addr_type is addr_type, channel
        # Import channel from global namespace into local namespace.
        local_channel = self[channel_name]
        assert local_channel is channel
        # Create I/O storage.
        ref = io_reference(channel, index)
        return cast(SingleStorageReference, ref)

    @override
    def add_argument(
        self, name: str, typ: IntType = IntType.u(8), location: InputLocation | None = None
    ) -> SingleStorageReference:
        ref = super().add_argument(name, typ, location)
        return cast(SingleStorageReference, ref)

    @override
    def add_variable(
        self, name: str, typ: IntType = IntType.u(8), location: InputLocation | None = None
    ) -> SingleStorageReference:
        ref = super().add_variable(name, typ, location)
        return cast(SingleStorageReference, ref)

    def add_ret_reference(self, value: Reference) -> None:
        super().define("ret", value)

    def inline_block(
        self, code: FunctionBody, arg_fetcher: Callable[[str], BitString] = no_args_to_fetch
    ) -> list[BitString]:
        return self.builder.inline_block(code, arg_fetcher)

    def inline_function_call(
        self,
        func: Function,
        arg_map: Mapping[str, BitString],
        location: InputLocation | None = None,
    ) -> BitString | None:
        return self.builder.inline_function_call(func, arg_map, location)
