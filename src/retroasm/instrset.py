from __future__ import annotations

from collections.abc import (
    Callable,
    Collection,
    Iterable,
    Iterator,
    Mapping,
    MutableSet,
    Sequence,
    Set,
)
from dataclasses import dataclass
from typing import cast, override

from .codeblock import FunctionBody, Store
from .codeblock_builder import SemanticsCodeBlockBuilder, decompose_store
from .decode import (
    Decoder,
    DecoderFactory,
    ParsedModeEntry,
    Prefix,
    create_prefix_decoder,
)
from .expression import Expression, IntLiteral
from .fetch import AdvancingFetcher, Fetcher
from .input import BadInput
from .mode import EncodingExpr, ModeMatch, ModeTable
from .namespace import GlobalNamespace, Namespace
from .reference import Reference, SingleStorage
from .storage import Register, Storage
from .symbol import CurrentAddress
from .types import IntType, Width
from .utils import const_property


@dataclass(frozen=True)
class PrefixMapping:
    prefixes: Sequence[Prefix]
    init_code: FunctionBody
    flag_for_var: Mapping[Storage, str]
    prefix_for_flag: Mapping[str, Prefix]
    encoding_width: Width | None


def flags_set_by_code(code: FunctionBody) -> Iterator[Storage]:
    """
    Yields those storages to which the value 1 is assigned by the given code block.
    """
    for node in code.nodes:
        if isinstance(node, Store):
            value = node.expr
            if isinstance(value, IntLiteral):
                if value.value == 0:
                    continue
                assert value.value == 1, value.value
            else:
                raise ValueError("non-literal assigned to decode flag")
            yield node.storage


class PrefixMappingFactory:
    def __init__(self, namespace: Namespace):
        self._namespace = namespace
        self._prefixes: list[Prefix] = []
        self._init_builder = SemanticsCodeBlockBuilder()
        self._flag_for_var: dict[Storage, str] = {}
        self._prefix_for_flag: dict[str, Prefix] = {}
        self._encoding_width: Width | None = None

    def has_flag(self, name: str) -> bool:
        """
        Return True iff a decode flag with the given name was added to this factory.
        """
        return name in self._prefix_for_flag

    def add_prefixes(
        self,
        decode_flags: Collection[str],
        prefixes: Iterable[Prefix],
    ) -> None:
        """
        Add `prefixes`, which use the flags in `decodeFlags`, to this mapping.

        Raises KeyError if a decode flag name either does not exist in the
        namespace or was added more than once.
        Raises ValueError if no reverse mapping could be computed from the
        given prefix semantics.
        Raises BadInput if an encoding item's width is inconsistent with
        earlier encoding item widths.
        """
        self._prefixes += prefixes

        # Check encoding width consistency.
        enc_width = self._encoding_width
        for prefix in prefixes:
            for enc_item in prefix.encoding:
                if enc_width is None:
                    enc_width = enc_item.encoding_width
                elif enc_width != enc_item.encoding_width:
                    raise BadInput(
                        f"encoding item has width {enc_item.encoding_width} "
                        f"while previous item(s) have width {enc_width}",
                        enc_item.location,
                    )
        self._encoding_width = enc_width

        # Collect decode flag variables, build init code for them.
        builder = self._init_builder
        namespace = self._namespace
        prefix_for_flag = self._prefix_for_flag
        flag_for_var = self._flag_for_var
        zero = IntLiteral(0)
        for name in decode_flags:
            ref = cast(Reference, namespace[name])
            if name in prefix_for_flag:
                raise KeyError(f"decode flag redefined: {name}")
            flag_for_var[cast(SingleStorage, ref.bits).storage] = name
            ref.emit_store(builder, zero, None)

        # Figure out which prefix sets which flag.
        for prefix in prefixes:
            set_flags = {
                flag_for_var[storage] for storage in flags_set_by_code(prefix.semantics)
            }
            if len(set_flags) == 1:
                (name,) = set_flags
                prefix_for_flag[name] = prefix
            else:
                # Note: In theory we could support prefixes that set multiple
                #       flags, but let's keep things simple until we encounter
                #       a processor that actually requires it.
                enc_str = " ".join(str(enc) for enc in prefix.encoding)
                raise ValueError(f'prefix "{enc_str}" sets {len(set_flags):d} flags')

        unsettable_flags = set(decode_flags) - set(prefix_for_flag.keys())
        if unsettable_flags:
            raise ValueError(
                "unsettable decode flags: " + ", ".join(sorted(unsettable_flags))
            )

    def create_mapping(self) -> PrefixMapping:
        """Create a `PrefixMapping` using the prefixes added so far."""
        return PrefixMapping(
            self._prefixes,
            self._init_builder.create_code_block(()),
            dict(self._flag_for_var),
            dict(self._prefix_for_flag),
            self._encoding_width,
        )


class InstructionSet(ModeTable):
    """Contains all definitions for a processor's instruction set."""

    @property
    @override
    def encoding_width(self) -> int:
        return cast(int, self._enc_width)

    @property
    def encoding_type(self) -> IntType:
        return IntType.u(self.encoding_width)

    @property
    def program_counter(self) -> Reference:
        return self._global_namespace.program_counter

    @const_property
    def program_counter_fixated(self) -> Mapping[Register, Expression]:
        """
        Fixate the program counter address in the underlying base registers.

        Typically, the returned dictionary will just map a single program counter
        register to the `CurrentAddress` singleton. However, more complex program
        counter setups could exist.
        """

        pc = self._global_namespace.program_counter
        mapping = decompose_store(pc, CurrentAddress())
        for storage in mapping:
            assert isinstance(storage, Register), storage
        return cast(Mapping[Register, Expression], mapping)

    @property
    def prefix_mapping(self) -> PrefixMapping:
        return self._prefix_mapping

    def __init__(
        self,
        enc_width: int,
        aux_enc_width: int | None,
        global_namespace: GlobalNamespace,
        prefix_mapping: PrefixMapping,
        mode_entries: Mapping[str | None, list[ParsedModeEntry]],
    ):
        if aux_enc_width not in (enc_width, None):
            raise ValueError(
                f"auxiliary encoding width must be None or equal to base "
                f"encoding width {enc_width}, got {aux_enc_width} instead"
            )
        if prefix_mapping.encoding_width not in (None, enc_width):
            raise ValueError(
                f"prefix encoding width {prefix_mapping.encoding_width} is "
                f"different from instruction encoding width {enc_width}"
            )
        instructions = mode_entries[None]
        ModeTable.__init__(
            self, enc_width, aux_enc_width, (instr.entry for instr in instructions)
        )
        self._global_namespace = global_namespace
        self._prefix_mapping = prefix_mapping
        self._modeEntries = mode_entries
        self._decoders: dict[frozenset[str], Decoder] = {}

    @const_property
    def prefix_decode_func(self) -> Callable[[Fetcher], Prefix | None]:
        return create_prefix_decoder(self._prefix_mapping.prefixes)

    def get_decoder(self, flags: Set[str] = frozenset()) -> Decoder:
        """
        Returns an instruction decoder that decodes an instruction for the
        given combination of decode flags.
        """
        flags = frozenset(flags)
        decoders = self._decoders
        decoder = decoders.get(flags)
        if decoder is None:
            decoder_factory = DecoderFactory(self._modeEntries, flags)
            decoder = decoder_factory.create_decoder(None, None)
            decoders[flags] = decoder
        return decoder

    def decode_instruction(
        self, fetcher: AdvancingFetcher
    ) -> tuple[int, ModeMatch | None]:
        """
        Attempt to decode one instruction from the given fetcher.

        Return the number of encoding items decoded and the decoded instruction, if any.
        """

        # Decode prefixes.
        prefixes = []
        decode_prefix = self.prefix_decode_func
        encoded_length = 0
        while (prefix := decode_prefix(fetcher)) is not None:
            prefixes.append(prefix)
            prefix_enc_len = prefix.encoding.encoded_length
            assert prefix_enc_len is not None, prefix
            fetcher = fetcher.advance(prefix_enc_len)
            encoded_length += prefix_enc_len

        # Compute prefix flags.
        if prefixes:
            prefix_mapping = self.prefix_mapping
            prefix_builder = SemanticsCodeBlockBuilder()
            prefix_builder.inline_block(prefix_mapping.init_code)
            for prefix in prefixes:
                prefix_builder.inline_block(prefix.semantics)
            prefix_code = prefix_builder.create_code_block(())
            flag_for_var = prefix_mapping.flag_for_var
            flags = frozenset(
                flag_for_var[storage] for storage in flags_set_by_code(prefix_code)
            )
        else:
            flags = frozenset()

        # Decode instruction.
        decoder = self.get_decoder(flags)
        enc_match = decoder.try_decode(fetcher)
        if enc_match is None:
            mode_match = None
        else:
            encoded_length += enc_match.encoded_length
            mode_match = enc_match.complete()

        return encoded_length, mode_match

    def encode_instruction(self, mode_match: ModeMatch) -> Iterator[int]:
        # Emit prefixes.
        # TODO: When there can be more than one prefix, alphabetical sorting
        #       may not be the right order.
        for name in sorted(mode_match.flags_required):
            prefix = self._prefix_mapping.prefix_for_flag[name]
            for enc_item in prefix.encoding:
                assert isinstance(enc_item, EncodingExpr), enc_item
                yield enc_item.bits.int_value

        for bits in mode_match.iter_bits():
            yield bits.int_value

    @const_property
    def decode_flag_combinations(self) -> Set[Set[str]]:
        """
        A set containing all possible combinations of decode flags that can
        be set simultaneously.
        """
        prefix_mapping = self._prefix_mapping
        prefixes = prefix_mapping.prefixes
        flag_for_var = prefix_mapping.flag_for_var

        flag_sets: MutableSet[Set[str]] = set()

        def add_recursive(flags: frozenset[str], code: FunctionBody) -> None:
            if flags in flag_sets:
                return
            flag_sets.add(flags)
            for prefix in prefixes:
                # Build a code block that describes the decoder state changes
                # from the given code and encountering the current prefix.
                builder = SemanticsCodeBlockBuilder()
                builder.inline_block(code)
                builder.inline_block(prefix.semantics)
                new_code = builder.create_code_block(())

                # Figure out which decode flags are set by 'newCode'.
                new_flags = frozenset(
                    flag_for_var[storage] for storage in flags_set_by_code(new_code)
                )
                add_recursive(new_flags, new_code)

        add_recursive(frozenset(), prefix_mapping.init_code)
        return flag_sets

    @property
    def addr_type(self) -> IntType:
        """The type of the program counter."""
        return self._global_namespace.program_counter.type
