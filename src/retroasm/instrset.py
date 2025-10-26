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
from typing import Self, cast, override

from .codeblock import FunctionBody, Store
from .codeblock_builder import SemanticsCodeBlockBuilder
from .decode import Decoder, DecoderFactory, Prefix, create_prefix_decoder
from .encoding import EncodingExpr, determine_encoding_width
from .expression import IntLiteral
from .fetch import AdvancingFetcher, Fetcher
from .input import BadInput, ErrorCollector
from .mode import ModeMatch, ModeRow, ModeTable
from .namespace import ReadOnlyNamespace
from .reference import Reference, SingleStorage
from .storage import Storage
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
    def __init__(self, namespace: ReadOnlyNamespace):
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

    def add_prefixes(self, decode_flags: Collection[str], prefixes: Iterable[Prefix]) -> None:
        """
        Add `prefixes`, which use the flags in `decode_flags`, to this mapping.

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
            for enc_item in prefix.encoding.items:
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
                enc_str = " ".join(str(enc) for enc in prefix.encoding.items)
                raise ValueError(f'prefix "{enc_str}" sets {len(set_flags):d} flags')

        unsettable_flags = set(decode_flags) - set(prefix_for_flag.keys())
        if unsettable_flags:
            raise ValueError("unsettable decode flags: " + ", ".join(sorted(unsettable_flags)))

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
    def program_counter(self) -> Reference:
        return self._program_counter

    @property
    def prefix_mapping(self) -> PrefixMapping:
        return self._prefix_mapping

    @classmethod
    def create(
        cls,
        instructions: Iterable[ModeRow],
        program_counter: Reference,
        collector: ErrorCollector,
        prefix_mapping: PrefixMapping | None = None,
    ) -> Self:
        """
        Create an instruction set containing the given instructions.

        Errors are reported on the given error collector; if any errors are reported
        the returned instruction set may not work correctly or at all.
        """
        instructions = tuple(instructions)

        encodings = [instr.encoding for instr in instructions]
        where_desc = "for instructions"
        enc_width = determine_encoding_width(encodings, False, where_desc, collector)
        any_aux = any(len(instr.encoding.items) >= 2 for instr in instructions)
        aux_enc_width = enc_width if any_aux else None
        if enc_width is None:
            # Since the last instruction with an identical encoding overrides earlier ones,
            # only degenerate instruction sets can have an empty encoding: either the
            # instruction set is empty or it has a single instruction with no encoding.
            if instructions:
                collector.error(
                    "no instruction encodings defined",
                    location=instructions[-1].encoding.location,
                )
            else:
                collector.warning("no instructions defined")
            enc_width = 0
        elif any_aux:
            for instruction in instructions:
                enc_def = instruction.encoding
                if (width := enc_def.aux_encoding_width) not in (None, aux_enc_width):
                    collector.error(
                        f"instruction row has auxiliary encoding width of {width} bits, "
                        f"expected {aux_enc_width} bits",
                        location=enc_def.aux_encoding_location,
                    )

        if prefix_mapping is None:
            prefix_mapping = PrefixMappingFactory({}).create_mapping()
        elif prefix_mapping.encoding_width not in (None, enc_width):
            collector.error(
                f"prefix encoding width {prefix_mapping.encoding_width} is "
                f"different from instruction encoding width {enc_width}"
            )

        return cls(enc_width, aux_enc_width, program_counter, prefix_mapping, instructions)

    def __init__(
        self,
        enc_width: int,
        aux_enc_width: int | None,
        program_counter: Reference,
        prefix_mapping: PrefixMapping,
        instructions: Sequence[ModeRow],
    ):
        ModeTable.__init__(self, enc_width, aux_enc_width, instructions)
        self._program_counter = program_counter
        self._prefix_mapping = prefix_mapping
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
            decoder_factory = DecoderFactory(flags)
            decoder = decoder_factory.create_decoder(self, None)
            decoders[flags] = decoder
        return decoder

    def decode_instruction(self, fetcher: AdvancingFetcher) -> tuple[int, ModeMatch | None]:
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
            for enc_item in prefix.encoding.items:
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
        return self._program_counter.type
