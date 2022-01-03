from __future__ import annotations

from dataclasses import dataclass
from typing import (
    AbstractSet,
    Callable,
    Collection,
    Iterable,
    Iterator,
    Mapping,
    MutableSet,
    Sequence,
    cast,
)

from .codeblock import CodeBlock, Store
from .codeblock_builder import SemanticsCodeBlockBuilder
from .decode import (
    Decoder,
    DecoderFactory,
    ParsedModeEntry,
    Prefix,
    createPrefixDecoder,
)
from .expression import IntLiteral
from .fetch import AdvancingFetcher, Fetcher
from .linereader import BadInput
from .mode import EncodingExpr, ModeMatch, ModeTable
from .namespace import GlobalNamespace, Namespace
from .reference import Reference, SingleStorage
from .storage import Storage
from .types import IntType
from .utils import const_property


@dataclass(frozen=True)
class PrefixMapping:
    prefixes: Sequence[Prefix]
    initCode: CodeBlock
    flagForVar: Mapping[Storage, str]
    prefixForFlag: Mapping[str, Prefix]
    encodingWidth: int | None


def flagsSetByCode(code: CodeBlock) -> Iterator[Storage]:
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
        self._initBuilder = SemanticsCodeBlockBuilder()
        self._flagForVar: dict[Storage, str] = {}
        self._prefixForFlag: dict[str, Prefix] = {}
        self._encodingWidth: int | None = None

    def hasFlag(self, name: str) -> bool:
        """
        Return True iff a decode flag with the given name was added to this factory.
        """
        return name in self._prefixForFlag

    def addPrefixes(
        self,
        decodeFlags: Collection[str],
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
        encWidth = self._encodingWidth
        for prefix in prefixes:
            for encItem in prefix.encoding:
                if encWidth is None:
                    encWidth = encItem.encodingWidth
                elif encWidth != encItem.encodingWidth:
                    raise BadInput(
                        f"encoding item has width {encItem.encodingWidth} "
                        f"while previous item(s) have width {encWidth}",
                        encItem.location,
                    )
        self._encodingWidth = encWidth

        # Collect decode flag variables, build init code for them.
        builder = self._initBuilder
        namespace = self._namespace
        prefixForFlag = self._prefixForFlag
        flagForVar = self._flagForVar
        zero = IntLiteral(0)
        for name in decodeFlags:
            ref = cast(Reference, namespace[name])
            if name in prefixForFlag:
                raise KeyError(f"decode flag redefined: {name}")
            flagForVar[cast(SingleStorage, ref.bits).storage] = name
            ref.emit_store(builder, zero, None)

        # Figure out which prefix sets which flag.
        for prefix in prefixes:
            setFlags = {
                flagForVar[storage] for storage in flagsSetByCode(prefix.semantics)
            }
            if len(setFlags) == 1:
                (name,) = setFlags
                prefixForFlag[name] = prefix
            else:
                # Note: In theory we could support prefixes that set multiple
                #       flags, but let's keep things simple until we encounter
                #       a processor that actually requires it.
                encStr = " ".join(str(enc) for enc in prefix.encoding)
                raise ValueError(f'prefix "{encStr}" sets {len(setFlags):d} flags')

        unsettableFlags = set(decodeFlags) - set(prefixForFlag.keys())
        if unsettableFlags:
            raise ValueError(
                "unsettable decode flags: " + ", ".join(sorted(unsettableFlags))
            )

    def createMapping(self) -> PrefixMapping:
        """Create a `PrefixMapping` using the prefixes added so far."""
        return PrefixMapping(
            self._prefixes,
            self._initBuilder.createCodeBlock(()),
            dict(self._flagForVar),
            dict(self._prefixForFlag),
            self._encodingWidth,
        )


class InstructionSet(ModeTable):
    """Contains all definitions for a processor's instruction set."""

    @property
    def encodingWidth(self) -> int:
        return cast(int, self._encWidth)

    @property
    def encodingType(self) -> IntType:
        return IntType.u(self.encodingWidth)

    @property
    def globalNamespace(self) -> GlobalNamespace:
        return self._globalNamespace

    @property
    def prefixMapping(self) -> PrefixMapping:
        return self._prefixMapping

    def __init__(
        self,
        encWidth: int,
        auxEncWidth: int | None,
        globalNamespace: GlobalNamespace,
        prefixMapping: PrefixMapping,
        modeEntries: Mapping[str | None, list[ParsedModeEntry]],
    ):
        if auxEncWidth not in (encWidth, None):
            raise ValueError(
                f"auxiliary encoding width must be None or equal to base "
                f"encoding width {encWidth}, got {auxEncWidth} instead"
            )
        if prefixMapping.encodingWidth not in (None, encWidth):
            raise ValueError(
                f"prefix encoding width {prefixMapping.encodingWidth} is "
                f"different from instruction encoding width {encWidth}"
            )
        instructions = modeEntries[None]
        ModeTable.__init__(
            self, encWidth, auxEncWidth, (instr.entry for instr in instructions)
        )
        self._globalNamespace = globalNamespace
        self._prefixMapping = prefixMapping
        self._modeEntries = modeEntries
        self._decoders: dict[frozenset[str], Decoder] = {}

    @const_property
    def prefixDecodeFunc(self) -> Callable[[Fetcher], Prefix | None]:
        return createPrefixDecoder(self._prefixMapping.prefixes)

    def getDecoder(self, flags: AbstractSet[str] = frozenset()) -> Decoder:
        """
        Returns an instruction decoder that decodes an instruction for the
        given combination of decode flags.
        """
        flags = frozenset(flags)
        decoders = self._decoders
        decoder = decoders.get(flags)
        if decoder is None:
            decoderFactory = DecoderFactory(self._modeEntries, flags)
            decoder = decoderFactory.createDecoder(None, None)
            decoders[flags] = decoder
        return decoder

    def decodeInstruction(
        self, fetcher: AdvancingFetcher
    ) -> tuple[int, ModeMatch | None]:
        """
        Attempt to decode one instruction from the given fetcher.

        Return the number of encoding items decoded and the decoded instruction, if any.
        """

        # Decode prefixes.
        prefixes = []
        decodePrefix = self.prefixDecodeFunc
        encodedLength = 0
        while (prefix := decodePrefix(fetcher)) is not None:
            prefixes.append(prefix)
            prefixEncLen = prefix.encoding.encodedLength
            assert prefixEncLen is not None, prefix
            fetcher = fetcher.advance(prefixEncLen)
            encodedLength += prefixEncLen

        # Compute prefix flags.
        if prefixes:
            prefixMapping = self.prefixMapping
            prefixBuilder = SemanticsCodeBlockBuilder()
            prefixBuilder.inlineBlock(prefixMapping.initCode)
            for prefix in prefixes:
                prefixBuilder.inlineBlock(prefix.semantics)
            prefixCode = prefixBuilder.createCodeBlock(())
            flagForVar = prefixMapping.flagForVar
            flags = frozenset(
                flagForVar[storage] for storage in flagsSetByCode(prefixCode)
            )
        else:
            flags = frozenset()

        # Decode instruction.
        decoder = self.getDecoder(flags)
        encMatch = decoder.tryDecode(fetcher)
        if encMatch is None:
            modeMatch = None
        else:
            encodedLength += encMatch.encodedLength
            modeMatch = ModeMatch.fromEncodeMatch(encMatch)

        return encodedLength, modeMatch

    def encodeInstruction(self, modeMatch: ModeMatch) -> Iterator[int]:
        # Emit prefixes.
        # TODO: When there can be more than one prefix, alphabetical sorting
        #       may not be the right order.
        for name in sorted(modeMatch.flagsRequired):
            prefix = self._prefixMapping.prefixForFlag[name]
            for encItem in prefix.encoding:
                assert isinstance(encItem, EncodingExpr), encItem
                yield encItem.bits.int_value

        for bits in modeMatch.iterBits():
            yield bits.int_value

    @const_property
    def decodeFlagCombinations(self) -> AbstractSet[AbstractSet[str]]:
        """
        A set containing all possible combinations of decode flags that can
        be set simultaneously.
        """
        prefixMapping = self._prefixMapping
        prefixes = prefixMapping.prefixes
        flagForVar = prefixMapping.flagForVar

        flagSets: MutableSet[AbstractSet[str]] = set()

        def addRecursive(flags: frozenset[str], code: CodeBlock) -> None:
            if flags in flagSets:
                return
            flagSets.add(flags)
            for prefix in prefixes:
                # Build a code block that describes the decoder state changes
                # from the given code and encountering the current prefix.
                builder = SemanticsCodeBlockBuilder()
                builder.inlineBlock(code)
                builder.inlineBlock(prefix.semantics)
                newCode = builder.createCodeBlock(())

                # Figure out which decode flags are set by 'newCode'.
                newFlags = frozenset(
                    flagForVar[storage] for storage in flagsSetByCode(newCode)
                )
                addRecursive(newFlags, newCode)

        addRecursive(frozenset(), prefixMapping.initCode)
        return flagSets

    @property
    def addrType(self) -> IntType:
        """The type of the program counter."""
        return cast(Reference, self._globalNamespace["pc"]).type

    @const_property
    def instructionNames(self) -> AbstractSet[str]:
        """A set containing the instruction names (operations)."""
        return cast(AbstractSet[str], self._mnemTree._children.keys())
