from typing import (
    AbstractSet, Callable, Collection, Dict, Iterable, Iterator, List, Mapping,
    MutableSet, NamedTuple, Optional, Sequence, cast
)

from .codeblock import CodeBlock, Store
from .codeblock_builder import SemanticsCodeBlockBuilder
from .decode import (
    Decoder, DecoderFactory, ParsedModeEntry, Prefix, PrefixDecoder
)
from .expression import IntLiteral
from .fetch import Fetcher
from .linereader import BadInput
from .mode import ModeTable
from .namespace import GlobalNamespace, Namespace
from .reference import Reference, SingleStorage
from .storage import Storage
from .types import Width
from .utils import const_property


class PrefixMapping(NamedTuple):
    prefixes: Sequence[Prefix]
    initCode: CodeBlock
    flagForVar: Mapping[Storage, str]
    prefixForFlag: Mapping[str, Prefix]
    encodingWidth: Optional[int]

def flagsSetByCode(code: CodeBlock) -> Iterator[Storage]:
    '''Yields those storages to which the value 1 is assigned by the given code
    block.
    '''
    for node in code.nodes:
        if isinstance(node, Store):
            value = node.expr
            if isinstance(value, IntLiteral):
                if value.value == 0:
                    continue
                assert value.value == 1, value.value
            else:
                raise ValueError('non-literal assigned to decode flag')
            yield node.storage

class PrefixMappingFactory:

    def __init__(self, namespace: Namespace):
        self._namespace = namespace
        self._prefixes: List[Prefix] = []
        self._initBuilder = SemanticsCodeBlockBuilder()
        self._flagForVar: Dict[Storage, str] = {}
        self._prefixForFlag: Dict[str, Prefix] = {}
        self._encodingWidth: Optional[int] = None

    def hasFlag(self, name: str) -> bool:
        '''Return True iff a decode flag with the given name was added to
        this factory.
        '''
        return name in self._prefixForFlag

    def addPrefixes(self,
                    decodeFlags: Collection[str],
                    prefixes: Iterable[Prefix],
                    ) -> None:
        '''Adds `prefixes`, which use the flags in `decodeFlags`, to this
        mapping.
        Raises KeyError if a decode flag name either does not exist in the
        namespace or was added more than once.
        Raises ValueError if no reverse mapping could be computed from the
        given prefix semantics.
        Raises BadInput if an encoding item's width is inconsistent with
        earlier encoding item widths.
        '''
        self._prefixes += prefixes

        # Check encoding width consistency.
        encWidth = self._encodingWidth
        for prefix in prefixes:
            for encItem in prefix.encoding:
                if encWidth is None:
                    encWidth = encItem.encodingWidth
                elif encWidth != encItem.encodingWidth:
                    raise BadInput(
                        f'encoding item has width {encItem.encodingWidth} '
                        f'while previous item(s) have width {encWidth}',
                        encItem.location
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
                raise KeyError(f'decode flag redefined: {name}')
            flagForVar[cast(SingleStorage, ref.bits).storage] = name
            ref.emitStore(builder, zero, None)

        # Figure out which prefix sets which flag.
        for prefix in prefixes:
            setFlags = {
                flagForVar[storage]
                for storage in flagsSetByCode(prefix.semantics)
                }
            if len(setFlags) == 1:
                name, = setFlags
                prefixForFlag[name] = prefix
            else:
                # Note: In theory we could support prefixes that set multiple
                #       flags, but let's keep things simple until we encounter
                #       a processor that actually requires it.
                raise ValueError('prefix "%s" sets %d flags' % (
                    ' '.join(str(enc) for enc in prefix.encoding),
                    len(setFlags)
                    ))

        unsettableFlags = set(decodeFlags) - set(prefixForFlag.keys())
        if unsettableFlags:
            raise ValueError(
                'unsettable decode flags: %s'
                % ', '.join(sorted(unsettableFlags))
                )

    def createMapping(self) -> PrefixMapping:
        '''Create a `PrefixMapping` using the prefixes added so far.
        '''
        return PrefixMapping(
            self._prefixes,
            self._initBuilder.createCodeBlock(()),
            dict(self._flagForVar),
            dict(self._prefixForFlag),
            self._encodingWidth
            )

class InstructionSet(ModeTable):
    '''Contains all definitions for a processor's instruction set.
    '''

    @property
    def globalNamespace(self) -> GlobalNamespace:
        return self._globalNamespace

    @property
    def prefixMapping(self) -> PrefixMapping:
        return self._prefixMapping

    def __init__(self,
                 encWidth: Optional[int],
                 auxEncWidth: Optional[int],
                 globalNamespace: GlobalNamespace,
                 prefixMapping: PrefixMapping,
                 modeEntries: Mapping[Optional[str], List[ParsedModeEntry]]
                 ):
        if auxEncWidth not in (encWidth, None):
            raise ValueError(
                f'auxiliary encoding width must be None or equal to base '
                f'encoding width {encWidth}, got {auxEncWidth} instead'
                )
        if prefixMapping.encodingWidth not in (None, encWidth):
            raise ValueError(
                f'prefix encoding width {prefixMapping.encodingWidth} is '
                f'different from instruction encoding width {encWidth}'
                )
        instructions = modeEntries[None]
        ModeTable.__init__(
            self, encWidth, auxEncWidth,
            (instr.entry for instr in instructions)
            )
        self._globalNamespace = globalNamespace
        self._prefixMapping = prefixMapping
        self._modeEntries = modeEntries
        self._decoders: Dict[AbstractSet[str], Decoder] = {}

    @const_property
    def prefixDecodeFunc(self) -> Callable[[Fetcher], Optional[Prefix]]:
        prefixes = self._prefixMapping.prefixes
        if len(prefixes) == 0:
            return lambda fetcher: None
        else:
            return PrefixDecoder(prefixes).tryDecode

    def getDecoder(self,
                   flags: AbstractSet[str] = frozenset()
                   ) -> Decoder:
        '''Returns an instruction decoder that decodes an instruction for the
        given combination of decode flags.
        '''
        flags = frozenset(flags)
        decoders = self._decoders
        decoder = decoders.get(flags)
        if decoder is None:
            decoderFactory = DecoderFactory(self._modeEntries, flags)
            decoder = decoderFactory.createDecoder(None, None)
            decoders[flags] = decoder
        return decoder

    @const_property
    def decodeFlagCombinations(self) -> AbstractSet[AbstractSet[str]]:
        '''A set containing all possible combinations of decode flags that can
        be set simultaneously.
        '''
        prefixMapping = self._prefixMapping
        prefixes = prefixMapping.prefixes
        flagForVar = prefixMapping.flagForVar

        flagSets: MutableSet[AbstractSet[str]] = set()

        def addRecursive(flags: AbstractSet[str], code: CodeBlock) -> None:
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
                    flagForVar[storage]
                    for storage in flagsSetByCode(newCode)
                    )
                addRecursive(newFlags, newCode)

        addRecursive(frozenset(), prefixMapping.initCode)
        return flagSets

    @property
    def addrWidth(self) -> Width:
        '''The width of the program counter, in bits.
        '''
        return cast(Reference, self._globalNamespace['pc']).width

    @const_property
    def instructionNames(self) -> AbstractSet[str]:
        '''A set containing the instruction names (operations).
        '''
        return cast(AbstractSet[str], self._mnemTree[0].keys())
