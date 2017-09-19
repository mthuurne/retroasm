from .codeblock import Store
from .codeblock_builder import SemanticsCodeBlockBuilder
from .decode import createDecoder, createEntryDecoder
from .expression import IntLiteral
from .linereader import BadInput
from .mode import ModeTable
from .utils import const_property

from collections import namedtuple

Prefix = namedtuple('Prefix', ('encoding', 'semantics'))

PrefixMapping = namedtuple('PrefixMapping', (
    'initCode', 'refForFlag', 'prefixForFlag', 'encodingWidth'
    ))

class PrefixMappingFactory:

    def __init__(self, namespace):
        self._namespace = namespace
        self._initBuilder = SemanticsCodeBlockBuilder()
        self._refForFlag = {}
        self._prefixForFlag = {}
        self._encodingWidth = None

    def hasFlag(self, name):
        '''Return True iff a decode flag with the given name was added to
        this factory.
        '''
        return name in self._refForFlag

    def addPrefixes(self, decodeFlags, prefixes):
        '''Adds the given prefixes (sequence of Prefix objects), which use the
        given decode flags (sequence of names), to this mapping.
        Raises KeyError if a decode flag name either does not exist in the
        namespace or was added more than once.
        Raises ValueError if no reverse mapping could be computed from the
        given prefix semantics.
        Raises BadInput if an encoding item's width is inconsistent with
        earlier encoding item widths.
        '''
        # Check encoding width consistency.
        encWidth = self._encodingWidth
        for prefix in prefixes:
            for encItem in prefix.encoding:
                if encWidth is None:
                    encWidth = encItem.encodingWidth
                elif encWidth != encItem.encodingWidth:
                    raise BadInput(
                        'encoding item has width %s while previous item(s) '
                        'have width %s' % (encItem.encodingWidth, encWidth),
                        encItem.location
                        )
        self._encodingWidth = encWidth

        # Collect decode flag variables, build init code for them.
        builder = self._initBuilder
        namespace = self._namespace
        refForFlag = self._refForFlag
        zero = IntLiteral(0)
        decodeVars = {}
        for name in decodeFlags:
            ref = namespace[name]
            if name in refForFlag:
                raise KeyError('decode flag redefined: %s' % name)
            refForFlag[name] = ref
            decodeVars[ref.bits.storage] = name
            ref.emitStore(builder, zero, None)

        # Figure out which prefix sets which flag.
        prefixForFlag = self._prefixForFlag
        for prefix in prefixes:
            setFlags = set()
            for node in prefix.semantics.nodes:
                if isinstance(node, Store):
                    value = node.expr
                    if isinstance(value, IntLiteral):
                        if value.value == 0:
                            continue
                        assert value.value == 1, value.value
                    else:
                        raise ValueError('non-literal assigned to decode flag')
                    name = decodeVars[node.storage]
                    setFlags.add(name)
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

    def createMapping(self):
        '''Create a PrefixMapping using the prefixes added so far.
        '''
        return PrefixMapping(
            self._initBuilder.createCodeBlock(()),
            dict(self._refForFlag),
            dict(self._prefixForFlag),
            self._encodingWidth
            )

class InstructionSet(ModeTable):
    '''Contains all definitions for a processor's instruction set.
    '''

    globalNamespace = property(lambda self: self._globalNamespace)

    def __init__(
            self, encWidth, auxEncWidth, globalNamespace, prefixMapping,
            instructions
            ):
        if auxEncWidth not in (encWidth, None):
            raise ValueError(
                'auxiliary encoding width must be None or equal to base '
                'encoding width %s, got %s instead' % (encWidth, auxEncWidth)
                )
        if prefixMapping.encodingWidth not in (None, encWidth):
            raise ValueError(
                'prefix encoding width %s is different from instruction '
                'encoding width %s' % (prefixMapping.encodingWidth, encWidth)
                )
        instructions = tuple(instructions)
        ModeTable.__init__(
            self, encWidth, auxEncWidth,
            (instr.entry for instr in instructions)
            )
        self._globalNamespace = globalNamespace
        self._prefixMapping = prefixMapping
        self.decoder = createDecoderFromParsedEntries(instructions)

    @property
    def addrWidth(self):
        '''The width of the program counter, in bits.
        '''
        return self._globalNamespace['pc'].width

    @const_property
    def instructionNames(self):
        '''A set containing the instruction names (operations).
        '''
        return self._mnemTree[0].keys()

ParsedModeEntry = namedtuple('ParsedModeEntry', (
    'entry', 'decoding', 'flagsRequired'
    ))

def createDecoderFromParsedEntries(parsedEntries):
    return createDecoder(
        createEntryDecoder(parsedEntry.entry, parsedEntry.decoding)
        for parsedEntry in parsedEntries
        # TODO: Add real prefix support.
        if not parsedEntry.flagsRequired
        )
