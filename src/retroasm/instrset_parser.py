from __future__ import annotations

from collections import defaultdict
from collections.abc import Callable, Iterable, Iterator, Mapping, Sequence
from importlib.abc import Traversable
from logging import WARNING, Logger, getLogger
from typing import AbstractSet, DefaultDict, cast
import re

from .codeblock import CodeBlock
from .codeblock_builder import SemanticsCodeBlockBuilder, StatelessCodeBlockBuilder
from .context_parser import MatchPlaceholderSpec, PlaceholderSpec, ValuePlaceholderSpec
from .decode import (
    EncodedSegment,
    FixedEncoding,
    ParsedModeEntry,
    Prefix,
    decomposeEncoding,
)
from .expression_builder import (
    BadExpression,
    UnknownNameError,
    buildExpression,
    buildReference,
    buildStatementEval,
    convertDefinition,
)
from .expression_nodes import (
    DeclarationNode,
    DefinitionNode,
    FlagTestNode,
    IdentifierNode,
    MultiMatchNode,
    ParseNode,
    parseInt,
)
from .expression_parser import (
    parseContext,
    parseExpr,
    parseExprList,
    parseRegs,
    parseStatement,
)
from .function_builder import createFunc
from .instrset import InstructionSet, PrefixMappingFactory
from .linereader import BadInput, DefLineReader, DelayedError, InputLocation, merge_span
from .mode import (
    CodeTemplate,
    ComputedPlaceholder,
    Encoding,
    EncodingExpr,
    EncodingItem,
    EncodingMultiMatch,
    MatchPlaceholder,
    MnemItem,
    Mnemonic,
    Mode,
    ModeEntry,
    ValuePlaceholder,
)
from .namespace import (
    ContextNamespace,
    GlobalNamespace,
    LocalNamespace,
    NameExistsError,
    Namespace,
)
from .reference import Reference, bad_reference, int_reference
from .storage import ArgStorage, IOChannel, IOStorage, Variable
from .types import IntType, ReferenceType, Width, parse_type, parse_type_decl
from .utils import bad_type

_namePat = r"[A-Za-z_][A-Za-z0-9_]*'?"
_nameTok = r"\s*(" + _namePat + r")\s*"
_typeTok = r"\s*(" + _namePat + r"&?)\s*"

_reDotSep = re.compile(r"\s*\.\s*")


def _parseRegs(
    reader: DefLineReader, args: InputLocation, globalNamespace: GlobalNamespace
) -> None:
    if args:
        reader.error("register definition must have no arguments", location=args)

    for line in reader.iter_block():
        try:
            nodes = parseRegs(line)
        except BadInput as ex:
            reader.error("bad register definition line: %s", ex, location=ex.locations)
            continue

        lastTypeLocation = None
        for node in nodes:
            # Parse type declaration.
            decl = node if isinstance(node, DeclarationNode) else node.decl
            declType = decl.type
            assert declType is not None
            typeLocation = declType.location
            try:
                regType = parse_type_decl(declType.name)
            except ValueError as ex:
                # Avoid reporting the same error twice.
                if typeLocation != lastTypeLocation:
                    reader.error("%s", ex, location=typeLocation)
                continue
            lastTypeLocation = typeLocation

            match node:
                case DeclarationNode():
                    # Define base register.
                    match regType:
                        case ReferenceType():
                            reader.error(
                                "base register cannot have a reference type",
                                location=(decl.name.location, typeLocation),
                            )
                        case typ:
                            try:
                                globalNamespace.addVariable(
                                    decl.name.name, typ, decl.name.location
                                )
                            except NameExistsError as ex:
                                reader.error(
                                    "bad base register definition: %s",
                                    ex,
                                    location=ex.locations,
                                )
                case DefinitionNode(value=value):
                    # Define register alias.
                    name = decl.name.name
                    try:
                        ref = convertDefinition(
                            decl.kind, name, regType, value, globalNamespace
                        )
                    except BadExpression as ex:
                        reader.error(
                            "bad register alias: %s", ex, location=ex.locations
                        )
                        ref = bad_reference(regType)
                    try:
                        globalNamespace.define(name, ref, decl.name.location)
                    except NameExistsError as ex:
                        reader.error(
                            "failed to define register alias: %s",
                            ex,
                            location=ex.locations,
                        )
                case node:
                    bad_type(node)


_reCommaSep = re.compile(r"\s*,\s*")
_reArgDecl = re.compile(_typeTok + r"\s" + _nameTok + r"$")


def _parse_typedArgs(
    reader: DefLineReader, args: InputLocation, description: str
) -> Iterator[tuple[IntType | ReferenceType, InputLocation, InputLocation]]:
    """
    Parses a typed arguments list, yielding a triple for each argument,
    containing the argument type and InputLocations for the type and name.
    Errors are logged on the given reader as they are discovered.
    """
    argLocs = tuple(args.split(_reCommaSep))
    if len(argLocs) == 1:
        # Arg list contains no separators; do we have 0 or 1 argument(s)?
        if len(args) == 0 or args.text.isspace():
            return

    for i, argLoc in enumerate(argLocs, 1):
        argMatch = argLoc.match(_reArgDecl)
        if argMatch is None:
            reader.error(
                '%s %d not of the form "<type> <name>"', description, i, location=argLoc
            )
            continue

        typeLoc, nameLoc = argMatch.groups
        try:
            argType = parse_type_decl(typeLoc.text)
        except ValueError as ex:
            reader.error(
                'bad %s %d ("%s"): %s',
                description,
                i,
                nameLoc.text,
                ex,
                location=typeLoc,
            )
        else:
            yield argType, typeLoc, nameLoc


def _parsePrefix(
    reader: DefLineReader,
    args: InputLocation,
    namespace: GlobalNamespace,
    factory: PrefixMappingFactory,
) -> None:
    headerLocation = reader.location

    # Parse header line.
    decodeFlags = []
    try:
        with reader.check_errors():
            flagType = IntType.u(1)
            for argType, argTypeLoc, argNameLoc in _parse_typedArgs(
                reader, args, "decode flag"
            ):
                match argType:
                    case ReferenceType():
                        reader.error(
                            "decode flag cannot be declared as a reference type",
                            location=argTypeLoc,
                        )
                        continue
                    case IntType() as typ:
                        if typ is not flagType:
                            # Maybe in the future we'll support other types.
                            reader.error(
                                'decode flag of type "%s", expected "%s"',
                                argType,
                                flagType,
                                location=argTypeLoc,
                            )
                    case typ:
                        bad_type(typ)
                argName = argNameLoc.text
                try:
                    namespace.addVariable(argName, argType, argNameLoc)
                except ValueError as ex:
                    reader.error(str(ex))
                except NameExistsError as ex:
                    reader.error(
                        "error defining decode flag: %s", ex, location=ex.locations
                    )
                else:
                    decodeFlags.append(argName)
    except DelayedError:
        reader.skip_block()
        return

    # Parse body.
    prefixes = []
    for line in reader.iter_block():
        # Split line into 3 fields.
        fields = tuple(line.split(_reDotSep))
        try:
            encLoc, mnemLoc, semLoc = fields
        except ValueError:
            reader.error(
                "wrong number of dot-separated fields in prefix line: "
                "expected 3, got %d",
                len(fields),
            )
            continue

        # Parse encoding.
        try:
            with reader.check_errors():
                if len(encLoc) == 0:
                    reader.error("prefix encoding cannot be empty", location=encLoc)
                else:
                    try:
                        encNodes = parseExprList(encLoc)
                    except BadInput as ex:
                        reader.error(
                            "bad prefix encoding: %s", ex, location=ex.locations
                        )
                    else:
                        encItems = []
                        for encNode in encNodes:
                            try:
                                encItems.append(
                                    _parseEncodingExpr(encNode, namespace, {})
                                )
                            except BadInput as ex:
                                reader.error(
                                    "bad prefix encoding: %s", ex, location=ex.locations
                                )
        except DelayedError:
            encoding = None
        else:
            encoding = Encoding(encItems, encLoc)

        # Parse mnemonic.
        if len(mnemLoc) != 0:
            reader.warning("prefix mnemonics are not supported yet", location=mnemLoc)

        # Parse semantics.
        semantics: CodeBlock | None
        try:
            with reader.check_errors():
                if len(semLoc) == 0:
                    reader.error(
                        'prefix semantics cannot be empty; use "nop" instead',
                        location=semLoc,
                    )
                else:
                    semBuilder = SemanticsCodeBlockBuilder()
                    semNamespace = LocalNamespace(namespace, semBuilder)
                    try:
                        _parseInstrSemantics(reader, semLoc, semNamespace)
                    except BadInput as ex:
                        reader.error(
                            "bad prefix semantics: %s", ex, location=ex.locations
                        )
                    else:
                        try:
                            semantics = semNamespace.createCodeBlock(
                                retRef=None, log=reader
                            )
                        except ValueError:
                            # Error was logged inside createCodeBlock().
                            pass
        except DelayedError:
            semantics = None

        if encoding is not None and semantics is not None:
            prefixes.append(Prefix(encoding, semantics))

    try:
        factory.addPrefixes(decodeFlags, prefixes)
    except ValueError as ex:
        reader.error(
            "validation of prefix block failed: %s", ex, location=headerLocation
        )
    except BadInput as ex:
        reader.error("validation of prefix block failed: %s", ex, location=ex.locations)


_reIOLine = re.compile(_nameTok + r"\s" + _nameTok + r"\[" + _nameTok + r"\]$")


def _parseIO(
    reader: DefLineReader, args: InputLocation, namespace: GlobalNamespace
) -> None:
    if args:
        reader.error("I/O definition must have no arguments", location=args)

    for line in reader.iter_block():
        match = line.match(_reIOLine)
        if match is None:
            reader.error("invalid I/O definition line")
        else:
            elemTypeLoc, nameLoc, addrTypeLoc = match.groups

            try:
                elemType: IntType | None = parse_type(elemTypeLoc.text)
            except ValueError as ex:
                reader.error("bad I/O element type: %s", ex, location=elemTypeLoc)
                elemType = None

            try:
                addrType: IntType | None = parse_type(addrTypeLoc.text)
            except ValueError as ex:
                reader.error("bad I/O address type: %s", ex, location=addrTypeLoc)
                addrType = None

            if elemType is None or addrType is None:
                continue

            name = nameLoc.text
            channel = IOChannel(name, elemType, addrType)
            try:
                namespace.define(name, channel, nameLoc)
            except NameExistsError as ex:
                reader.error(
                    "error defining I/O channel: %s", ex, location=ex.locations
                )


_reFuncHeader = re.compile(r"(?:" + _typeTok + r"\s)?" + _nameTok + r"\((.*)\)$")


def _parseFunc(
    reader: DefLineReader,
    headerArgs: InputLocation,
    namespace: GlobalNamespace,
    wantSemantics: bool,
) -> None:
    # Parse header line.
    match = headerArgs.match(_reFuncHeader)
    if match is None:
        reader.error("invalid function header line", location=headerArgs)
        reader.skip_block()
        return

    # Parse return type.
    retType: IntType | ReferenceType | None
    retTypeLoc: InputLocation | None
    if match.has_group(1):
        retTypeLoc = match.group(1)
        try:
            retType = parse_type_decl(retTypeLoc.text)
        except ValueError as ex:
            reader.error("bad return type: %s", ex, location=retTypeLoc)
            reader.skip_block()
            return
    else:
        retTypeLoc = None
        retType = None

    # Parse arguments.
    args = {}
    try:
        with reader.check_errors():
            nameLocations: dict[str, InputLocation] = {}
            for i, (argType, argTypeLoc_, argNameLoc) in enumerate(
                _parse_typedArgs(reader, match.group(3), "function argument"), 1
            ):
                argName = argNameLoc.text
                if argName == "ret":
                    reader.error(
                        '"ret" is reserved for the return value; '
                        "it cannot be used as an argument name",
                        location=argNameLoc,
                    )
                elif argName in nameLocations:
                    reader.error(
                        "function argument %d has the same name as "
                        "an earlier argument: %s",
                        i,
                        argName,
                        location=(argNameLoc, nameLocations[argName]),
                    )
                else:
                    args[argName] = argType
                    nameLocations[argName] = argNameLoc
    except DelayedError:
        reader.skip_block()
        return

    if wantSemantics:
        funcNameLoc = match.group(2)
        funcName = funcNameLoc.text

        # Parse body lines.
        func = createFunc(
            reader, funcNameLoc, retType, retTypeLoc, args, nameLocations, namespace
        )

        # Store function in namespace.
        try:
            namespace.define(funcName, func, funcNameLoc)
        except NameExistsError as ex:
            reader.error("error declaring function: %s", ex, location=ex.locations)
    else:
        reader.skip_block()


def _parseModeContext(
    ctxLoc: InputLocation,
    prefixes: PrefixMappingFactory,
    modes: Mapping[str, Mode],
    reader: DefLineReader,
) -> tuple[Mapping[str, PlaceholderSpec], set[str]]:
    placeholderSpecs = {}
    flagsRequired = set()
    for node in parseContext(ctxLoc):
        match node:
            case (DeclarationNode() as decl) | DefinitionNode(decl=decl):
                name = decl.name.name
                declType = decl.type
                assert declType is not None
                typeName = declType.name

                # Figure out whether the name is a mode or type.
                mode = modes.get(typeName)
                placeholder: PlaceholderSpec
                if mode is not None:
                    placeholder = MatchPlaceholderSpec(decl, mode)
                    if isinstance(node, DefinitionNode):
                        reader.error(
                            "filter values for mode placeholders are not supported yet",
                            location=merge_span(
                                node.location, node.value.tree_location
                            ),
                        )
                else:
                    try:
                        # TODO: While the documentation says we do support defining
                        #       references in the context, parse_type() rejects
                        #       "<type>&"; we'd have to use parse_type_decl() instead.
                        typ = parse_type(typeName)
                    except ValueError:
                        reader.error(
                            'there is no type or mode named "%s"',
                            typeName,
                            location=declType.location,
                        )
                        continue
                    value = node.value if isinstance(node, DefinitionNode) else None
                    placeholder = ValuePlaceholderSpec(decl, typ, value)
                if name in placeholderSpecs:
                    reader.error(
                        'multiple placeholders named "%s"',
                        name,
                        location=decl.name.location,
                    )
                else:
                    placeholderSpecs[name] = placeholder
            case FlagTestNode(name=name):
                if prefixes.hasFlag(name):
                    flagsRequired.add(name)
                else:
                    reader.error(
                        'there is no decode flag named "%s"',
                        name,
                        location=node.location,
                    )
            case node:
                bad_type(node)

    return placeholderSpecs, flagsRequired


def _buildPlaceholders(
    placeholderSpecs: Mapping[str, PlaceholderSpec],
    globalNamespace: GlobalNamespace,
    reader: DefLineReader,
) -> Iterator[MatchPlaceholder | ValuePlaceholder]:
    """Create placeholders from a spec."""
    semNamespace = ContextNamespace(globalNamespace)

    for name, spec in placeholderSpecs.items():
        decl = spec.decl
        semType = spec.type
        value = spec.value

        code = None
        if semType is not None and value is not None:
            placeholderNamespace = LocalNamespace(
                semNamespace, SemanticsCodeBlockBuilder()
            )
            try:
                ref = convertDefinition(
                    decl.kind, decl.name.name, semType, value, placeholderNamespace
                )
            except BadExpression as ex:
                reader.error("%s", ex, location=ex.locations)
            else:
                code = placeholderNamespace.createCodeBlock(ref)

        if semType is not None:
            match semType:
                case ReferenceType(type=argType) | (IntType() as argType):
                    try:
                        semNamespace.addArgument(name, argType, decl.name.location)
                    except NameExistsError as ex:
                        reader.error("%s", ex, location=ex.locations)
                        continue
                case typ:
                    bad_type(typ)

        match spec:
            case ValuePlaceholderSpec():
                if semType is not None:
                    # TODO: We don't actually support references types yet.
                    #       See TODO in _parseModeContext().
                    assert isinstance(semType, IntType), semType
                    if code is None:
                        yield ValuePlaceholder(name, semType)
                    else:
                        yield ComputedPlaceholder(name, semType, code)
            case MatchPlaceholderSpec(mode=mode):
                yield MatchPlaceholder(name, mode)
            case spec:
                bad_type(spec)


def _parseEncodingExpr(
    encNode: ParseNode,
    encNamespace: Namespace,
    placeholderSpecs: Mapping[str, PlaceholderSpec],
) -> EncodingExpr:
    """
    Parse encoding node that is not a MultiMatchNode.
    Returns the parse result as an EncodingExpr.
    Raises BadInput if the node is invalid.
    """
    namespace = LocalNamespace(encNamespace, SemanticsCodeBlockBuilder())
    try:
        encRef = buildReference(encNode, namespace)
    except BadInput as ex:
        if isinstance(ex, UnknownNameError):
            spec = placeholderSpecs.get(ex.name)
            if spec is not None:
                if spec.encodingWidth is None:
                    # Only MatchPlaceholderSpec.encodingWidth can return None.
                    assert isinstance(spec, MatchPlaceholderSpec), spec
                    raise BadInput(
                        f'cannot use placeholder "{ex.name}" '
                        f'in encoding field, since mode "{spec.mode.name}" '
                        f"has an empty encoding sequence",
                        *ex.locations,
                        spec.decl.tree_location,
                    ) from None
                if spec.value is not None:
                    raise BadInput(
                        f'cannot use placeholder "{ex.name}" '
                        f"in encoding field, since its value is "
                        f"computed in the context",
                        *ex.locations,
                        spec.value.tree_location,
                    ) from None
        raise BadInput(
            f"error in encoding: {ex}", encNode.tree_location, *ex.locations
        ) from ex

    code = namespace.builder.createCodeBlock((encRef.bits,))
    if len(code.nodes) != 0:
        raise BadInput(
            "encoding expression accesses state or performs I/O", encNode.tree_location
        )
    (encBits,) = code.returned
    for storage in encBits.iter_storages():
        match storage:
            case Variable():
                raise BadInput(
                    "encoding expression references register", encNode.tree_location
                )
            case IOStorage(channel=channel):
                raise BadInput(
                    f"encoding expression references storage location "
                    f'on I/O channel "{channel.name}"',
                    encNode.tree_location,
                )
    return EncodingExpr(encBits, encNode.tree_location)


def _parseMultiMatch(
    encNode: MultiMatchNode,
    identifiers: AbstractSet[str],
    placeholderSpecs: Mapping[str, PlaceholderSpec],
) -> EncodingMultiMatch:
    """
    Parse an encoding node of type MultiMatchNode.
    Returns the parse result as an EncodingMultiMatch.
    Raises BadInput if the node is invalid.
    """
    name = encNode.name
    try:
        placeholder = placeholderSpecs[name]
    except KeyError:
        raise BadInput(
            f'placeholder "{name}" does not exist in context', encNode.tree_location
        ) from None
    if not isinstance(placeholder, MatchPlaceholderSpec):
        raise BadInput(
            f'placeholder "{name}" does not represent a mode match',
            encNode.tree_location,
            placeholder.decl.tree_location,
        )

    mode = placeholder.mode
    start = 1 if name in identifiers else 0
    return EncodingMultiMatch(name, mode, start, encNode.tree_location)


def _parseModeEncoding(
    encNodes: Iterable[ParseNode],
    placeholderSpecs: Mapping[str, PlaceholderSpec],
    globalNamespace: GlobalNamespace,
    logger: DefLineReader,
) -> Iterator[EncodingItem]:
    # Define placeholders in encoding namespace.
    encNamespace = ContextNamespace(globalNamespace)
    for name, spec in placeholderSpecs.items():
        if spec.value is None:
            encWidth = spec.encodingWidth
            if encWidth is not None:
                encType = IntType.u(encWidth)
                location = spec.decl.name.location
                try:
                    encNamespace.addArgument(name, encType, location)
                except NameExistsError as ex:
                    logger.error("bad placeholder: %s", ex, location=ex.locations)

    # Collect the names of all identifiers used in the encoding.
    identifiers = {
        subNode.name
        for encNode in encNodes
        for subNode in encNode
        if isinstance(subNode, IdentifierNode)
    }

    # Evaluate encoding field.
    for encNode in encNodes:
        try:
            if isinstance(encNode, MultiMatchNode):
                # Match multiple encoding fields as-is.
                yield _parseMultiMatch(encNode, identifiers, placeholderSpecs)
            else:
                # Expression possibly containing single encoding field matches.
                yield _parseEncodingExpr(encNode, encNamespace, placeholderSpecs)
        except BadInput as ex:
            logger.error("%s", ex, location=ex.locations)


def _checkEmptyMultiMatches(
    encItems: Iterable[EncodingItem],
    placeholderSpecs: Mapping[str, PlaceholderSpec],
    logger: DefLineReader,
) -> None:
    """
    Warn about multi-matches that always match zero elements.
    Technically there is nothing wrong with those, but it is probably not what
    the user intended.
    """
    for encItem in encItems:
        match encItem:
            case EncodingMultiMatch(mode=mode):
                if mode.encodingWidth is None:
                    logger.warning(
                        'mode "%s" does not contain encoding elements',
                        mode.name,
                        location=(
                            encItem.location,
                            placeholderSpecs[encItem.name].decl.tree_location,
                        ),
                    )
                elif encItem.start >= 1 and mode.auxEncodingWidth is None:
                    logger.warning(
                        'mode "%s" does not match auxiliary encoding units',
                        mode.name,
                        location=(
                            encItem.location,
                            placeholderSpecs[encItem.name].decl.tree_location,
                        ),
                    )


def _checkMissingPlaceholders(
    encItems: Iterable[EncodingItem],
    placeholderSpecs: Mapping[str, PlaceholderSpec],
    location: InputLocation,
    logger: DefLineReader,
) -> None:
    """
    Check that our encoding field contains sufficient placeholders to be able
    to make matches in all included mode tables.
    """
    # Take inventory of placeholders in the encoding.
    identifiers = set()
    multiMatches = set()
    for encItem in encItems:
        match encItem:
            case EncodingExpr(bits=bits):
                for storage in bits.iter_storages():
                    if isinstance(storage, ArgStorage):
                        identifiers.add(storage.name)
            case EncodingMultiMatch(name=name):
                multiMatches.add(name)
            case item:
                bad_type(item)

    # Check whether all placeholders from the context occur in the encoding.
    for name, spec in placeholderSpecs.items():
        match spec:
            case ValuePlaceholderSpec(value=value, decl=decl):
                if value is not None:
                    # The value is computed, so we don't need to encode it.
                    continue
                if name not in identifiers:
                    logger.error(
                        'value placeholder "%s" does not occur in encoding',
                        name,
                        location=(location, decl.tree_location),
                    )
            case MatchPlaceholderSpec(mode=mode, decl=decl):
                if mode.encodingWidth is None:
                    # Mode has empty encoding, no match needed.
                    continue
                if name in multiMatches:
                    # Mode is matched using "X@" syntax.
                    continue
                if name not in identifiers:
                    logger.error(
                        'no placeholder "%s" for mode "%s" in encoding',
                        name,
                        mode.name,
                        location=(location, decl.tree_location),
                    )
                if mode.auxEncodingWidth is not None:
                    logger.error(
                        'mode "%s" matches auxiliary encoding units, but there '
                        'is no "%s@" placeholder for them',
                        mode.name,
                        name,
                        location=(location, decl.tree_location),
                    )
            case spec:
                bad_type(spec)


def _checkAuxEncodingWidth(
    encItems: Iterable[EncodingItem], logger: DefLineReader
) -> None:
    """
    Check whether the encoding widths in the given encoding are the same
    for all auxiliary encoding items.
    Violations are logged as errors on the given logger.
    """
    first_aux_width: Width | None = None
    first_aux_location: InputLocation | None = None

    def checkAux(width: Width | None, location: InputLocation) -> None:
        nonlocal first_aux_width, first_aux_location
        if first_aux_width is None:
            first_aux_width = width
            first_aux_location = location
        elif width != first_aux_width:
            logger.error(
                "encoding item matches width %s, while first auxiliary "
                "encoding match has width %s",
                width,
                first_aux_width,
                location=(location, first_aux_location),
            )

    first = True
    for encItem in encItems:
        match encItem:
            case EncodingExpr() if first:
                pass
            case EncodingExpr(encodingWidth=enc_width, location=loc):
                checkAux(enc_width, loc)
            case EncodingMultiMatch(
                mode=Mode(auxEncodingWidth=aux_width),
                encodedLength=enc_len,
                location=loc,
            ) if first:
                if enc_len != 1 and aux_width is not None:
                    checkAux(aux_width, loc)
            case EncodingMultiMatch(
                mode=Mode(encodingWidth=enc_width, auxEncodingWidth=aux_width),
                start=start,
                location=loc,
            ):
                if start == 0:
                    checkAux(enc_width, loc)
                if aux_width is not None:
                    checkAux(aux_width, loc)
            case item:
                bad_type(item)
        first = False


def _checkDuplicateMultiMatches(
    encItems: Iterable[EncodingItem], logger: DefLineReader
) -> None:
    """
    Checks whether more than one multi-matcher exists for the same
    placeholder. If they exist, they are reported as errors on the given logger.
    """
    claimedMultiMatches: dict[str, InputLocation] = {}
    for encItem in encItems:
        match encItem:
            case EncodingMultiMatch(name=name, location=location):
                if name in claimedMultiMatches:
                    logger.error(
                        'duplicate multi-match placeholder "%s@"',
                        name,
                        location=(location, claimedMultiMatches[name]),
                    )
                else:
                    claimedMultiMatches[name] = location


def _combinePlaceholderEncodings(
    decodeMap: Mapping[str, Sequence[tuple[int, EncodedSegment]]],
    placeholderSpecs: Mapping[str, PlaceholderSpec],
    reader: DefLineReader,
) -> Iterator[tuple[str, Sequence[EncodedSegment]]]:
    """
    Yield pairs of placeholder name and the locations where the placeholder
    resides in the encoded items.
    Each such location is a triple of index in the encoded items, bit offset
    within that item and width in bits.
    """
    for name, slices in decodeMap.items():
        placeholderSpec = placeholderSpecs[name]
        immWidth = placeholderSpec.encodingWidth
        # TODO: Can this actually never happen?
        assert immWidth is not None, placeholderSpec
        decoding = []
        problems = []
        prev: Width = 0
        for immIdx, encSegment in sorted(slices):
            width = encSegment.segment.width
            if prev < immIdx:
                problems.append(f"gap at [{prev:d}:{immIdx:d}]")
            elif prev > immIdx:
                problems.append(f"overlap at [{immIdx:d}:{min(immIdx + width, prev)}]")
            prev = max(immIdx + width, prev)
            decoding.append(encSegment)
        if prev < immWidth:
            problems.append(f"gap at [{prev:d}:{immWidth:d}]")
        elif prev > immWidth:
            assert False, (name, slices)
        if problems:
            reader.error(
                'cannot decode value for "%s": %s',
                name,
                ", ".join(problems),
                location=placeholderSpec.decl.tree_location,
            )
        else:
            yield name, tuple(decoding)


def _checkDecodingOrder(
    encoding: Encoding,
    sequentialMap: Mapping[str, Sequence[EncodedSegment]],
    placeholderSpecs: Mapping[str, PlaceholderSpec],
    reader: DefLineReader,
) -> None:
    """
    Verifies that there is an order in which placeholders can be decoded.
    Such an order might not exist because of circular dependencies.
    """
    # Find indices of multi-matches.
    multiMatchIndices = {
        encElem.name: encIdx
        for encIdx, encElem in enumerate(encoding)
        if isinstance(encElem, EncodingMultiMatch)
    }

    for name, decoding in sequentialMap.items():
        # Are we dealing with a multi-match of unknown length?
        placeholderSpec = placeholderSpecs[name]
        if not isinstance(placeholderSpec, MatchPlaceholderSpec):
            continue
        multiIdx = multiMatchIndices.get(name)
        if multiIdx is None:
            continue
        matcher = encoding[multiIdx]
        if matcher.encodedLength is not None:
            continue

        # Are any parts of the placeholder are located after the multi-match?
        badIdx = [
            encEegment.encIdx for encEegment in decoding if encEegment.encIdx > multiIdx
        ]
        if badIdx:
            mode = placeholderSpec.mode
            reader.error(
                'cannot match "%s": mode "%s" has a variable encoding length '
                'and (parts of) the placeholder "%s" are placed after the '
                'multi-match placeholder "%s@"',
                name,
                mode.name,
                name,
                name,
                location=[placeholderSpec.decl.tree_location, matcher.location]
                + [encoding[idx].location for idx in badIdx],
            )


def _parseModeDecoding(
    encoding: Encoding,
    placeholderSpecs: Mapping[str, PlaceholderSpec],
    reader: DefLineReader,
) -> tuple[Sequence[FixedEncoding], Mapping[str, Sequence[EncodedSegment]]] | None:
    """
    Construct a mapping that, given an encoded instruction, produces the
    values for context placeholders.
    """
    try:
        # Decompose the encoding expressions.
        fixedMatcher, decodeMap = decomposeEncoding(encoding)
    except BadInput as ex:
        reader.error("%s", ex, location=ex.locations)
        return None
    try:
        with reader.check_errors():
            # Create a mapping to extract immediate values from encoded items.
            sequentialMap = dict(
                _combinePlaceholderEncodings(decodeMap, placeholderSpecs, reader)
            )
        with reader.check_errors():
            # Check whether unknown-length multi-matches are blocking decoding.
            _checkDecodingOrder(encoding, sequentialMap, placeholderSpecs, reader)
    except DelayedError:
        return None
    else:
        return fixedMatcher, sequentialMap


def _parseModeSemantics(
    # While 'reader' is not used here, it is part of the signature that
    # _parseModeEntries() uses to call this function.
    reader: DefLineReader,  # pylint: disable=unused-argument
    semLoc: InputLocation,
    semNamespace: LocalNamespace,
    modeType: None | IntType | ReferenceType,
) -> Reference | None:
    semantics = parseExpr(semLoc)
    if isinstance(modeType, ReferenceType):
        ref = buildReference(semantics, semNamespace)
        if ref.type != modeType.type:
            raise BadInput(
                f"semantics type {ref.type} does not match mode type {modeType.type}",
                semLoc,
            )
        semNamespace.define("ret", ref, semLoc)
        return ref
    else:
        expr = buildExpression(semantics, semNamespace)
        # Note that modeType can be None because of earlier errors.
        if modeType is None:
            return None
        ref = semNamespace.addVariable("ret", modeType, semLoc)
        ref.emit_store(semNamespace.builder, expr, semLoc)
        return ref


def _parseInstrSemantics(
    reader: DefLineReader,
    semLoc: InputLocation,
    namespace: LocalNamespace,
    modeType: None | IntType | ReferenceType = None,
) -> None:
    assert modeType is None, modeType
    node = parseStatement(semLoc)
    buildStatementEval(reader, "semantics field", namespace, node)


_reMnemonic = re.compile(r"\w+'?|[$%]\w+|[^\w\s]")


def _parseMnemonic(
    mnemLoc: InputLocation,
    placeholders: Iterable[MatchPlaceholder | ValuePlaceholder],
    reader: DefLineReader,
) -> Iterator[MnemItem]:
    placeholder_map = {p.name: p for p in placeholders}
    seenPlaceholders: dict[str, InputLocation] = {}
    for mnemElem in mnemLoc.find_locations(_reMnemonic):
        text = mnemElem.text
        placeholder = placeholder_map.get(text)
        if placeholder is None:
            if "0" <= text[0] <= "9" or text[0] in "$%":
                try:
                    value, width = parseInt(text)
                except ValueError as ex:
                    reader.error("%s", ex, location=mnemElem)
                else:
                    yield int_reference(value, IntType.u(width))
            else:
                yield text
        elif text in seenPlaceholders:
            # In theory we could support repeated placeholders, but the only
            # meaning that would make sense is that they would all match the
            # same mode entry or expression and I don't know of any situation
            # in which that would be a useful feature.
            reader.error(
                'placeholder "%s" occurs multiple times in mnemonic',
                text,
                location=(mnemElem, seenPlaceholders[text]),
            )
        else:
            yield placeholder
            seenPlaceholders[text] = mnemElem


def _parseModeEntries(
    reader: DefLineReader,
    globalNamespace: GlobalNamespace,
    prefixes: PrefixMappingFactory,
    modes: Mapping[str, Mode],
    modeType: None | IntType | ReferenceType,
    mnemBase: tuple[str, ...],
    parseSem: Callable[
        [DefLineReader, InputLocation, LocalNamespace, None | IntType | ReferenceType],
        Reference | None,
    ],
    wantSemantics: bool,
) -> Iterator[ParsedModeEntry]:
    for line in reader.iter_block():
        # Split mode line into 4 fields.
        fields = list(line.split(_reDotSep))
        if len(fields) < 2:
            reader.error('field separator "." missing in mode line')
            continue
        if len(fields) > 4:
            reader.error("too many fields (%d) in mode line", len(fields))
            continue
        fields += [line.end_location] * (4 - len(fields))
        encLoc, mnemLoc, semLoc, ctxLoc = fields

        try:
            with reader.check_errors():
                # Parse context.
                if len(ctxLoc) != 0:
                    try:
                        with reader.check_errors():
                            placeholderSpecs, flagsRequired = _parseModeContext(
                                ctxLoc, prefixes, modes, reader
                            )
                            placeholders = tuple(
                                _buildPlaceholders(
                                    placeholderSpecs, globalNamespace, reader
                                )
                            )
                    except DelayedError:
                        # To avoid error spam, skip this line.
                        continue
                else:
                    placeholderSpecs, flagsRequired = {}, set()
                    placeholders = ()

                # Parse encoding.
                encNodes: Iterable[ParseNode] | None
                if len(encLoc) != 0:
                    try:
                        # Parse encoding field.
                        encNodes = parseExprList(encLoc)
                    except BadInput as ex:
                        reader.error("error in encoding: %s", ex, location=ex.locations)
                        encNodes = None
                else:
                    encNodes = ()
                if encNodes is None:
                    encoding = None
                else:
                    try:
                        with reader.check_errors():
                            encItems = tuple(
                                _parseModeEncoding(
                                    encNodes, placeholderSpecs, globalNamespace, reader
                                )
                            )
                        with reader.check_errors():
                            _checkAuxEncodingWidth(encItems, reader)
                            _checkEmptyMultiMatches(encItems, placeholderSpecs, reader)
                            _checkDuplicateMultiMatches(encItems, reader)
                            _checkMissingPlaceholders(
                                encItems, placeholderSpecs, encLoc, reader
                            )
                    except DelayedError:
                        encoding = None
                    else:
                        encoding = Encoding(encItems, encLoc)
                if encoding is None:
                    decoding = None
                else:
                    decoding = _parseModeDecoding(encoding, placeholderSpecs, reader)

                # Parse mnemonic.
                mnemItems = mnemBase + tuple(
                    _parseMnemonic(mnemLoc, placeholders, reader)
                )
                if len(mnemItems) == 0:
                    reader.error("missing mnemonic", location=mnemLoc)
                else:
                    mnemonic = Mnemonic(mnemItems)

                # Parse semantics.
                if wantSemantics:
                    semBuilder = SemanticsCodeBlockBuilder()
                    semNamespace = LocalNamespace(globalNamespace, semBuilder)
                    try:
                        # Define placeholders in semantics builder.
                        for name, spec in placeholderSpecs.items():
                            match spec.type:
                                case None:
                                    # TODO: Is this guaranteed impossible?
                                    assert False
                                case ReferenceType(type=argType) | argType:
                                    location = spec.decl.name.location
                                    semNamespace.addArgument(name, argType, location)

                        if len(semLoc) == 0:
                            # Parse mnemonic field as semantics.
                            semLoc = mnemLoc

                        semRef = parseSem(reader, semLoc, semNamespace, modeType)
                    except BadInput as ex:
                        reader.error(
                            "error in semantics: %s", ex, location=ex.locations
                        )
                        # This is the last field.
                        continue
                    try:
                        semantics: CodeBlock | None
                        semantics = semNamespace.createCodeBlock(
                            retRef=semRef, log=reader
                        )
                    except ValueError:
                        # Error was already logged inside createCodeBlock().
                        pass
                else:
                    semantics = None
        except DelayedError:
            pass
        else:
            # TODO: An older version of this code created the ModeEntry
            #       with a None encoding or decoding; was that better or
            #       incorrect?
            if encoding is not None and decoding is not None:
                if semantics is None:
                    template = None
                else:
                    template = CodeTemplate(semantics, placeholders)
                entry = ModeEntry(
                    encoding, mnemonic, template, placeholders, frozenset(flagsRequired)
                )
                yield ParsedModeEntry(entry, *decoding)


def _formatEncodingWidth(width: Width | None) -> str:
    return "empty" if width is None else f"{width} bits wide"


def _determineEncodingWidth(
    entries: list[ParsedModeEntry],
    aux: bool,
    modeName: str | None,
    logger: DefLineReader,
) -> int | None:
    """
    Returns the common encoding width for the given list of mode entries.
    Entries with a deviating encoding width will be logged as errors on the
    given logger and removed from the entries list.
    If the 'aux' argument is False, the first matched unit width of each entry
    is checked, otherwise the width of auxiliary encoding units is checked.
    If the entries represent instructions, pass None for the mode name.
    """

    widthAttr = "auxEncodingWidth" if aux else "encodingWidth"

    widthFreqs: DefaultDict[int | None, int] = defaultdict(int)
    for entry in entries:
        widthFreqs[getattr(entry.entry.encoding, widthAttr)] += 1
    if aux:
        widthFreqs.pop(None, None)

    if len(widthFreqs) == 0:
        # Empty mode, only errors or aux check with no aux items.
        encWidth = None
    elif len(widthFreqs) == 1:
        # Single type.
        (encWidth,) = widthFreqs.keys()
    else:
        # Multiple widths; use one with the maximum frequency.
        encWidth, _ = max(widthFreqs.items(), key=lambda item: item[1])
        validWidths: Iterable[Width | None]
        if aux:
            validWidths = (encWidth, None)
        else:
            validWidths = (encWidth,)
        badEntryIndices = []
        for idx, entry in enumerate(entries):
            encDef = entry.entry.encoding
            if cast(Width, getattr(encDef, widthAttr)) not in validWidths:
                logger.error(
                    "%sencoding match is %s, while %s is dominant %s",
                    ("auxiliary " if aux else ""),
                    _formatEncodingWidth(getattr(encDef, widthAttr)),
                    _formatEncodingWidth(encWidth),
                    (
                        "for instructions"
                        if modeName is None
                        else f'in mode "{modeName}"'
                    ),
                    location=(
                        encDef.auxEncodingLocation if aux else encDef.encodingLocation
                    ),
                )
                badEntryIndices.append(idx)
        for idx in reversed(badEntryIndices):
            del entries[idx]

    return encWidth


_reModeArgs = re.compile(_typeTok + r"\s" + _nameTok + r"$")


def _parseMode(
    reader: DefLineReader,
    args: InputLocation,
    globalNamespace: GlobalNamespace,
    prefixes: PrefixMappingFactory,
    modes: dict[str, Mode],
    modeEntries: dict[str | None, list[ParsedModeEntry]],
    wantSemantics: bool,
) -> None:
    # Parse header line.
    match = args.match(_reModeArgs)
    if match is None:
        reader.error(
            'invalid mode arguments, expected "mode <type> <name>"', location=args
        )
        reader.skip_block()
        return
    modeTypeLoc, modeNameLoc = match.groups
    semType: None | IntType | ReferenceType
    try:
        semType = parse_type_decl(modeTypeLoc.text)
    except ValueError as ex:
        reader.error("bad mode type: %s", ex, location=modeTypeLoc)
        semType = None

    # Check whether it's safe to add mode to namespace.
    modeName = modeNameLoc.text
    addMode = False
    if modeName in modes:
        reader.error(
            'mode "%s" redefined; first definition was on line %d',
            modeName,
            modes[modeName].location.lineno,
            location=modeNameLoc,
        )
    else:
        try:
            parse_type(modeName)
        except ValueError:
            addMode = True
        else:
            reader.error(
                'mode name "%s" conflicts with type', modeName, location=modeNameLoc
            )

    # Parse entries.
    parsedEntries = list(
        _parseModeEntries(
            reader,
            globalNamespace,
            prefixes,
            modes,
            semType,
            (),
            _parseModeSemantics,
            wantSemantics,
        )
    )

    # Create and remember mode object.
    encWidth = _determineEncodingWidth(parsedEntries, False, modeName, reader)
    auxEncWidth = _determineEncodingWidth(parsedEntries, True, modeName, reader)
    entries = tuple(parsedEntry.entry for parsedEntry in parsedEntries)
    mode = Mode(modeName, encWidth, auxEncWidth, semType, modeNameLoc, entries)
    if addMode:
        modes[modeName] = mode
        modeEntries[modeName] = parsedEntries


def _parseInstr(
    reader: DefLineReader,
    args: InputLocation,
    globalNamespace: GlobalNamespace,
    prefixes: PrefixMappingFactory,
    modes: Mapping[str, Mode],
    wantSemantics: bool,
) -> Iterator[ParsedModeEntry]:
    mnemBase = []
    for mnemItem in _parseMnemonic(args, {}, reader):
        if isinstance(mnemItem, str):
            mnemBase.append(mnemItem)
        else:
            # TODO: The main reason to disallow this is that it would complicate
            #       the code to support it. Is that a good enough reason?
            reader.error("the mnemonic base cannot contain numbers", location=args)

    for instr in _parseModeEntries(
        reader,
        globalNamespace,
        prefixes,
        modes,
        None,
        tuple(mnemBase),
        _parseInstrSemantics,
        wantSemantics,
    ):
        encDef = instr.entry.encoding
        encWidth = encDef.encodingWidth
        if encWidth is None:
            reader.error(
                "instruction encoding must not be empty",
                location=encDef.encodingLocation,
            )
            # Do not yield the instruction, to avoid this problem from being
            # reporting again as a width inconsistency.
            continue
        auxEncodingWidth = encDef.auxEncodingWidth
        if auxEncodingWidth not in (encWidth, None):
            reader.error(
                "auxiliary instruction encoding units are %s bits wide, "
                "while first unit is %s bits wide",
                auxEncodingWidth,
                encWidth,
                location=encDef.auxEncodingLocation,
            )
        yield instr


_reHeader = re.compile(_nameTok + r"(?:\s+(.*\S)\s*)?$")


def parseInstrSet(
    path: Traversable, logger: Logger | None = None, wantSemantics: bool = True
) -> InstructionSet | None:
    if logger is None:
        logger = getLogger("parse-instr")
        logger.setLevel(WARNING)

    globalBuilder = StatelessCodeBlockBuilder()
    globalNamespace = GlobalNamespace(globalBuilder)
    prefixes = PrefixMappingFactory(globalNamespace)
    modes: dict[str, Mode] = {}
    modeEntries: dict[str | None, list[ParsedModeEntry]] = {}
    instructions = modeEntries.setdefault(None, [])

    with DefLineReader.open(path, logger) as reader:
        for header in reader:
            if not header:
                continue
            match = header.match(_reHeader)
            if match is None:
                reader.error("malformed line outside block")
                continue
            keyword = match.group(1)
            args = match.group(2) if match.has_group(2) else header.end_location
            defType = keyword.text
            if defType == "reg":
                _parseRegs(reader, args, globalNamespace)
            elif defType == "io":
                _parseIO(reader, args, globalNamespace)
            elif defType == "prefix":
                _parsePrefix(reader, args, globalNamespace, prefixes)
            elif defType == "func":
                _parseFunc(reader, args, globalNamespace, wantSemantics)
            elif defType == "mode":
                _parseMode(
                    reader,
                    args,
                    globalNamespace,
                    prefixes,
                    modes,
                    modeEntries,
                    wantSemantics,
                )
            elif defType == "instr":
                instructions += _parseInstr(
                    reader, args, globalNamespace, prefixes, modes, wantSemantics
                )
            else:
                reader.error('unknown definition type "%s"', defType, location=keyword)
                reader.skip_block()

        # Check that the program counter was defined.
        try:
            pc = globalNamespace["pc"]
        except KeyError:
            reader.error(
                "no program counter defined: "
                'a register or alias named "pc" is required'
            )
        else:
            assert isinstance(pc, Reference), pc

        encWidth = _determineEncodingWidth(instructions, False, None, reader)
        anyAux = any(len(instr.entry.encoding) >= 2 for instr in instructions)
        auxEncWidth = encWidth if anyAux else None

        prefixMapping = prefixes.createMapping()

        instrSet = None
        if reader.problem_counter.num_errors == 0:
            try:
                if encWidth is None:
                    # Since the last instruction with an identical encoding overrides
                    # earlier ones, only degenerate instruction sets can have an empty
                    # encoding: either the instruction set is empty or it has a single
                    # instruction with no encoding.
                    raise ValueError("no encodings")
                instrSet = InstructionSet(
                    encWidth, auxEncWidth, globalNamespace, prefixMapping, modeEntries
                )
            except ValueError as ex:
                reader.error("final validation of instruction set failed: %s", ex)

        reader.summarize()

    logger.debug(
        "regs: %s",
        ", ".join(
            f"{name} = {value!r}" for name, value in sorted(globalNamespace.items())
        ),
    )

    return instrSet
