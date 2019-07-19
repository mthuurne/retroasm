from collections import OrderedDict, defaultdict
from logging import WARNING, getLogger
import re

from .analysis import CodeTemplate
from .codeblock_builder import (
    SemanticsCodeBlockBuilder, StatelessCodeBlockBuilder
)
from .context_parser import MatchPlaceholderSpec, ValuePlaceholderSpec
from .decode import ParsedModeEntry, decomposeEncoding
from .expression_builder import (
    BadExpression, UnknownNameError, buildExpression, buildReference,
    buildStatementEval, convertDefinition
)
from .expression_parser import (
    DeclarationNode, DefinitionNode, FlagTestNode, IdentifierNode,
    MultiMatchNode, parseContext, parseExpr, parseExprList, parseInt,
    parseRegs, parseStatement
)
from .function_builder import createFunc
from .instrset import InstructionSet, Prefix, PrefixMappingFactory
from .linereader import BadInput, DefLineReader, DelayedError, mergeSpan
from .mode import (
    Encoding, EncodingExpr, EncodingMultiMatch, MatchPlaceholder, Mnemonic,
    Mode, ModeEntry, ValuePlaceholder
)
from .namespace import (
    ContextNamespace, GlobalNamespace, LocalNamespace, NameExistsError
)
from .reference import badReference
from .storage import IOChannel, IOStorage, ValArgStorage, Variable
from .types import IntType, ReferenceType, parseType, parseTypeDecl

_namePat = r"[A-Za-z_][A-Za-z0-9_]*'?"
_nameTok = r'\s*(' + _namePat + r')\s*'
_typeTok = r'\s*(' + _namePat + r'&?)\s*'

_reDotSep = re.compile(r'\s*\.\s*')

def _parseRegs(reader, args, globalNamespace):
    headerLocation = reader.location
    if args:
        reader.error(
            'register definition must have no arguments', location=args
            )

    for line in reader.iterBlock():
        try:
            nodes = parseRegs(line)
        except BadInput as ex:
            reader.error(
                'bad register definition line: %s', ex, location=ex.location
                )
            continue

        lastTypeLocation = None
        for node in nodes:
            # Parse type declaration.
            decl = node if isinstance(node, DeclarationNode) else node.decl
            typeLocation = decl.type.location
            try:
                regType = parseTypeDecl(decl.type.name)
            except ValueError as ex:
                # Avoid reporting the same error twice.
                if typeLocation != lastTypeLocation:
                    reader.error('%s', ex, location=typeLocation)
                continue
            lastTypeLocation = typeLocation

            if isinstance(node, DeclarationNode):
                # Define base register.
                if isinstance(regType, ReferenceType):
                    reader.error(
                        'base register cannot have a reference type',
                        location=(decl.name.location, typeLocation)
                        )
                else:
                    try:
                        globalNamespace.addVariable(
                            decl.name.name, regType, decl.name.location
                            )
                    except NameExistsError as ex:
                        reader.error(
                            'bad base register definition: %s', ex,
                            location=ex.location
                            )
            else:
                # Define register alias.
                name = decl.name.name
                try:
                    ref = convertDefinition(
                        decl.kind, name, regType, node.value, globalNamespace
                        )
                except BadExpression as ex:
                    reader.error(
                        'bad register alias: %s', ex,
                        location=ex.location
                        )
                    ref = badReference(regType)
                try:
                    globalNamespace.define(name, ref, decl.name.location)
                except NameExistsError as ex:
                    reader.error(
                        'failed to define register alias: %s', ex,
                        location=ex.location
                        )

    # Check the program counter.
    try:
        return globalNamespace['pc'].bits
    except KeyError:
        reader.error(
            'no program counter defined: '
            'a register or alias named "pc" is required',
            location=headerLocation
            )
        return None

_reCommaSep = re.compile(r'\s*,\s*')
_reArgDecl = re.compile(_typeTok + r'\s' + _nameTok + r'$')

def _parseTypedArgs(reader, args, description):
    '''Parses a typed arguments list, yielding a triple for each argument,
    containing the argument type and InputLocations for the type and name.
    Errors are logged on the given reader as they are discovered.
    '''
    argLocs = tuple(args.split(_reCommaSep))
    if len(argLocs) == 1:
        # Arg list contains no separators; do we have 0 or 1 argument(s)?
        if len(args) == 0 or args.text.isspace():
            return

    for i, argLoc in enumerate(argLocs, 1):
        argMatch = argLoc.match(_reArgDecl)
        if argMatch is None:
            reader.error(
                '%s %d not of the form "<type> <name>"', description, i,
                location=argLoc
                )
            continue

        typeLoc, nameLoc = argMatch.groups
        try:
            argType = parseTypeDecl(typeLoc.text)
        except ValueError as ex:
            reader.error(
                'bad %s %d ("%s"): %s', description, i, nameLoc.text, ex,
                location=typeLoc
                )
        else:
            yield argType, typeLoc, nameLoc

def _parsePrefix(reader, args, namespace, factory):
    headerLocation = reader.location

    # Parse header line.
    decodeFlags = []
    try:
        with reader.checkErrors():
            flagType = IntType.u(1)
            for argType, argTypeLoc, argNameLoc in _parseTypedArgs(
                    reader, args, 'decode flag'
                    ):
                if isinstance(argType, ReferenceType):
                    reader.error(
                        'decode flag cannot be declared as a reference type',
                        location=argTypeLoc
                        )
                    continue
                if argType is not flagType:
                    # Maybe in the future we'll support other types.
                    reader.error(
                        'decode flag of type "%s", expected "u1"', argType,
                        location=argTypeLoc
                        )
                argName = argNameLoc.text
                try:
                    namespace.addVariable(argName, argType, argNameLoc)
                except ValueError as ex:
                    reader.error(str(ex))
                except NameExistsError as ex:
                    reader.error(
                        'error defining decode flag: %s', ex,
                        location=ex.location
                        )
                else:
                    decodeFlags.append(argName)
    except DelayedError:
        reader.skipBlock()
        return

    # Parse body.
    prefixes = []
    for line in reader.iterBlock():
        # Split line into 3 fields.
        fields = tuple(line.split(_reDotSep))
        try:
            encLoc, mnemLoc, semLoc = fields
        except ValueError:
            reader.error(
                'wrong number of dot-separated fields in prefix line: '
                'expected 3, got %d', len(fields)
                )
            continue

        # Parse encoding.
        try:
            with reader.checkErrors():
                if len(encLoc) == 0:
                    reader.error(
                        'prefix encoding cannot be empty',
                        location=encLoc
                        )
                else:
                    try:
                        encNodes = parseExprList(encLoc)
                    except BadInput as ex:
                        reader.error(
                            'bad prefix encoding: %s', ex,
                            location=ex.location
                            )
                    else:
                        encItems = []
                        for encNode in encNodes:
                            try:
                                encItems.append(_parseEncodingExpr(
                                    encNode, namespace, {}
                                    ))
                            except BadInput as ex:
                                reader.error(
                                    'bad prefix encoding: %s', ex,
                                    location=ex.location
                                    )
        except DelayedError:
            encoding = None
        else:
            encoding = Encoding(encItems, encLoc)

        # Parse mnemonic.
        if len(mnemLoc) != 0:
            reader.warning(
                'prefix mnemonics are not supported yet',
                location=mnemLoc
                )

        # Parse semantics.
        try:
            with reader.checkErrors():
                if len(semLoc) == 0:
                    reader.error(
                        'prefix semantics cannot be empty; use "nop" instead',
                        location=semLoc
                        )
                else:
                    semBuilder = SemanticsCodeBlockBuilder()
                    semNamespace = LocalNamespace(namespace, semBuilder)
                    try:
                        _parseInstrSemantics(reader, semLoc, semNamespace)
                    except BadInput as ex:
                        reader.error(
                            'bad prefix semantics: %s', ex,
                            location=ex.location
                            )
                    else:
                        try:
                            semantics = semNamespace.createCodeBlock(log=reader)
                        except ValueError:
                            # Error was already logged inside createCodeBlock().
                            pass
        except DelayedError:
            semantics = None

        if encoding is not None and semantics is not None:
            prefixes.append(Prefix(encoding, semantics))

    try:
        factory.addPrefixes(decodeFlags, prefixes)
    except ValueError as ex:
        reader.error(
            'validation of prefix block failed: %s', ex,
            location=headerLocation
            )
    except BadInput as ex:
        reader.error(
            'validation of prefix block failed: %s', ex,
            location=ex.location
            )

_reIOLine = re.compile(_nameTok + r'\s' + _nameTok + r'\[' + _nameTok + r'\]$')

def _parseIO(reader, args, namespace):
    if args:
        reader.error('I/O definition must have no arguments', location=args)

    for line in reader.iterBlock():
        match = line.match(_reIOLine)
        if match is None:
            reader.error('invalid I/O definition line')
        else:
            elemTypeLoc, nameLoc, addrTypeLoc = match.groups

            try:
                elemType = parseType(elemTypeLoc.text)
            except ValueError as ex:
                reader.error(
                    'bad I/O element type: %s', ex, location=elemTypeLoc
                    )
                elemType = None

            try:
                addrType = parseType(addrTypeLoc.text)
            except ValueError as ex:
                reader.error(
                    'bad I/O address type: %s', ex, location=addrTypeLoc
                    )
                addrType = None

            if elemType is None or addrType is None:
                continue

            name = nameLoc.text
            channel = IOChannel(name, elemType, addrType)
            try:
                namespace.define(name, channel, nameLoc)
            except NameExistsError as ex:
                reader.error(
                    'error defining I/O channel: %s', ex, location=ex.location
                    )

_reFuncHeader = re.compile(
    r'(?:' + _typeTok + r'\s)?' + _nameTok + r'\((.*)\)$'
    )

def _parseFunc(reader, headerArgs, namespace, wantSemantics):
    # Parse header line.
    match = headerArgs.match(_reFuncHeader)
    if match is None:
        reader.error('invalid function header line', location=headerArgs)
        reader.skipBlock()
        return

    # Parse return type.
    if match.hasGroup(1):
        retTypeLoc = match.group(1)
        try:
            retType = parseTypeDecl(retTypeLoc.text)
        except ValueError as ex:
            reader.error('bad return type: %s', ex, location=retTypeLoc)
            reader.skipBlock()
            return
    else:
        retType = None

    # Parse arguments.
    args = OrderedDict()
    try:
        with reader.checkErrors():
            nameLocations = {}
            for i, (argType, argTypeLoc_, argNameLoc) in enumerate(
                    _parseTypedArgs(reader, match.group(3),
                                    'function argument'),
                    1):
                argName = argNameLoc.text
                if argName == 'ret':
                    reader.error(
                        '"ret" is reserved for the return value; '
                        'it cannot be used as an argument name',
                        location=argNameLoc
                        )
                elif argName in nameLocations:
                    reader.error(
                        'function argument %d has the same name as '
                        'an earlier argument: %s', i, argName,
                        location=(argNameLoc, nameLocations[argName])
                        )
                else:
                    args[argName] = argType
                    nameLocations[argName] = argNameLoc
    except DelayedError:
        reader.skipBlock()
        return

    if wantSemantics:
        funcNameLoc = match.group(2)
        funcName = funcNameLoc.text

        # Parse body lines.
        func = createFunc(reader, funcName, retType, args, namespace)

        # Store function in namespace.
        try:
            namespace.define(funcName, func, funcNameLoc)
        except NameExistsError as ex:
            reader.error(
                'error declaring function: %s', ex, location=ex.location
                )
    else:
        reader.skipBlock()

def _parseModeContext(ctxLoc, prefixes, modes, reader):
    placeholderSpecs = OrderedDict()
    flagsRequired = set()
    for node in parseContext(ctxLoc):
        if isinstance(node, (DeclarationNode, DefinitionNode)):
            decl = node if isinstance(node, DeclarationNode) else node.decl
            name = decl.name.name
            typeName = decl.type.name

            # Figure out whether the name is a mode or type.
            mode = modes.get(typeName)
            if mode is not None:
                placeholder = MatchPlaceholderSpec(decl, mode)
                if isinstance(node, DefinitionNode):
                    reader.error(
                        'filter values for mode placeholders are '
                        'not supported yet',
                        location=mergeSpan(
                            node.location, node.value.treeLocation
                            )
                        )
            else:
                try:
                    # TODO: While the documentation says we do support defining
                    #       references in the context, parseType() rejects
                    #       "<type>&"; we'd have to use parseTypeDecl() instead.
                    typ = parseType(typeName)
                except ValueError:
                    reader.error(
                        'there is no type or mode named "%s"', typeName,
                        location=decl.type.location
                        )
                    continue
                value = node.value if isinstance(node, DefinitionNode) else None
                placeholder = ValuePlaceholderSpec(decl, typ, value)
            if name in placeholderSpecs:
                reader.error(
                    'multiple placeholders named "%s"', name,
                    location=decl.name.location
                    )
            else:
                placeholderSpecs[name] = placeholder
        elif isinstance(node, FlagTestNode):
            name = node.name
            if prefixes.hasFlag(name):
                flagsRequired.add(name)
            else:
                reader.error(
                    'there is no decode flag named "%s"', name,
                    location=node.location
                    )
        else:
            assert False, node

    return placeholderSpecs, flagsRequired

def _buildPlaceholders(placeholderSpecs, globalNamespace, reader):
    '''Yields pairs of name and Placeholder object.
    '''
    semNamespace = ContextNamespace(globalNamespace)

    for name, spec in placeholderSpecs.items():
        decl = spec.decl
        semType = spec.semanticsType
        value = spec.value

        code = None
        if value is not None:
            placeholderNamespace = LocalNamespace(
                semNamespace, SemanticsCodeBlockBuilder()
                )
            try:
                convertDefinition(
                    decl.kind, decl.name, semType, value, placeholderNamespace
                    )
            except BadExpression as ex:
                reader.error('%s', ex, location=ex.location)
            else:
                code = placeholderNamespace.createCodeBlock(name)

        location = decl.name.location
        try:
            if isinstance(semType, ReferenceType):
                semNamespace.addReferenceArgument(name, semType.type, location)
            else:
                semNamespace.addValueArgument(name, semType, location)
        except NameExistsError as ex:
            reader.error('%s', ex, location=ex.location)
            continue

        if isinstance(spec, ValuePlaceholderSpec):
            yield name, ValuePlaceholder(name, semType, code)
        elif isinstance(spec, MatchPlaceholderSpec):
            yield name, MatchPlaceholder(name, spec.mode)
        else:
            assert False, spec

def _parseEncodingExpr(encNode, encNamespace, placeholderSpecs):
    '''Parse encoding node that is not a MultiMatchNode.
    Returns the parse result as an EncodingExpr.
    Raises BadInput if the node is invalid.
    '''
    namespace = LocalNamespace(encNamespace, SemanticsCodeBlockBuilder())
    try:
        encRef = buildReference(encNode, namespace)
    except BadInput as ex:
        if isinstance(ex, UnknownNameError):
            spec = placeholderSpecs.get(ex.name)
            if spec is not None:
                if spec.encodingWidth is None:
                    raise BadInput(
                        'cannot use placeholder "%s" in encoding field, '
                        'since mode "%s" has an empty encoding sequence'
                        % (ex.name, spec.mode.name),
                        location=(ex.location, spec.decl.treeLocation)
                        )
                if spec.value is not None:
                    raise BadInput(
                        'cannot use placeholder "%s" in encoding field, '
                        'since its value is computed in the context'
                        % ex.name,
                        location=(ex.location, spec.value.treeLocation)
                        )
        raise BadInput(
            'error in encoding: %s' % ex,
            location=(encNode.treeLocation, ex.location)
            )

    encLoc = encNode.treeLocation
    code = namespace.builder.createCodeBlock((encRef.bits,))
    if len(code.nodes) != 0:
        raise BadInput(
            'encoding expression accesses state or performs I/O',
            location=encLoc
            )
    encBits, = code.returned
    for storage in encBits.iterStorages():
        if isinstance(storage, Variable):
            raise BadInput(
                'encoding expression references register',
                location=encLoc
                )
        if isinstance(storage, IOStorage):
            raise BadInput(
                'encoding expression references storage location '
                'on I/O channel "%s"' % storage.channel.name,
                location=encLoc
                )
    return EncodingExpr(encBits, encLoc)

def _parseMultiMatch(encNode, identifiers, placeholderSpecs):
    '''Parse an encoding node of type MultiMatchNode.
    Returns the parse result as an EncodingMultiMatch.
    Raises BadInput if the node is invalid.
    '''
    name = encNode.name
    try:
        placeholder = placeholderSpecs[name]
    except KeyError:
        raise BadInput(
            'placeholder "%s" does not exist in context' % name,
            location=encNode.treeLocation
            )
    if not isinstance(placeholder, MatchPlaceholderSpec):
        raise BadInput(
            'placeholder "%s" does not represent a mode match' % name,
            location=(encNode.treeLocation, placeholder.decl.treeLocation)
            )

    mode = placeholder.mode
    start = 1 if name in identifiers else 0
    return EncodingMultiMatch(name, mode, start, encNode.treeLocation)

def _parseModeEncoding(encNodes, placeholderSpecs, globalNamespace, logger):
    # Define placeholders in encoding namespace.
    encNamespace = ContextNamespace(globalNamespace)
    for name, spec in placeholderSpecs.items():
        if spec.value is None:
            encWidth = spec.encodingWidth
            if encWidth is not None:
                encType = IntType.u(encWidth)
                location = spec.decl.name.location
                try:
                    encNamespace.addValueArgument(name, encType, location)
                except NameExistsError as ex:
                    logger.error(
                        'bad placeholder: %s', ex, location=ex.location
                        )

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
                yield _parseMultiMatch(
                    encNode, identifiers, placeholderSpecs
                    )
            else:
                # Expression possibly containing single encoding field matches.
                yield _parseEncodingExpr(
                    encNode, encNamespace, placeholderSpecs
                    )
        except BadInput as ex:
            logger.error('%s', ex, location=ex.location)

def _checkEmptyMultiMatches(encItems, placeholderSpecs, logger):
    '''Warn about multi-matches that always match zero elements.
    Technically there is nothing wrong with those, but it is probably not what
    the user intended.
    '''
    for encItem in encItems:
        if isinstance(encItem, EncodingMultiMatch):
            mode = encItem.mode
            if mode.encodingWidth is None:
                logger.warning(
                    'mode "%s" does not contain encoding elements',
                    mode.name, location=(
                        encItem.location,
                        placeholderSpecs[encItem.name].decl.treeLocation
                        )
                    )
            elif encItem.start >= 1 and mode.auxEncodingWidth is None:
                logger.warning(
                    'mode "%s" does not match auxiliary encoding units',
                    mode.name, location=(
                        encItem.location,
                        placeholderSpecs[encItem.name].decl.treeLocation
                        )
                    )

def _checkMissingPlaceholders(encItems, placeholderSpecs, location, logger):
    '''Check that our encoding field contains sufficient placeholders to be able
    to make matches in all included mode tables.
    '''
    # Take inventory of placeholders in the encoding.
    identifiers = set()
    multiMatches = set()
    for encItem in encItems:
        if isinstance(encItem, EncodingExpr):
            for storage in encItem.bits.iterStorages():
                if isinstance(storage, ValArgStorage):
                    identifiers.add(storage.name)
        elif isinstance(encItem, EncodingMultiMatch):
            multiMatches.add(encItem.name)
        else:
            assert False, encItem

    # Check whether all placeholders from the context occur in the encoding.
    for name, spec in placeholderSpecs.items():
        if isinstance(spec, ValuePlaceholderSpec):
            if spec.value is not None:
                # The value is computed, so we don't need to encode it.
                continue
            if name not in identifiers:
                logger.error(
                    'value placeholder "%s" does not occur in encoding', name,
                    location=(location, spec.decl.treeLocation)
                    )
        elif isinstance(spec, MatchPlaceholderSpec):
            mode = spec.mode
            if mode.encodingWidth is None:
                # Mode has empty encoding, no match needed.
                continue
            if name in multiMatches:
                # Mode is matched using "X@" syntax.
                continue
            if name not in identifiers:
                logger.error(
                    'no placeholder "%s" for mode "%s" in encoding',
                    name, mode.name,
                    location=(location, spec.decl.treeLocation)
                    )
            if mode.auxEncodingWidth is not None:
                logger.error(
                    'mode "%s" matches auxiliary encoding units, but there '
                    'is no "%s@" placeholder for them',
                    mode.name, name,
                    location=(location, spec.decl.treeLocation)
                    )
        else:
            assert False, spec

def _checkAuxEncodingWidth(encItems, logger):
    '''Check whether the encoding widths in the given encoding are the same
    for all auxiliary encoding items.
    Violations are logged as errors on the given logger.
    '''
    firstAux = [None, None]
    def checkAux(width, location):
        auxWidth, auxLoc = firstAux
        if auxWidth is None:
            firstAux[0] = width
            firstAux[1] = location
        elif width != auxWidth:
            logger.error(
                'encoding item matches width %s, while first auxiliary '
                'encoding match has width %s', width, auxWidth,
                location=(location, auxLoc)
                )
    firstUnitMatched = False
    for encItem in encItems:
        encLoc = encItem.location
        if isinstance(encItem, EncodingExpr):
            if firstUnitMatched:
                checkAux(encItem.encodingWidth, encLoc)
            else:
                firstUnitMatched = True
        elif isinstance(encItem, EncodingMultiMatch):
            mode = encItem.mode
            modeAuxWidth = mode.auxEncodingWidth
            if firstUnitMatched:
                if encItem.start == 0:
                    checkAux(mode.encodingWidth, encLoc)
                if modeAuxWidth is not None:
                    checkAux(modeAuxWidth, encLoc)
            else:
                if modeAuxWidth is not None:
                    if encItem.encodedLength != 1:
                        checkAux(modeAuxWidth, encLoc)
                firstUnitMatched = True
        else:
            assert False, encItem

def _checkDuplicateMultiMatches(encItems, logger):
    '''Checks whether more than one multi-matcher exists for the same
    placeholder. If they exist, they are reported as errors on the given logger.
    '''
    claimedMultiMatches = {}
    for encItem in encItems:
        if isinstance(encItem, EncodingMultiMatch):
            name = encItem.name
            if name in claimedMultiMatches:
                logger.error(
                    'duplicate multi-match placeholder "%s@"', name,
                    location=(encItem.location, claimedMultiMatches[name])
                    )
            else:
                claimedMultiMatches[name] = encItem.location

def _combinePlaceholderEncodings(decodeMap, placeholderSpecs, reader):
    '''Yield pairs of placeholder name and the locations where the placeholder
    resides in the encoded items.
    Each such location is a triple of index in the encoded items, bit offset
    within that item and width in bits.
    '''
    for name, slices in decodeMap.items():
        placeholderSpec = placeholderSpecs[name]
        immWidth = placeholderSpec.encodingWidth
        decoding = []
        problems = []
        prev = 0
        for immIdx, encIdx, refIdx, width in sorted(slices):
            if prev < immIdx:
                problems.append('gap at [%d:%d]' % (prev, immIdx))
            elif prev > immIdx:
                problems.append(
                    'overlap at [%d:%d]' % (immIdx, min(immIdx + width, prev))
                    )
            prev = max(immIdx + width, prev)
            decoding.append((encIdx, refIdx, width))
        if prev < immWidth:
            problems.append('gap at [%d:%d]' % (prev, immWidth))
        elif prev > immWidth:
            assert False, (name, slices)
        if problems:
            reader.error(
                'cannot decode value for "%s": %s', name, ', '.join(problems),
                location=placeholderSpec.decl.treeLocation
                )
        else:
            yield name, tuple(decoding)

def _checkDecodingOrder(encoding, sequentialMap, placeholderSpecs, reader):
    '''Verifies that there is an order in which placeholders can be decoded.
    Such an order might not exist because of circular dependencies.
    '''
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
            encIdx
            for encIdx, refIdx, width in decoding
            if encIdx > multiIdx
            ]
        if badIdx:
            mode = placeholderSpec.mode
            reader.error(
                'cannot match "%s": mode "%s" has a variable encoding length '
                'and (parts of) the placeholder "%s" are placed after the '
                'multi-match placeholder "%s@"',
                name, mode.name, name, name,
                location=[placeholderSpec.decl.treeLocation, matcher.location]
                    + [encoding[idx].location for idx in badIdx]
                )

def _parseModeDecoding(encoding, placeholderSpecs, reader):
    '''Construct a mapping that, given an encoded instruction, produces the
    values for context placeholders.
    '''
    try:
        # Decompose the encoding expressions.
        fixedMatcher, decodeMap = decomposeEncoding(encoding)
    except BadInput as ex:
        reader.error('%s', ex, location=ex.location)
        return None
    try:
        with reader.checkErrors():
            # Create a mapping to extract immediate values from encoded items.
            sequentialMap = dict(_combinePlaceholderEncodings(
                decodeMap, placeholderSpecs, reader
                ))
        with reader.checkErrors():
            # Check whether unknown-length multi-matches are blocking decoding.
            _checkDecodingOrder(
                encoding, sequentialMap, placeholderSpecs, reader
                )
    except DelayedError:
        return None
    else:
        sequentialMap[None] = fixedMatcher
        return sequentialMap

def _parseModeSemantics(reader, semLoc, semNamespace, modeType):
    semantics = parseExpr(semLoc)
    if isinstance(modeType, ReferenceType):
        ref = buildReference(semantics, semNamespace)
        if modeType is not None:
            if ref.type != modeType.type:
                raise BadInput(
                    'semantics type %s does not match mode type %s'
                    % (ref.type, modeType.type),
                    location=semLoc
                    )
        semNamespace.define('ret', ref, semLoc)
    else:
        expr = buildExpression(semantics, semNamespace)
        # Note that modeType can be None because of earlier errors.
        if modeType is not None:
            ref = semNamespace.addVariable('ret', modeType, semLoc)
            ref.emitStore(semNamespace.builder, expr, semLoc)

def _parseInstrSemantics(reader, semLoc, namespace, modeType=None):
    assert modeType is None, modeType
    node = parseStatement(semLoc)
    buildStatementEval(reader, 'semantics field', namespace, node)

_reMnemonic = re.compile(r"\w+'?|[$%]\w+|[^\w\s]")

def _parseMnemonic(mnemLoc, placeholders, reader):
    seenPlaceholders = {}
    for mnemElem in mnemLoc.findLocations(_reMnemonic):
        text = mnemElem.text
        placeholder = placeholders.get(text)
        if placeholder is None:
            if '0' <= text[0] <= '9' or text[0] in '$%':
                try:
                    value, width = parseInt(text)
                except ValueError as ex:
                    reader.error('%s', ex, location=mnemElem)
                else:
                    yield value
            else:
                yield text
        elif text in seenPlaceholders:
            # In theory we could support repeated placeholders, but the only
            # meaning that would make sense is that they would all match the
            # same mode entry or expression and I don't know of any situation
            # in which that would be a useful feature.
            reader.error(
                'placeholder "%s" occurs multiple times in mnemonic', text,
                location=(mnemElem, seenPlaceholders[text])
                )
        else:
            yield placeholder
            seenPlaceholders[text] = mnemElem

def _parseModeEntries(
        reader, globalNamespace, pc, prefixes, modes, modeType, mnemBase,
        parseSem, wantSemantics
        ):
    for line in reader.iterBlock():
        # Split mode line into 4 fields.
        fields = list(line.split(_reDotSep))
        if len(fields) < 2:
            reader.error('field separator "." missing in mode line')
            continue
        if len(fields) > 4:
            reader.error('too many fields (%d) in mode line', len(fields))
            continue
        fields += [line.endLocation] * (4 - len(fields))
        encLoc, mnemLoc, semLoc, ctxLoc = fields

        try:
            with reader.checkErrors():
                # Parse context.
                if len(ctxLoc) != 0:
                    try:
                        with reader.checkErrors():
                            placeholderSpecs, flagsRequired = _parseModeContext(
                                ctxLoc, prefixes, modes, reader
                                )
                            placeholders = OrderedDict(_buildPlaceholders(
                                placeholderSpecs, globalNamespace, reader
                                ))
                    except DelayedError:
                        # To avoid error spam, skip this line.
                        continue
                else:
                    placeholderSpecs, flagsRequired = {}, set()
                    placeholders = OrderedDict()

                # Parse encoding.
                if len(encLoc) != 0:
                    try:
                        # Parse encoding field.
                        encNodes = parseExprList(encLoc)
                    except BadInput as ex:
                        reader.error(
                            'error in encoding: %s', ex, location=ex.location
                            )
                        encNodes = None
                else:
                    encNodes = ()
                if encNodes is None:
                    encoding = None
                else:
                    try:
                        with reader.checkErrors():
                            encItems = tuple(_parseModeEncoding(
                                encNodes, placeholderSpecs, globalNamespace,
                                reader
                                ))
                        with reader.checkErrors():
                            _checkAuxEncodingWidth(encItems, reader)
                            _checkEmptyMultiMatches(
                                encItems, placeholderSpecs, reader
                                )
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
                    decoding = _parseModeDecoding(
                        encoding, placeholderSpecs, reader
                        )

                # Parse mnemonic.
                mnemItems = mnemBase + tuple(_parseMnemonic(
                    mnemLoc, placeholders, reader
                    ))
                if len(mnemItems) == 0:
                    reader.error('missing mnemonic', location=mnemLoc)
                else:
                    mnemonic = Mnemonic(mnemItems)

                # Parse semantics.
                if wantSemantics:
                    semBuilder = SemanticsCodeBlockBuilder()
                    semNamespace = LocalNamespace(globalNamespace, semBuilder)
                    try:
                        # Define placeholders in semantics builder.
                        for name, spec in placeholderSpecs.items():
                            location = spec.decl.name.location
                            semType = spec.semanticsType
                            if isinstance(semType, ReferenceType):
                                semNamespace.addReferenceArgument(
                                    name, semType.type, location
                                    )
                            else:
                                semNamespace.addValueArgument(
                                    name, semType, location
                                    )

                        if len(semLoc) == 0:
                            # Parse mnemonic field as semantics.
                            semLoc = mnemLoc

                        parseSem(reader, semLoc, semNamespace, modeType)
                    except BadInput as ex:
                        reader.error(
                            'error in semantics: %s', ex, location=ex.location
                            )
                        # This is the last field.
                        continue
                    try:
                        semantics = semNamespace.createCodeBlock(log=reader)
                    except ValueError:
                        # Error was already logged inside createCodeBlock().
                        pass
                else:
                    semantics = None
        except DelayedError:
            pass
        else:
            template = CodeTemplate(semantics, placeholders, pc)
            entry = ModeEntry(encoding, mnemonic, template, placeholders)
            yield ParsedModeEntry(entry, decoding, flagsRequired)

def _formatEncodingWidth(width):
    return 'empty' if width is None else '%s bits wide' % width

def _determineEncodingWidth(entries, aux, modeName, logger):
    '''Returns the common encoding width for the given list of mode entries.
    Entries with a deviating encoding width will be logged as errors on the
    given logger and removed from the entries list.
    If the 'aux' argument is False, the first matched unit width of each entry
    is checked, otherwise the width of auxiliary encoding units is checked.
    If the entries represent instructions, pass None for the mode name.
    '''

    widthAttr = 'auxEncodingWidth' if aux else 'encodingWidth'

    widthFreqs = defaultdict(int)
    for entry in entries:
        widthFreqs[getattr(entry.entry.encoding, widthAttr)] += 1
    if aux:
        widthFreqs.pop(None, None)

    if len(widthFreqs) == 0:
        # Empty mode, only errors or aux check with no aux items.
        encWidth = None
    elif len(widthFreqs) == 1:
        # Single type.
        encWidth, = widthFreqs.keys()
    else:
        # Multiple widths; use one with the maximum frequency.
        encWidth, _ = max(widthFreqs.items(), key=lambda item: item[1])
        validWidths = (encWidth, None) if aux else (encWidth, )
        badEntryIndices = []
        for idx, entry in enumerate(entries):
            encDef = entry.entry.encoding
            if getattr(encDef, widthAttr) not in validWidths:
                logger.error(
                    '%sencoding match is %s, while %s is dominant %s',
                    ('auxiliary ' if aux else ''),
                    _formatEncodingWidth(getattr(encDef, widthAttr)),
                    _formatEncodingWidth(encWidth),
                    ('for instructions' if modeName is None else
                        'in mode "%s"' % modeName),
                    location=(encDef.auxEncodingLocation if aux
                        else encDef.encodingLocation)
                    )
                badEntryIndices.append(idx)
        for idx in reversed(badEntryIndices):
            del entries[idx]

    return encWidth

_reModeArgs = re.compile(_typeTok + r'\s' + _nameTok + r'$')

def _parseMode(
        reader, args, globalNamespace, pc, prefixes, modes, modeEntries,
        wantSemantics
        ):
    # Parse header line.
    match = args.match(_reModeArgs)
    if match is None:
        reader.error(
            'invalid mode arguments, expected "mode <type> <name>"',
            location=args
            )
        reader.skipBlock()
        return
    modeTypeLoc, modeNameLoc = match.groups
    try:
        semType = parseTypeDecl(modeTypeLoc.text)
    except ValueError as ex:
        reader.error('bad mode type: %s', ex, location=modeTypeLoc)
        semType = None

    # Check whether it's safe to add mode to namespace.
    modeName = modeNameLoc.text
    addMode = False
    if modeName in modes:
        reader.error(
            'mode "%s" redefined; first definition was on line %d',
            modeName, modes[modeName].location.lineno,
            location=modeNameLoc
            )
    else:
        try:
            parseType(modeName)
        except ValueError:
            addMode = True
        else:
            reader.error(
                'mode name "%s" conflicts with type', modeName,
                location=modeNameLoc
                )

    # Parse entries.
    parsedEntries = list(_parseModeEntries(
        reader, globalNamespace, pc, prefixes, modes, semType, (),
        _parseModeSemantics, wantSemantics
        ))

    # Create and remember mode object.
    encWidth = _determineEncodingWidth(parsedEntries, False, modeName, reader)
    auxEncWidth = _determineEncodingWidth(parsedEntries, True, modeName, reader)
    entries = tuple(parsedEntry.entry for parsedEntry in parsedEntries)
    mode = Mode(modeName, encWidth, auxEncWidth, semType, modeNameLoc, entries)
    if addMode:
        modes[modeName] = mode
        modeEntries[modeName] = parsedEntries

def _parseInstr(
        reader, args, globalNamespace, pc, prefixes, modes, wantSemantics
        ):
    mnemBase = tuple(_parseMnemonic(args, {}, reader))

    for instr in _parseModeEntries(
            reader, globalNamespace, pc, prefixes, modes, None, mnemBase,
            _parseInstrSemantics, wantSemantics
            ):
        encDef = instr.entry.encoding
        encWidth = encDef.encodingWidth
        if encWidth is None:
            reader.error(
                'instruction encoding must not be empty',
                location=encDef.encodingLocation
                )
            # Do not yield the instruction, to avoid this problem from being
            # reporting again as a width inconsistency.
            continue
        auxEncodingWidth = encDef.auxEncodingWidth
        if auxEncodingWidth not in (encWidth, None):
            reader.error(
                'auxiliary instruction encoding units are %s bits wide, '
                'while first unit is %s bits wide', auxEncodingWidth, encWidth,
                location=encDef.auxEncodingLocation
                )
        yield instr

_reHeader = re.compile(_nameTok + r'(?:\s+(.*\S)\s*)?$')

def parseInstrSet(pathname, logger=None, wantSemantics=True):
    if logger is None:
        logger = getLogger('parse-instr')
        logger.setLevel(WARNING)

    globalBuilder = StatelessCodeBlockBuilder()
    globalNamespace = GlobalNamespace(globalBuilder)
    prefixes = PrefixMappingFactory(globalNamespace)
    modes = {}
    modeEntries = {}
    instructions = modeEntries.setdefault(None, [])
    pc = None

    with DefLineReader.open(pathname, logger) as reader:
        for header in reader:
            if not header:
                continue
            match = header.match(_reHeader)
            if match is None:
                reader.error('malformed line outside block')
                continue
            keyword = match.group(1)
            args = match.group(2) if match.hasGroup(2) else header.endLocation
            defType = keyword.text
            if defType == 'reg':
                pc = _parseRegs(reader, args, globalNamespace)
            elif defType == 'io':
                _parseIO(reader, args, globalNamespace)
            elif defType == 'prefix':
                _parsePrefix(reader, args, globalNamespace, prefixes)
            elif defType == 'func':
                _parseFunc(reader, args, globalNamespace, wantSemantics)
            elif defType == 'mode':
                _parseMode(
                    reader, args, globalNamespace, pc, prefixes, modes,
                    modeEntries, wantSemantics
                    )
            elif defType == 'instr':
                instructions += _parseInstr(
                    reader, args, globalNamespace, pc, prefixes, modes,
                    wantSemantics
                    )
            else:
                reader.error(
                    'unknown definition type "%s"', defType, location=keyword
                    )
                reader.skipBlock()

        encWidth = _determineEncodingWidth(instructions, False, None, reader)
        anyAux = any(len(instr.entry.encoding) >= 2 for instr in instructions)
        auxEncWidth = encWidth if anyAux else None

        prefixMapping = prefixes.createMapping()

        instrSet = None
        if reader.errors == 0:
            try:
                instrSet = InstructionSet(
                    encWidth, auxEncWidth, globalNamespace, prefixMapping,
                    modeEntries
                    )
            except ValueError as ex:
                reader.error(
                    'final validation of instruction set failed: %s', ex
                    )

        reader.summarize()

    logger.debug('regs: %s', ', '.join(
        '%s = %r' % item for item in sorted(globalNamespace.items())
        ))

    return instrSet
