from .analysis import CodeTemplate
from .codeblock import ArgumentValue
from .codeblock_builder import (
    SemanticsCodeBlockBuilder, StatelessCodeBlockBuilder
    )
from .context_parser import MatchPlaceholderSpec, ValuePlaceholderSpec
from .expression import IntLiteral
from .expression_builder import (
    BadExpression, UnknownNameError, buildExpression, buildReference,
    convertDefinition
    )
from .expression_parser import (
    AssignmentNode, BranchNode, DeclarationNode, DefinitionNode, EmptyNode,
    FlagTestNode, IdentifierNode, LabelNode, MultiMatchNode, parseContext,
    parseExpr, parseExprList, parseInt, parseStatement
    )
from .function_builder import createFunc
from .instrset import InstructionSet
from .linereader import BadInput, DefLineReader, DelayedError, mergeSpan
from .mode import (
    EncodingExpr, EncodingMultiMatch, MatchPlaceholder, Mode, ModeEntry,
    ValuePlaceholder
    )
from .namespace import (
    ContextNamespace, GlobalNamespace, LocalNamespace, NameExistsError
    )
from .reference import ConcatenatedBits, FixedValue, SingleStorage, SlicedBits
from .storage import IOChannel, namePat
from .types import (
    IntType, ReferenceType, maskForWidth, parseType, parseTypeDecl, unlimited
    )
from collections import OrderedDict, defaultdict
from logging import WARNING, getLogger
import re

_nameTok = r'\s*(' + namePat + r')\s*'
_typeTok = r'\s*(' + namePat + r'&?)\s*'

def _parseRegs(reader, argStr, globalNamespace):
    headerLocation = reader.getLocation()
    if argStr:
        reader.error('register definition must have no arguments')

    for line in reader.iterBlock():
        parts = line.split('=')
        if len(parts) == 1:
            # base register
            parts = line.split()

            try:
                regType = parseType(parts[0])
            except ValueError as ex:
                reader.error(str(ex))
                continue

            for regName in parts[1:]:
                try:
                    globalNamespace.addVariable(
                        regName, regType, reader.getLocation()
                        )
                except ValueError as ex:
                    reader.error(str(ex))
                except NameExistsError as ex:
                    reader.error(
                        'error defining register: %s', ex, location=ex.location
                        )
        elif len(parts) == 2:
            # register alias

            # Parse left hand side.
            try:
                aliasTypeStr, aliasName = parts[0].split()
            except ValueError:
                reader.error(
                    'left hand side of register alias must be of the form '
                    '"<type> <name>"'
                    )
                continue
            try:
                aliasType = parseType(aliasTypeStr)
            except ValueError as ex:
                reader.error(str(ex))
                continue

            # Parse right hand side.
            rhsLoc = reader.getLocation((len(parts[0]) + 1, len(line)))
            try:
                tree = parseExpr(parts[1], rhsLoc)
                alias = buildReference(tree, globalNamespace)
            except BadInput as ex:
                reader.error(str(ex), location=ex.location)
                continue
            if alias.width != aliasType.width:
                reader.error(
                    'alias is declared as %s bits wide but its definition is '
                    '%s bits wide', aliasType.width, alias.width
                    )
                continue

            # Add alias definition.
            try:
                location = reader.getLocation()
                globalNamespace.define(aliasName, alias, location)
            except NameExistsError as ex:
                reader.error(
                    'error defining register alias: %s', ex,
                    location=ex.location
                    )
        else:
            reader.error('register definition line with multiple "="')

    # Check the program counter.
    if 'pc' not in globalNamespace:
        reader.error(
            'no program counter defined: '
            'a register or alias named "pc" is required',
            location=headerLocation
            )
        return None
    pcBits = globalNamespace['pc'].bits
    if isinstance(pcBits, SingleStorage):
        return pcBits.storage
    else:
        reader.error(
            'program counter must be a single register',
            location=globalNamespace.locations['pc']
            )
        return None

_reIOLine = re.compile(_nameTok + r'\s' + _nameTok + r'\[' + _nameTok + r'\]$')

def _parseIO(reader, argStr, namespace):
    if argStr:
        reader.error('I/O definition must have no arguments')

    for line in reader.iterBlock():
        match = _reIOLine.match(line)
        if match:
            elemTypeStr, name, addrTypeStr = match.groups()

            try:
                elemType = parseType(elemTypeStr)
            except ValueError as ex:
                reader.error(
                    'bad I/O element type: %s', ex,
                    location=reader.getLocation(match.span(1))
                    )
                elemType = None

            try:
                addrType = parseType(addrTypeStr)
            except ValueError as ex:
                reader.error(
                    'bad I/O address type: %s', ex,
                    location=reader.getLocation(match.span(3))
                    )
                addrType = None

            if elemType is None or addrType is None:
                continue

            channel = IOChannel(name, elemType, addrType)
            try:
                namespace.define(name, channel, reader.getLocation())
            except NameExistsError as ex:
                reader.error(
                    'error defining I/O channel: %s', ex, location=ex.location
                    )
        else:
            reader.error('invalid I/O definition line')

def _parseFuncArgs(log, argsStr):
    '''Parses a function arguments list, returning an OrderedDict.
    Errors are appended to the given log as they are discovered.
    '''
    argStrs = [] if not argsStr or argsStr.isspace() else argsStr.split(',')
    args = OrderedDict()
    for i, argStr in enumerate(argStrs, 1):
        try:
            typeStr, argName = argStr.split()
        except ValueError:
            log.error(
                'function argument %d not of the form "<type> <name>": %s',
                i, argStr
                )
        else:
            if argName == 'ret':
                log.error(
                    '"ret" is reserved for the return value; '
                    'it cannot be used as an argument name'
                    )
            elif argName in args:
                log.error(
                    'function argument %d has the same name as '
                    'an earlier argument: %s', i, argName
                    )
            else:
                try:
                    arg = parseTypeDecl(typeStr)
                except ValueError as ex:
                    log.error(
                        'bad function argument %d ("%s"): %s', i, argName, ex
                        )
                    # We still want to check for duplicates, so store
                    # a dummy value in the local namespace.
                    arg = None
                args[argName] = arg
    return args

_reFuncHeader = re.compile(
    r'(?:' + _typeTok + r'\s)?' + _nameTok + r'\((.*)\)$'
    )

def _parseFunc(reader, argStr, namespace, wantSemantics):
    headerLocation = reader.getLocation()

    # Parse header line.
    match = _reFuncHeader.match(argStr)
    if not match:
        reader.error('invalid function header line')
        reader.skipBlock()
        return
    retTypeStr, funcName, funcArgsStr = match.groups()

    # Parse return type.
    if retTypeStr is None:
        retType = None
    else:
        try:
            retType = parseTypeDecl(retTypeStr)
        except ValueError as ex:
            reader.error('bad return type: %s', ex)
            reader.skipBlock()
            return

    # Parse arguments.
    try:
        with reader.checkErrors():
            args = _parseFuncArgs(reader, funcArgsStr)
    except DelayedError:
        reader.skipBlock()
        return

    if wantSemantics:
        # Parse body lines.
        func = createFunc(reader, funcName, retType, args, namespace)

        # Store function in namespace.
        try:
            namespace.define(funcName, func, headerLocation)
        except NameExistsError as ex:
            reader.error(
                'error declaring function: %s', ex, location=ex.location
                )
    else:
        reader.skipBlock()

def _parseModeContext(ctxStr, ctxLoc, modes, reader):
    placeholderSpecs = OrderedDict()
    flagsRequired = set()
    for node in parseContext(ctxStr, ctxLoc):
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
            flagsRequired.add(node.name)
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
    try:
        encRef = buildReference(encNode, encNamespace)
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
    encBits = encRef.bits
    return EncodingExpr(encBits, encLoc)

def _parseModeEncoding(encNodes, placeholderSpecs, reader):
    # Define placeholders in encoding namespace.
    encNamespace = ContextNamespace(None)
    for name, spec in placeholderSpecs.items():
        if spec.value is None:
            encWidth = spec.encodingWidth
            if encWidth is not None:
                encType = IntType.u(encWidth)
                location = spec.decl.name.location
                try:
                    encNamespace.addValueArgument(name, encType, location)
                except NameExistsError as ex:
                    reader.error(
                        'bad placeholder: %s', ex, location=ex.location
                        )

    # Collect all identifiers and multi-matches used in the encoding.
    def collectNames(cls):
        for encNode in encNodes:
            for subNode in encNode:
                if isinstance(subNode, cls):
                    yield subNode.name
    identifiers = set(collectNames(IdentifierNode))
    multiMatches = set(collectNames(MultiMatchNode))

    # Evaluate encoding field.
    claimedMultiMatches = {}
    for encNode in encNodes:
        encLoc = encNode.treeLocation
        if isinstance(encNode, MultiMatchNode):
            # Match multiple encoding fields as-is.
            name = encNode.name
            try:
                placeholder = placeholderSpecs[name]
            except KeyError:
                reader.error(
                    'placeholder "%s" does not exist in context', name,
                    location=encLoc
                    )
                continue
            if not isinstance(placeholder, MatchPlaceholderSpec):
                reader.error(
                    'placeholder "%s" does not represent a mode match', name,
                    location=(encLoc, placeholder.decl.treeLocation)
                    )
                continue
            mode = placeholder.mode
            if name in claimedMultiMatches:
                reader.error(
                    'duplicate multi-match placeholder "%s@"', name,
                    location=(encLoc, claimedMultiMatches[name])
                    )
            else:
                claimedMultiMatches[name] = encLoc

            start = 1 if name in identifiers else 0
            # Technically there is nothing wrong with always matching zero
            # elements, but it is probably not what the user intended.
            modeWidth = mode.encodingWidth
            if modeWidth is None:
                reader.warning(
                    'mode "%s" does not contain encoding elements',
                    mode.name, location=(encLoc, placeholder.decl.treeLocation)
                    )
                continue
            modeAuxWidth = mode.auxEncodingWidth
            if start >= 1 and modeAuxWidth is None:
                reader.warning(
                    'mode "%s" does not match auxiliary encoding units',
                    mode.name, location=(encLoc, placeholder.decl.treeLocation)
                    )
                continue

            yield EncodingMultiMatch(name, mode, start, encLoc)
        else:
            # Expression possibly containing single encoding field matches.
            try:
                yield _parseEncodingExpr(
                    encNode, encNamespace, placeholderSpecs
                    )
            except BadInput as ex:
                reader.error('%s', ex, location=ex.location)

    # Check that our encoding field contains sufficient placeholders to be able
    # to make matches in all included mode tables.
    def encFullSpan():
        if len(encNodes) == 0:
            return reader.getLocation((0, 1))
        else:
            return mergeSpan(
                encNodes[0].treeLocation,
                encNodes[-1].treeLocation
                )
    for name, spec in placeholderSpecs.items():
        if isinstance(spec, ValuePlaceholderSpec):
            if spec.value is not None:
                # The value is computed, so we don't need to encode it.
                continue
            if name not in identifiers:
                reader.error(
                    'value placeholder "%s" does not occur in encoding', name,
                    location=(encFullSpan(), spec.decl.treeLocation)
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
                reader.error(
                    'no placeholder "%s" for mode "%s" in encoding',
                    name, mode.name,
                    location=(encFullSpan(), spec.decl.treeLocation)
                    )
            if mode.auxEncodingWidth is not None:
                reader.error(
                    'mode "%s" matches auxiliary encoding units, but there '
                    'is no "%s@" placeholder for them',
                    mode.name, name,
                    location=(encFullSpan(), spec.decl.treeLocation)
                    )
        else:
            assert False, spec

def _checkAuxEncodingWidth(encoding, logger):
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
    for encExpr in encoding:
        encLoc = encExpr.location
        if isinstance(encExpr, EncodingExpr):
            if firstUnitMatched:
                checkAux(encExpr.encodingWidth, encLoc)
            else:
                firstUnitMatched = True
        elif isinstance(encExpr, EncodingMultiMatch):
            mode = encExpr.mode
            modeAuxWidth = mode.auxEncodingWidth
            if firstUnitMatched:
                if encExpr.start == 0:
                    checkAux(mode.encodingWidth, encLoc)
                if modeAuxWidth is not None:
                    checkAux(modeAuxWidth, encLoc)
            else:
                if modeAuxWidth is not None:
                    if mode.encodedLength != encExpr.start + 1:
                        checkAux(modeAuxWidth, encLoc)
                firstUnitMatched = True
        else:
            assert False, encExpr

def _decomposeBitString(bits):
    if isinstance(bits, FixedValue):
        yield bits.expr, 0, 0, bits.width
    elif isinstance(bits, ConcatenatedBits):
        offset = 0
        for sub in bits:
            for expr, immIdx, refIdx, width in _decomposeBitString(sub):
                yield expr, immIdx, offset + refIdx, width
            offset += sub.width
    elif isinstance(bits, SlicedBits):
        # Note that SlicedBits has already simplified the offset.
        offset = bits.offset
        if isinstance(offset, IntLiteral):
            start = offset.value
            end = start + bits.width
            for expr, immIdx, bitsIdx, width in _decomposeBitString(bits.bits):
                # Clip to slice boundaries.
                bitsStart = max(bitsIdx, start)
                bitsEnd = min(bitsIdx + width, end)
                # Output if clipped slice is not empty.
                width = bitsEnd - bitsStart
                if width > 0:
                    immShift = bitsStart - bitsIdx
                    yield expr, immIdx + immShift, bitsStart - start, width
        else:
            raise ValueError('slices in encoding must have fixed offset')
    else:
        # Note: SingleStorage cannot occur in encoding since the stateless
        #       builder would trigger an error when loading from it.
        assert False, bits

def _decomposeEncodingExprs(encElems, reader):
    fixedMatcher = []
    decodeMap = defaultdict(list)
    for encIdx, encElem in enumerate(encElems):
        if not isinstance(encElem, EncodingExpr):
            continue
        fixedMask = 0
        fixedValue = 0
        try:
            for expr, immIdx, refIdx, width in _decomposeBitString(
                    encElem.bits
                    ):
                if isinstance(expr, ArgumentValue):
                    decodeMap[expr.name].append(
                        (immIdx, encIdx, refIdx, width)
                        )
                elif isinstance(expr, IntLiteral):
                    mask = maskForWidth(width) << refIdx
                    fixedMask |= mask
                    fixedValue |= ((expr.value >> immIdx) << refIdx) & mask
                else:
                    raise ValueError('unsupported operation in encoding')
        except ValueError as ex:
            # TODO: This message is particularly unclear, because we do not
            #       have the exact location nor can we print the offending
            #       expression.
            #       We could store locations in non-simplified expressions
            #       or decompose parse trees instead of references.
            reader.error('%s', ex, location=encElem.location)
        else:
            if fixedMask != 0:
                fixedMatcher.append((encIdx, fixedMask, fixedValue))
    return fixedMatcher, decodeMap

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
    '''Verifies that there is an order in which placeholder can be decoded.
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
        with reader.checkErrors():
            # Decompose the encoding expressions.
            fixedMatcher, decodeMap = _decomposeEncodingExprs(encoding, reader)
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

def _parseModeSemantics(semStr, semLoc, semNamespace, modeType):
    semantics = parseExpr(semStr, semLoc)
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

def _rejectNodeClasses(node, badClasses):
    if isinstance(node, badClasses):
        raise BadInput(
            '%s is not allowed here', node.__class__.__name__[:-4].lower(),
            location=node.treeLocation
            )

def _parseInstrSemantics(semStr, semLoc, namespace, modeType):
    assert modeType is None, modeType
    node = parseStatement(semStr, semLoc)
    if isinstance(node, AssignmentNode):
        _rejectNodeClasses(node.lhs, (DefinitionNode, DeclarationNode))
        lhs = buildReference(node.lhs, namespace)
        rhs = buildExpression(node.rhs, namespace)
        lhs.emitStore(namespace.builder, rhs, node.lhs.treeLocation)
    elif isinstance(node, EmptyNode):
        pass
    else:
        _rejectNodeClasses(node, (
            DefinitionNode, DeclarationNode, BranchNode, LabelNode
            ))
        buildExpression(node, namespace)

_reMnemonic = re.compile(r"\w+'?|[$%]\w+|[^\w\s]")

def _parseMnemonic(mnemStr, mnemLoc, placeholders, reader):
    seenPlaceholders = {}
    for match in _reMnemonic.finditer(mnemStr):
        def getMatchLocation():
            span = match.span()
            shift = mnemLoc.span[0]
            return mnemLoc.updateSpan((shift + span[0], shift + span[1]))

        text = match.group()
        placeholder = placeholders.get(text)
        if placeholder is None:
            if '0' <= text[0] <= '9' or text[0] in '$%':
                try:
                    value, width = parseInt(text)
                except ValueError as ex:
                    reader.error('%s', ex, location=getMatchLocation())
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
                location=(getMatchLocation(), seenPlaceholders[text])
                )
        else:
            yield placeholder
            seenPlaceholders[text] = getMatchLocation()

_reDotSep = re.compile(r'\s*(?:\.\s*|$)')

def _parseModeEntries(
        reader, globalNamespace, pc, modes, modeType, mnemBase, parseSem,
        wantSemantics
        ):
    for line in reader.iterBlock():
        # Split mode line into 4 fields.
        fields = list(reader.splitOn(_reDotSep.finditer(line)))
        if len(fields) < 2:
            reader.error('field separator "." missing in mode line')
            continue
        if len(fields) > 4:
            reader.error('too many fields (%d) in mode line', len(fields))
            continue
        fields += [('', None)] * (4 - len(fields))
        (encStr, encLoc), (mnemStr, mnemLoc), (semStr, semLoc), \
                (ctxStr, ctxLoc) = fields

        try:
            with reader.checkErrors():
                # Parse context.
                if ctxStr:
                    try:
                        with reader.checkErrors():
                            placeholderSpecs, flagsRequired = _parseModeContext(
                                ctxStr, ctxLoc, modes, reader
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
                if encStr:
                    try:
                        # Parse encoding field.
                        encNodes = parseExprList(encStr, encLoc)
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
                    # Evaluate encoding field in encoding builder.
                    try:
                        with reader.checkErrors():
                            encoding = tuple(_parseModeEncoding(
                                encNodes, placeholderSpecs, reader
                                ))
                        with reader.checkErrors():
                            _checkAuxEncodingWidth(encoding, reader)
                    except DelayedError:
                        encoding = None
                if encoding is None:
                    decoding = None
                else:
                    decoding = _parseModeDecoding(
                        encoding, placeholderSpecs, reader
                        )

                # Parse mnemonic.
                mnemonic = mnemBase + tuple(_parseMnemonic(
                    mnemStr, mnemLoc, placeholders, reader
                    ))
                if len(mnemonic) == 0:
                    reader.error('missing mnemonic', location=mnemLoc)

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

                        if not semStr:
                            # Parse mnemonic field as semantics.
                            semStr = mnemStr
                            semLoc = mnemLoc

                        parseSem(semStr, semLoc, semNamespace, modeType)
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
            if semantics is None:
                template = None
            else:
                template = CodeTemplate(semantics, placeholders, pc)
            yield ModeEntry(
                encoding, decoding, mnemonic, template, placeholders,
                flagsRequired, reader.getLocation()
                )

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
        if entry.encoding is not None:
            widthFreqs[getattr(entry, widthAttr)] += 1
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
            if getattr(entry, widthAttr) not in validWidths:
                logger.error(
                    '%sencoding match is %s, while %s is dominant %s',
                    ('auxiliary ' if aux else ''),
                    _formatEncodingWidth(getattr(entry, widthAttr)),
                    _formatEncodingWidth(encWidth),
                    ('for instructions' if modeName is None else
                        'in mode "%s"' % modeName),
                    location=(entry.auxEncodingLocation if aux
                        else entry.encodingLocation)
                    )
                badEntryIndices.append(idx)
        for idx in reversed(badEntryIndices):
            del entries[idx]

    return encWidth

_reModeHeader = re.compile(r'mode\s+' + _typeTok + r'\s' + _nameTok + r'$')

def _parseMode(reader, globalNamespace, pc, modes, wantSemantics):
    # Parse header line.
    modeLocation = reader.getLocation()
    match = _reModeHeader.match(modeLocation.line)
    if not match:
        reader.error('invalid mode header, expected "mode <type> <name>"')
        reader.skipBlock()
        return
    modeTypeStr, modeName = match.groups()
    try:
        semType = parseTypeDecl(modeTypeStr)
    except ValueError as ex:
        reader.error(
            'bad mode type: %s', ex,
            location=modeLocation.updateSpan(match.span(1))
            )
        semType = None

    # Check whether it's safe to add mode to namespace.
    addMode = False
    if modeName in modes:
        reader.error(
            'mode "%s" redefined; first definition was on line %d',
            modeName, modes[modeName].location.lineno,
            location=modeLocation.updateSpan(match.span(2))
            )
    else:
        try:
            parseType(modeName)
        except ValueError:
            addMode = True
        else:
            reader.error(
                'mode name "%s" conflicts with type', modeName,
                location=modeLocation.updateSpan(match.span(2))
                )

    # Parse entries.
    entries = list(_parseModeEntries(
        reader, globalNamespace, pc, modes, semType, (), _parseModeSemantics,
        wantSemantics
        ))

    # Create and remember mode object.
    encWidth = _determineEncodingWidth(entries, False, modeName, reader)
    auxEncWidth = _determineEncodingWidth(entries, True, modeName, reader)
    mode = Mode(modeName, encWidth, auxEncWidth, semType, modeLocation, entries)
    if addMode:
        modes[modeName] = mode

def _parseInstr(reader, argStr, globalNamespace, pc, modes, wantSemantics):
    mnemBase = tuple(_parseMnemonic(argStr, None, {}, reader))

    for instr in _parseModeEntries(
            reader, globalNamespace, pc, modes, None, mnemBase,
            _parseInstrSemantics, wantSemantics
            ):
        encWidth = instr.encodingWidth
        if encWidth is None:
            reader.error(
                'instruction encoding must not be empty',
                location=instr.encodingLocation
                )
            # Do not yield the instruction, to avoid this problem from being
            # reporting again as a width inconsistency.
            continue
        auxEncodingWidth = instr.auxEncodingWidth
        if auxEncodingWidth not in (encWidth, None):
            reader.error(
                'auxiliary instruction encoding units are %s bits wide, '
                'while first unit is %s bits wide', auxEncodingWidth, encWidth,
                location=instr.auxEncodingLocation
                )
        yield instr

def parseInstrSet(pathname, logger=None, wantSemantics=True):
    if logger is None:
        logger = getLogger('parse-instr')
        logger.setLevel(WARNING)

    globalBuilder = StatelessCodeBlockBuilder()
    globalNamespace = GlobalNamespace(globalBuilder)
    modes = {}
    instructions = []
    pc = None

    with DefLineReader.open(pathname, logger) as reader:
        for header in reader:
            if not header:
                continue
            parts = header.split(maxsplit=1)
            defType = parts[0]
            argStr = '' if len(parts) == 1 else parts[1]
            if defType == 'reg':
                pc = _parseRegs(reader, argStr, globalNamespace)
            elif defType == 'io':
                _parseIO(reader, argStr, globalNamespace)
            elif defType == 'func':
                _parseFunc(reader, argStr, globalNamespace, wantSemantics)
            elif defType == 'mode':
                _parseMode(reader, globalNamespace, pc, modes, wantSemantics)
            elif defType == 'instr':
                instructions += _parseInstr(
                    reader, argStr, globalNamespace, pc, modes, wantSemantics
                    )
            else:
                reader.error('unknown definition type "%s"', defType)
                reader.skipBlock()

        encWidth = _determineEncodingWidth(instructions, False, None, reader)

        reader.summarize()

    logger.debug('regs: %s', ', '.join(
        '%s = %r' % item for item in sorted(globalNamespace.items())
        ))

    if reader.errors == 0:
        anyAux = any(len(instr.encoding) >= 2 for instr in instructions)
        auxEncWidth = encWidth if anyAux else None
        return InstructionSet(
            encWidth, auxEncWidth, globalNamespace, instructions
            )
    else:
        return None
