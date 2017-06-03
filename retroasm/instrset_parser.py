from .codeblock import (
    ComputedConstant, ConcatenatedReference, FixedValue, SlicedReference
    )
from .codeblock_builder import (
    EncodingCodeBlockBuilder, GlobalCodeBlockBuilder, LocalCodeBlockBuilder
    )
from .expression import IntLiteral
from .expression_builder import (
    UnknownNameError, buildExpression, buildReference, convertDefinition
    )
from .expression_parser import (
    AssignmentNode, BranchNode, DeclarationNode, DefinitionNode, EmptyNode,
    FlagTestNode, IdentifierNode, LabelNode, NumberNode, parseContext,
    parseExpr, parseExprList, parseInt, parseStatement
    )
from .function_builder import createFunc
from .instrset import InstructionSet
from .linereader import BadInput, DefLineReader, DelayedError, mergeSpan
from .mode import (
    EncodingExpr, Immediate, MatchPlaceholder, Mode, ModeEntry, ValuePlaceholder
    )
from .namespace import GlobalNamespace, NameExistsError
from .storage import IOChannel, Variable, namePat
from .types import IntType, ReferenceType, parseType, parseTypeDecl, unlimited

from collections import OrderedDict, defaultdict
from logging import getLogger
import re

logger = getLogger('parse-instr')

_nameTok = r'\s*(' + namePat + r')\s*'
_typeTok = r'\s*(' + namePat + r'&?)\s*'

def _parseRegs(reader, argStr, builder):
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
                    builder.emitVariable(regName, regType, reader.getLocation())
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
                alias = buildReference(tree, builder)
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
                builder.defineReference(aliasName, alias, location)
            except NameExistsError as ex:
                reader.error(
                    'error defining register alias: %s', ex,
                    location=ex.location
                    )
        else:
            reader.error('register definition line with multiple "="')

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

def _parseFunc(reader, argStr, builder, wantSemantics):
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
        func = createFunc(reader, funcName, retType, args, builder)

        # Store function in global namespace.
        try:
            builder.namespace.define(funcName, func, headerLocation)
        except NameExistsError as ex:
            reader.error(
                'error declaring function: %s', ex, location=ex.location
                )
    else:
        reader.skipBlock()

def _parseModeContext(ctxStr, ctxLoc, modes, reader):
    placeholders = OrderedDict()
    flagsRequired = set()
    for node in parseContext(ctxStr, ctxLoc):
        if isinstance(node, (DeclarationNode, DefinitionNode)):
            decl = node if isinstance(node, DeclarationNode) else node.decl
            name = decl.name.name
            typeName = decl.type.name

            # Figure out whether the name is a mode or type.
            mode = modes.get(typeName)
            if mode is not None:
                placeholder = MatchPlaceholder(decl, mode)
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
                    typ = parseType(typeName)
                except ValueError:
                    reader.error(
                        'there is no type or mode named "%s"', typeName,
                        location=decl.type.location
                        )
                    continue
                value = node.value if isinstance(node, DefinitionNode) else None
                placeholder = ValuePlaceholder(decl, typ, value)
            if name in placeholders:
                reader.error(
                    'multiple placeholders named "%s"', name,
                    location=decl.name.location
                    )
            else:
                placeholders[name] = placeholder
        elif isinstance(node, FlagTestNode):
            flagsRequired.add(node.name)
        else:
            assert False, node

    return placeholders, flagsRequired

def _buildPlaceholder(placeholder, typ, builder):
    decl = placeholder.decl
    name = decl.name.name
    value = placeholder.value
    if value is not None:
        convertDefinition(decl.kind, decl.name, typ, value, builder)
    elif isinstance(typ, ReferenceType):
        builder.emitReferenceArgument(name, typ.type, decl.name.location)
    else:
        immediate = Immediate(name, typ, decl.treeLocation)
        ref = builder.emitFixedValue(immediate, typ)
        builder.defineReference(name, ref, decl.name.location)

def _parseModeEncoding(encNodes, encBuilder, placeholders, reader):
    # Define placeholders in encoding builder.
    # Errors are stored rather than reported immediately, since it is possible
    # to define expressions that are valid as semantics but not as encodings.
    # This is not a problem as long as the associated name is not used in the
    # encoding field. For example relative addressing reads the base address
    # from a register.
    encErrors = {}
    for name, placeholder in placeholders.items():
        encWidth = placeholder.encodingWidth
        if encWidth is not None:
            encType = IntType.u(encWidth)
            try:
                _buildPlaceholder(placeholder, encType, encBuilder)
            except BadInput as ex:
                encErrors[name] = ex

    # Evaluate encoding field.
    for encNode in encNodes:
        encLoc = encNode.treeLocation
        try:
            encRef = buildReference(encNode, encBuilder)
        except BadInput as ex:
            if isinstance(ex, UnknownNameError):
                placeholder = placeholders.get(ex.name)
                if placeholder is not None \
                        and placeholder.encodingWidth is None:
                    reader.error(
                        'cannot use placeholder "%s" in encoding field, '
                        'since mode "%s" has an empty encoding sequence',
                        ex.name, placeholder.mode.name,
                        location=ex.location
                        )
                    continue
                ex = encErrors.get(ex.name, ex)
            reader.error(
                'error in encoding: %s', ex,
                location=ex.location
                )
            continue
        try:
            encValue = encRef.emitLoad(encLoc)
        except BadInput as ex:
            reader.error(
                'error evaluating encoding: %s', ex,
                location=ex.location
                )
            continue
        if encRef.width is unlimited:
            reader.error(
                'unlimited width integers are not allowed in encoding',
                location=encLoc
                )
        yield EncodingExpr(encRef, encValue, encLoc)

def _decomposeEncoding(ref, location, reader):
    if isinstance(ref, FixedValue):
        const = ref.const
        assert isinstance(const, ComputedConstant), const
        expr = const.expr
        if isinstance(expr, Immediate):
            yield expr.name, 0, 0, ref.width
        elif isinstance(expr, IntLiteral):
            pass
        else:
            # TODO: This message is particularly unclear, because we do not
            #       have the exact location nor can we print the expression.
            reader.error('unsupported operation in encoding', location=location)
    elif isinstance(ref, ConcatenatedReference):
        offset = 0
        for subRef in ref:
            for name, immIdx, refIdx, width in _decomposeEncoding(
                    subRef, location, reader
                    ):
                yield name, immIdx, offset + refIdx, width
            offset += subRef.width
    elif isinstance(ref, SlicedReference):
        # Note that SlicedReference has already simplified the offset.
        offset = ref.offset
        if isinstance(offset, IntLiteral):
            start = offset.value
            end = start + ref.width
            for name, immIdx, refIdx, width in _decomposeEncoding(
                    ref.ref, location, reader
                    ):
                # Clip to slice boundaries.
                refStart = max(refIdx, start)
                refEnd = min(refIdx + width, end)
                # Output if clipped slice is not empty.
                width = refEnd - refStart
                if width > 0:
                    immShift = refStart - refIdx
                    yield name, immIdx + immShift, refStart - start, width
        else:
            # TODO: This message is particularly unclear, because we do not
            #       have the exact location nor can we print the offset.
            reader.error(
                'slices in encoding must have fixed offset',
                location=location
                )
    else:
        # Note: SingleReference cannot occur in encoding since the stateless
        #       builder would trigger an error when loading from it.
        assert False, ref

def _parseModeDecoding(encoding, encBuilder, reader):
    '''Construct a mapping that, given an encoded instruction, produces the
    values for context placeholders.
    '''

    # Decompose the references.
    decodeMap = defaultdict(list)
    try:
        with reader.checkErrors():
            for encIdx, encElem in enumerate(encoding):
                for name, immIdx, refIdx, width in _decomposeEncoding(
                        encElem.ref, encElem.location, reader
                        ):
                    decodeMap[name].append((immIdx, encIdx, refIdx, width))
    except DelayedError:
        return None

    # Gather all Immediate objects from the constant pool.
    immediates = {
        value.name: value
        for value in (
            const.expr
            for const in encBuilder.constants
            if isinstance(const, ComputedConstant)
            )
        if isinstance(value, Immediate)
        }

    try:
        with reader.checkErrors():
            # Check whether all immediates can be decoded.
            missingNames = set(immediates.keys()) - set(decodeMap.keys())
            for name in missingNames:
                # Zero-width immediates need not occur in the encoding, since
                # they can have only one possible value. These immediates are
                # used when an included mode has empty encoding fields, for
                # example because it matches using only decode flags.
                if immediates[name].width != 0:
                    reader.error(
                        'placeholder "%s" does not occur in encoding', name,
                        location=immediates[name].location
                        )

            # Create a mapping from opcode to immediate values.
            sequentialMap = {}
            for name, slices in decodeMap.items():
                immWidth = immediates[name].width
                decoding = []
                problems = []
                prev = 0
                for immIdx, encIdx, refIdx, width in sorted(slices):
                    if prev < immIdx:
                        problems.append(
                            'gap at [%d:%d]' % (prev, immIdx)
                            )
                    elif prev > immIdx:
                        problems.append(
                            'overlap at [%d:%d]'
                            % (immIdx, min(immIdx + width, prev))
                            )
                    prev = max(immIdx + width, prev)
                    decoding.append((encIdx, refIdx, width))
                if prev < immWidth:
                    problems.append('gap at [%d:%d]' % (prev, immWidth))
                elif prev > immWidth:
                    assert False, (name, slices)
                if problems:
                    reader.error(
                        'cannot decode value for "%s": %s',
                        name, ', '.join(problems),
                        location=immediates[name].location
                        )
                else:
                    sequentialMap[name] = decoding
    except DelayedError:
        return None
    else:
        return sequentialMap

def _parseModeSemantics(semStr, semLoc, semBuilder, modeType):
    semantics = parseExpr(semStr, semLoc)
    if isinstance(modeType, ReferenceType):
        ref = buildReference(semantics, semBuilder)
        if modeType is not None:
            if ref.type != modeType.type:
                raise BadInput(
                    'semantics type %s does not match mode type %s'
                    % (ref.type, modeType.type),
                    location=semLoc
                    )
        semBuilder.defineReference('ret', ref, semLoc)
    else:
        expr = buildExpression(semantics, semBuilder)
        # Note that modeType can be None because of earlier errors.
        if modeType is not None:
            retRef = semBuilder.emitVariable('ret', modeType, semLoc)
            retRef.emitStore(expr, semLoc)

def _rejectNodeClasses(node, badClasses):
    if isinstance(node, badClasses):
        raise BadInput(
            '%s is not allowed here', node.__class__.__name__[:-4].lower(),
            location=node.treeLocation
            )

def _parseInstrSemantics(semStr, semLoc, builder, modeType):
    assert modeType is None, modeType
    node = parseStatement(semStr, semLoc)
    if isinstance(node, AssignmentNode):
        _rejectNodeClasses(node.lhs, (DefinitionNode, DeclarationNode))
        lhs = buildReference(node.lhs, builder)
        rhs = buildExpression(node.rhs, builder)
        lhs.emitStore(builder.emitCompute(rhs), node.lhs.treeLocation)
    elif isinstance(node, EmptyNode):
        pass
    else:
        _rejectNodeClasses(node, (
            DefinitionNode, DeclarationNode, BranchNode, LabelNode
            ))
        buildExpression(node, builder)

_reMnemonic = re.compile(r"\w+'?|[$%]\w+|[^\w\s]")

def _parseMnemonic(mnemStr, mnemLoc, placeholders, reader):
    seenPlaceholders = set()
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
            span = match.span()
            shift = mnemLoc.span[0]
            reader.error(
                'placeholder "%s" occurs multiple times in mnemonic', text,
                location=getMatchLocation()
                )
        else:
            yield placeholder
            seenPlaceholders.add(text)

_reDotSep = re.compile(r'\s*(?:\.\s*|$)')

def _parseModeEntries(
        reader, globalBuilder, modes, modeType, mnemBase, parseSem,
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
                encBuilder = EncodingCodeBlockBuilder(globalBuilder)
                semBuilder = LocalCodeBlockBuilder(globalBuilder)

                # Parse context.
                if ctxStr:
                    try:
                        with reader.checkErrors():
                            placeholders, flagsRequired = _parseModeContext(
                                ctxStr, ctxLoc, modes, reader
                                )
                    except DelayedError:
                        # To avoid error spam, skip this line.
                        continue
                else:
                    placeholders, flagsRequired = {}, set()

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
                                encNodes, encBuilder, placeholders, reader
                                ))
                    except DelayedError:
                        encoding = None
                if encoding is None:
                    decoding = None
                else:
                    decoding = _parseModeDecoding(encoding, encBuilder, reader)

                # Parse mnemonic.
                mnemonic = mnemBase + tuple(_parseMnemonic(
                    mnemStr, mnemLoc, placeholders, reader
                    ))
                if len(mnemonic) == 0:
                    reader.error('missing mnemonic', location=mnemLoc)

                # Parse semantics.
                if wantSemantics:
                    try:
                        # Define placeholders in semantics builder.
                        for placeholder in placeholders.values():
                            semType = placeholder.semanticsType
                            _buildPlaceholder(placeholder, semType, semBuilder)

                        # Parse semantics field.
                        if not semStr:
                            # Parse mnemonic as semantics.
                            semStr = mnemStr
                            semLoc = mnemLoc
                        parseSem(semStr, semLoc, semBuilder, modeType)
                    except BadInput as ex:
                        reader.error(
                            'error in semantics: %s', ex, location=ex.location
                            )
        except DelayedError:
            pass
        else:
            context = semBuilder.namespace
            yield ModeEntry(
                encoding, decoding, mnemonic, semBuilder, context, flagsRequired
                )

def _formatEncodingWidth(width):
    return 'empty' if width is None else '%d bits wide' % width

def _checkEncodingWidth(encElems, encWidth, modeName, logger):
    allGood = True
    where = (
        'for instructions'
        if modeName is None else
        'in mode "%s"' % modeName
        )
    for encElem in encElems:
        if encElem.width != encWidth:
            logger.error(
                'encoding field is %s, while %s is dominant %s',
                _formatEncodingWidth(encElem.width),
                _formatEncodingWidth(encWidth),
                where,
                location=encElem.location
                )
            allGood = False
    return allGood

def _determineEncodingWidth(entries, modeName, logger):
    '''Returns the common encoding width for the given list of mode entries.
    Entries with a deviating encoding width will be logged as errors on the
    given logger and removed from the entries list.
    If the entries represent instructions, pass None for the mode name.
    '''

    widthFreqs = defaultdict(int)
    for entry in entries:
        if entry.encoding is not None:
            widthFreqs[entry.encodingWidth] += 1

    if len(widthFreqs) == 0:
        # Empty mode or only errors; use dummy type.
        encWidth = 0
    elif len(widthFreqs) == 1:
        # Single type.
        encWidth, = widthFreqs.keys()
    else:
        # Multiple widths; use one with the maximum frequency.
        encWidth, _ = max(widthFreqs.items(), key=lambda item: item[1])
        badEntryIndices = []
        for idx, entry in enumerate(entries):
            if not _checkEncodingWidth(
                    entry.encoding[:1], encWidth, modeName, logger
                    ):
                badEntryIndices.append(idx)
        for idx in reversed(badEntryIndices):
            del entries[idx]

    return encWidth

_reModeHeader = re.compile(r'mode\s+' + _typeTok + r'\s' + _nameTok + r'$')

def _parseMode(reader, globalBuilder, modes, wantSemantics):
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

    # Check whether it's safe to add mode to builder's namespace.
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
        reader, globalBuilder, modes, semType, (), _parseModeSemantics,
        wantSemantics
        ))

    # Create and remember mode object.
    encWidth = _determineEncodingWidth(entries, modeName, reader)
    mode = Mode(modeName, encWidth, semType, modeLocation, entries)
    if addMode:
        modes[modeName] = mode

def _parseInstr(reader, argStr, globalBuilder, modes, wantSemantics):
    mnemBase = tuple(_parseMnemonic(argStr, None, {}, reader))

    for entry in _parseModeEntries(
            reader, globalBuilder, modes, None, mnemBase, _parseInstrSemantics,
            wantSemantics
            ):
        yield entry

def parseInstrSet(pathname, wantSemantics=True):
    globalNamespace = GlobalNamespace()
    builder = GlobalCodeBlockBuilder(globalNamespace)
    modes = {}
    instructions = []

    with DefLineReader.open(pathname, logger) as reader:
        for header in reader:
            if not header:
                continue
            parts = header.split(maxsplit=1)
            defType = parts[0]
            argStr = '' if len(parts) == 1 else parts[1]
            if defType == 'reg':
                _parseRegs(reader, argStr, builder)
            elif defType == 'io':
                _parseIO(reader, argStr, globalNamespace)
            elif defType == 'func':
                _parseFunc(reader, argStr, builder, wantSemantics)
            elif defType == 'mode':
                _parseMode(reader, builder, modes, wantSemantics)
            elif defType == 'instr':
                instructions += _parseInstr(
                    reader, argStr, builder, modes, wantSemantics
                    )
            else:
                reader.error('unknown definition type "%s"', defType)
                reader.skipBlock()

        encWidth = _determineEncodingWidth(instructions, None, reader)
        # Note: Additional encoding elements in modes will be auto-appended
        #       whole to instructions that use those modes, so they should
        #       have the same width.
        for mode in sorted(modes.values(), key=lambda m: m.location.lineno):
            for entry in mode:
                _checkEncodingWidth(entry.encoding[1:], encWidth, None, reader)
        for instr in instructions:
            _checkEncodingWidth(instr.encoding, encWidth, None, reader)

        reader.summarize()

    logger.debug('regs: %s', ', '.join(
        '%s = %r' % item for item in sorted(globalNamespace.items())
        ))

    if reader.errors == 0:
        return InstructionSet(encWidth, instructions)
    else:
        return None

def checkInstrSet(pathname):
    logger.info('checking: %s', pathname)
    instrSet = parseInstrSet(pathname)
