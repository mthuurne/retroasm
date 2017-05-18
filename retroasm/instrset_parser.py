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
    parseExpr, parseExprList, parseStatement
    )
from .function_builder import createFunc
from .instrset import InstructionSet
from .linereader import BadInput, DefLineReader, DelayedError, mergeSpan
from .mode import Immediate, Mode
from .namespace import GlobalNamespace, NameExistsError
from .storage import IOChannel, Variable, namePat
from .types import IntType, ReferenceType, parseType, parseTypeDecl

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

        func.dump()
        print()
    else:
        reader.skipBlock()

def _parseModeContext(ctxStr, ctxLoc, encBuilder, semBuilder, modes, reader):
    flagsRequired = set()
    encErrors = {}
    for node in parseContext(ctxStr, ctxLoc):
        if isinstance(node, (DeclarationNode, DefinitionNode)):
            decl = node if isinstance(node, DeclarationNode) else node.decl
            name = decl.name.name
            nameLoc = decl.name.location
            typeName = decl.type.name

            # Figure out whether the name is a mode or type.
            mode = modes.get(typeName)
            if mode is not None:
                encType = mode.encodingType
                semType = mode.semanticsType
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
                    encType = semType = parseType(typeName)
                except ValueError:
                    reader.error(
                        'there is no type or mode named "%s"', typeName,
                        location=decl.type.location
                        )
                    continue

            reportedNameExists = []
            def reportNameExists(ex):
                if not reportedNameExists:
                    reader.error('%s', ex, location=ex.location)
                    reportedNameExists.append(None)

            def emitImmediate(builder, typ):
                immediate = Immediate(name, typ, decl.treeLocation)
                ref = builder.emitFixedValue(immediate, typ)
                try:
                    builder.defineReference(name, ref, nameLoc)
                except NameExistsError as ex:
                    reportNameExists(ex)

            # Define name in encoding builder.
            # Errors are stored rather than reported immediately, since it is
            # possible to define expressions that are valid as semantics but
            # not as encodings. This is not a problem as long as the associated
            # name is not used in the encoding field. For example relative
            # addressing reads the base address from a register.
            if isinstance(node, DefinitionNode):
                try:
                    convertDefinition(
                        decl.kind, decl.name, encType, node.value, encBuilder
                        )
                except NameExistsError as ex:
                    reportNameExists(ex)
                except BadInput as ex:
                    encErrors[name] = ex
            elif isinstance(encType, ReferenceType):
                assert False, encType
            else:
                emitImmediate(encBuilder, encType)

            # Define name in semantics builder.
            if isinstance(node, DefinitionNode):
                try:
                    convertDefinition(
                        decl.kind, decl.name, semType, node.value, semBuilder
                        )
                except NameExistsError as ex:
                    reportNameExists(ex)
                except BadInput as ex:
                    reader.error('%s', ex, location=ex.location)
            elif isinstance(semType, ReferenceType):
                semBuilder.emitReferenceArgument(name, semType.type, nameLoc)
            else:
                emitImmediate(semBuilder, semType)
        elif isinstance(node, FlagTestNode):
            flagsRequired.add(node.name)
        else:
            assert False, node

    return flagsRequired, encErrors

def _parseModeEncoding(encNodes, encBuilder, encErrors, reader):
    for encNode in encNodes:
        encLoc = encNode.treeLocation
        try:
            encRef = buildReference(encNode, encBuilder)
        except BadInput as ex:
            if isinstance(ex, UnknownNameError):
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
        yield encRef, encValue, encLoc

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
            for encIdx, (encRef, _, encLoc) in enumerate(encoding):
                for name, immIdx, refIdx, width in _decomposeEncoding(
                        encRef, encLoc, reader
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

_reMnemonic = re.compile(r'\w+|[^\w\s]')

def _parseMnemonic(mnemStr):
    return tuple(_reMnemonic.findall(mnemStr))

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
                            flagsRequired, encErrors = _parseModeContext(
                                ctxStr, ctxLoc, encBuilder, semBuilder, modes,
                                reader
                                )
                    except DelayedError:
                        # To avoid error spam, skip this line.
                        continue
                else:
                    flagsRequired, encErrors = set(), {}

                # Parse encoding.
                if encStr:
                    try:
                        encNodes = parseExprList(encStr, encLoc)
                    except BadInput as ex:
                        reader.error(
                            'error in encoding: %s', ex, location=ex.location
                            )
                        encNodes = None
                else:
                    encNodes = (NumberNode(0, 0, encLoc), )
                if encNodes is None:
                    encoding = None
                else:
                    try:
                        with reader.checkErrors():
                            encoding = tuple(_parseModeEncoding(
                                encNodes, encBuilder, encErrors, reader
                                ))
                    except DelayedError:
                        encoding = None
                if encoding is None:
                    decoding = None
                else:
                    decoding = _parseModeDecoding(encoding, encBuilder, reader)

                # Parse mnemonic.
                mnemonic = mnemBase + _parseMnemonic(mnemStr)

                # Parse semantics.
                if wantSemantics:
                    if not semStr:
                        # Parse mnemonic as semantics.
                        semStr = mnemStr
                        semLoc = mnemLoc
                    try:
                        parseSem(semStr, semLoc, semBuilder, modeType)
                    except BadInput as ex:
                        reader.error(
                            'error in semantics: %s', ex, location=ex.location
                            )
        except DelayedError:
            pass
        else:
            context = semBuilder.namespace
            yield encoding, mnemonic, semBuilder, context, flagsRequired

_reModeHeader = re.compile(r'mode\s+' + _typeTok + r'\s' + _nameTok + r'$')

def _parseMode(reader, globalBuilder, modes, wantSemantics):
    # Parse header line.
    match = _reModeHeader.match(reader.lastline)
    if not match:
        reader.error('invalid mode header, expected "mode <type> <name>"')
        reader.skipBlock()
        return
    modeTypeStr, modeName = match.groups()
    try:
        modeType = parseTypeDecl(modeTypeStr)
    except ValueError as ex:
        reader.error(
            'bad mode type: %s', ex,
            location=reader.getLocation(match.span(1))
            )
        modeType = None

    # Add mode to builder's namespace.
    mode = Mode(modeName, modeType, reader.getLocation())
    if modeName in modes:
        reader.error(
            'mode "%s" redefined; first definition was on line %d'
            % (modeName, modes[modeName].location.lineno),
            location=reader.getLocation(match.span(2))
            )
    else:
        try:
            parseType(modeName)
        except ValueError:
            modes[modeName] = mode
        else:
            reader.error(
                'mode name "%s" conflicts with type', modeName,
                location=reader.getLocation(match.span(2))
                )

    # Parse entries.
    for entry in _parseModeEntries(
            reader, globalBuilder, modes, modeType, (), _parseModeSemantics,
            wantSemantics
            ):
        mode.addEntry(*entry)

    # Determine encoding type.
    encTypes = []
    for entry in mode:
        encoding = entry[0]
        if encoding is not None:
            firstRef, firstVal, firstLoc = encoding[0]
            encTypes.append((firstRef.type, firstLoc))
    typeFreqs = defaultdict(int)
    for typ, _ in encTypes:
        typeFreqs[typ] += 1
    if len(typeFreqs) == 0:
        # Empty mode or only errors; use dummy type.
        encType = IntType.u(0)
    elif len(typeFreqs) == 1:
        # Single type.
        encType, = typeFreqs.keys()
    else:
        # Multiple types.
        encType, _ = max(typeFreqs.items(), key=lambda item: item[1])
        for typ, loc in encTypes:
            if typ is not encType:
                reader.error(
                    'encoding field has type %s, while %s is dominant in '
                    'mode "%s"', typ, encType, modeName,
                    location=loc
                    )
    mode.encodingType = encType

def _parseInstr(reader, argStr, globalBuilder, modes, wantSemantics):
    mnemBase = _parseMnemonic(argStr)

    for entry in _parseModeEntries(
            reader, globalBuilder, modes, None, mnemBase, _parseInstrSemantics,
            wantSemantics
            ):
        yield entry

def parseInstrSet(pathname, wantSemantics=True):
    globalNamespace = GlobalNamespace()
    builder = GlobalCodeBlockBuilder(globalNamespace)
    modes = {}
    instr = []

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
                instr += _parseInstr(
                    reader, argStr, builder, modes, wantSemantics
                    )
            else:
                reader.error('unknown definition type "%s"', defType)
                reader.skipBlock()
        reader.summarize()

    logger.debug('regs: %s', ', '.join(
        '%s = %r' % item for item in sorted(globalNamespace.items())
        ))

    return InstructionSet(instr) if reader.errors == 0 else None

def checkInstrSet(pathname):
    logger.info('checking: %s', pathname)
    instrSet = parseInstrSet(pathname)
