from .codeblock import (
    ArgumentConstant, ComputedConstant, ConcatenatedReference, ConstantValue,
    FixedValue, LoadedConstant, SlicedReference, Store
    )
from .codeblock_builder import (
    EncodingCodeBlockBuilder, GlobalCodeBlockBuilder, LocalCodeBlockBuilder
    )
from .context_parser import MatchPlaceholderSpec, ValuePlaceholderSpec
from .expression import IntLiteral
from .expression_builder import (
    UnknownNameError, buildExpression, buildReference, convertDefinition
    )
from .expression_parser import (
    AssignmentNode, BranchNode, DeclarationNode, DefinitionNode, EmptyNode,
    FlagTestNode, IdentifierNode, LabelNode, MultiMatchNode, NumberNode,
    parseContext, parseExpr, parseExprList, parseInt, parseStatement
    )
from .function_builder import createFunc
from .instrset import InstructionSet
from .linereader import BadInput, DefLineReader, DelayedError, mergeSpan
from .mode import (
    EncodingExpr, EncodingMultiMatch, Immediate, MatchPlaceholder, Mode,
    ModeEntry, PlaceholderRole, ValuePlaceholder
    )
from .namespace import GlobalNamespace, NameExistsError
from .storage import IOChannel, Variable, namePat
from .types import (
    IntType, ReferenceType, maskForWidth, parseType, parseTypeDecl, unlimited
    )
from collections import OrderedDict, defaultdict
from logging import WARNING, getLogger
import re

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

def _buildPlaceholder(placeholder, typ, builder):
    decl = placeholder.decl
    name = decl.name.name
    value = placeholder.value
    if value is not None:
        convertDefinition(decl.kind, decl.name, typ, value, builder)
    elif isinstance(typ, ReferenceType):
        builder.emitReferenceArgument(name, typ.type, decl.name.location)
    else:
        immediate = Immediate(name, typ)
        ref = builder.emitFixedValue(immediate, typ)
        builder.defineReference(name, ref, decl.name.location)

def _parseModeEncoding(encNodes, encBuilder, placeholderSpecs, reader):
    # Define placeholders in encoding builder.
    # Errors are stored rather than reported immediately, since it is possible
    # to define expressions that are valid as semantics but not as encodings.
    # This is not a problem as long as the associated name is not used in the
    # encoding field. For example relative addressing reads the base address
    # from a register.
    encErrors = {}
    for name, placeholder in placeholderSpecs.items():
        encWidth = placeholder.encodingWidth
        if encWidth is not None:
            encType = IntType.u(encWidth)
            try:
                _buildPlaceholder(placeholder, encType, encBuilder)
            except BadInput as ex:
                encErrors[name] = ex

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
    auxWidth = None
    for encIdx, encNode in enumerate(encNodes):
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
            if mode.encodingWidth is None:
                reader.warning(
                    'mode "%s" does not contain encoding elements',
                    mode.name, location=(encLoc, placeholder.decl.treeLocation)
                    )
                continue
            if start >= 1 and mode.auxEncodingWidth is None:
                reader.warning(
                    'mode "%s" does not contain auxiliary encoding elements',
                    mode.name, location=(encLoc, placeholder.decl.treeLocation)
                    )
                continue

            yield EncodingMultiMatch(name, mode, start, encLoc)
        else:
            # Expression possibly containing single encoding field matches.
            try:
                encRef = buildReference(encNode, encBuilder)
            except BadInput as ex:
                if isinstance(ex, UnknownNameError):
                    placeholder = placeholderSpecs.get(ex.name)
                    if placeholder is not None \
                            and placeholder.encodingWidth is None:
                        reader.error(
                            'cannot use placeholder "%s" in encoding field, '
                            'since mode "%s" has an empty encoding sequence',
                            ex.name, placeholder.mode.name, location=(
                                ex.location,
                                placeholder.decl.treeLocation
                                )
                            )
                        continue
                    ex = encErrors.get(ex.name, ex)
                reader.error(
                    'error in encoding: %s', ex,
                    location=(encLoc, ex.location)
                    )
                continue

            try:
                encValue = encRef.emitLoad(encLoc)
            except BadInput as ex:
                reader.error(
                    'error evaluating encoding: %s', ex,
                    location=(encLoc, ex.location)
                    )
                continue

            encWidth = encRef.width
            if encWidth is unlimited:
                reader.error(
                    'unlimited width integers are not allowed in encoding',
                    location=encLoc
                    )
            elif encIdx > 0:
                if auxWidth is None:
                    auxWidth = encWidth
                elif encWidth != auxWidth:
                    reader.error(
                        'auxiliary encoding item has width %d, while first '
                        'auxiliary item has width %d',
                        encWidth, auxWidth,
                        location=(encLoc, encNodes[1].treeLocation)
                        )

            yield EncodingExpr(encRef, encValue, encLoc)

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
    for name, placeholder in placeholderSpecs.items():
        if isinstance(placeholder, ValuePlaceholderSpec):
            if placeholder.value is not None:
                # The value is computed, so we don't need to encode it.
                continue
            if name not in identifiers:
                reader.error(
                    'value placeholder "%s" does not occur in encoding', name,
                    location=(encFullSpan(), placeholder.decl.treeLocation)
                    )
        elif isinstance(placeholder, MatchPlaceholderSpec):
            mode = placeholder.mode
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
                    location=(encFullSpan(), placeholder.decl.treeLocation)
                    )
            if mode.auxEncodingWidth is not None:
                reader.error(
                    'mode "%s" contains auxiliary encoding items, but '
                    'there is no "%s@" placeholder to match them',
                    mode.name, name,
                    location=(encFullSpan(), placeholder.decl.treeLocation)
                    )
        else:
            assert False, placeholder

def _decomposeReference(ref):
    if isinstance(ref, FixedValue):
        const = ref.const
        assert isinstance(const, ComputedConstant), const
        yield const.expr, 0, 0, ref.width
    elif isinstance(ref, ConcatenatedReference):
        offset = 0
        for subRef in ref:
            for expr, immIdx, refIdx, width in _decomposeReference(subRef):
                yield expr, immIdx, offset + refIdx, width
            offset += subRef.width
    elif isinstance(ref, SlicedReference):
        # Note that SlicedReference has already simplified the offset.
        offset = ref.offset
        if isinstance(offset, IntLiteral):
            start = offset.value
            end = start + ref.width
            for expr, immIdx, refIdx, width in _decomposeReference(ref.ref):
                # Clip to slice boundaries.
                refStart = max(refIdx, start)
                refEnd = min(refIdx + width, end)
                # Output if clipped slice is not empty.
                width = refEnd - refStart
                if width > 0:
                    immShift = refStart - refIdx
                    yield expr, immIdx + immShift, refStart - start, width
        else:
            raise ValueError('slices in encoding must have fixed offset')
    else:
        # Note: SingleReference cannot occur in encoding since the stateless
        #       builder would trigger an error when loading from it.
        assert False, ref

def _decomposeEncodingExprs(encElems, reader):
    fixedMatcher = []
    decodeMap = defaultdict(list)
    for encIdx, encElem in enumerate(encElems):
        if not isinstance(encElem, EncodingExpr):
            continue
        fixedMask = 0
        fixedValue = 0
        try:
            for expr, immIdx, refIdx, width in _decomposeReference(encElem.ref):
                if isinstance(expr, Immediate):
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

def _parseModeDecoding(encoding, encBuilder, placeholderSpecs, reader):
    '''Construct a mapping that, given an encoded instruction, produces the
    values for context placeholders.
    '''

    # Decompose the encoding expressions.
    try:
        with reader.checkErrors():
            fixedMatcher, decodeMap = _decomposeEncodingExprs(encoding, reader)
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
                        location=placeholderSpecs[name].decl.treeLocation
                        )
                else:
                    sequentialMap[name] = decoding
    except DelayedError:
        return None
    else:
        sequentialMap[None] = fixedMatcher
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

def _inlineConstants(expr, constants):
    '''Inline all ConstantValues in the given expression.
    Constant IDs are looked up in the given constants collection.
    '''
    def subst(expr):
        if isinstance(expr, ConstantValue):
            const = constants[expr.cid]
            if isinstance(const, ComputedConstant):
                return const.expr.substitute(subst)
        return None
    return expr.substitute(subst)

def _replaceProgramCounter(expr, semBuilder):
    '''Replaces values loaded from the program counter by an Immediate.
    '''
    def subst(expr):
        if isinstance(expr, ConstantValue):
            const = semBuilder.constants[expr.cid]
            assert isinstance(const, LoadedConstant), const
            storage = semBuilder.storages[const.sid]
            if isinstance(storage, Variable) and storage.name == 'pc':
                return Immediate('pc', semBuilder.namespace['pc'].type)
        return None
    return expr.substitute(subst)

def _determinePlaceholderRoles(semantics, placeholders):
    '''Analyze semantics to figure out the roles of placeholders.
    While this won't be sufficient to determine the role of all literal values,
    it can handle a few common cases reliably and efficiently.
    '''

    # Find storage ID for program counter, if any.
    pcSids = tuple(
        sid
        for sid, storage in semantics.storages.items()
        if isinstance(storage, Variable) and storage.name == 'pc'
        )
    if len(pcSids) != 0:
        pcSid, = pcSids
        # Mark placeholders written to the program counter as code addresses.
        for node in semantics.nodes:
            if isinstance(node, Store) and node.sid == pcSid:
                const = semantics.constants[node.cid]
                if isinstance(const, ArgumentConstant):
                    placeholder = placeholders[const.name]
                    placeholder.addRole(PlaceholderRole.code_addr)

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
                # Parse context.
                if ctxStr:
                    try:
                        with reader.checkErrors():
                            placeholderSpecs, flagsRequired = _parseModeContext(
                                ctxStr, ctxLoc, modes, reader
                                )
                    except DelayedError:
                        # To avoid error spam, skip this line.
                        continue
                else:
                    placeholderSpecs, flagsRequired = {}, set()

                # Define placeholders in context builder.
                ctxBuilder = LocalCodeBlockBuilder(globalBuilder)
                try:
                    for spec in placeholderSpecs.values():
                        semType = spec.semanticsType
                        _buildPlaceholder(spec, semType, ctxBuilder)
                except BadInput as ex:
                    reader.error(
                        'error in context: %s', ex, location=ex.location
                        )
                    # To avoid error spam, skip this line.
                    continue

                # Parse encoding.
                encBuilder = EncodingCodeBlockBuilder(globalBuilder)
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
                                encNodes, encBuilder, placeholderSpecs, reader
                                ))
                    except DelayedError:
                        encoding = None
                if encoding is None:
                    decoding = None
                else:
                    decoding = _parseModeDecoding(
                        encoding, encBuilder, placeholderSpecs, reader
                        )

                # Convert placeholders.
                placeholders = {}
                for name, spec in placeholderSpecs.items():
                    if isinstance(spec, ValuePlaceholderSpec):
                        if spec.value is None:
                            value = None
                        else:
                            ref = ctxBuilder.namespace[name]
                            # TODO: While the documentation says we do support
                            #       defining references in the context, the
                            #       parse code rejects "<type>&".
                            assert isinstance(ref, FixedValue), ref
                            expr = ref.const.expr
                            value = _replaceProgramCounter(
                                _inlineConstants(expr, ctxBuilder.constants),
                                ctxBuilder
                                )
                        placeholder = ValuePlaceholder(
                            name, spec.semanticsType, value
                            )
                    elif isinstance(spec, MatchPlaceholderSpec):
                        placeholder = MatchPlaceholder(name, spec.mode)
                    else:
                        assert False, spec
                    placeholders[name] = placeholder

                # Parse mnemonic.
                mnemonic = mnemBase + tuple(_parseMnemonic(
                    mnemStr, mnemLoc, placeholders, reader
                    ))
                if len(mnemonic) == 0:
                    reader.error('missing mnemonic', location=mnemLoc)

                # Parse semantics.
                if wantSemantics:
                    semBuilder = LocalCodeBlockBuilder(globalBuilder)
                    try:
                        # Define placeholders in semantics builder.
                        for name, spec in placeholderSpecs.items():
                            location = spec.decl.name.location
                            semType = spec.semanticsType
                            if isinstance(semType, ReferenceType):
                                semBuilder.emitReferenceArgument(
                                    name, semType.type, location
                                    )
                            else:
                                semBuilder.emitValueArgument(
                                    name, semType, location
                                    )

                        if not semStr:
                            # Parse mnemonic field as semantics.
                            semStr = mnemStr
                            semLoc = mnemLoc

                        parseSem(semStr, semLoc, semBuilder, modeType)
                    except BadInput as ex:
                        reader.error(
                            'error in semantics: %s', ex, location=ex.location
                            )
                        # This is the last field.
                        continue
                    try:
                        semCode = semBuilder.createCodeBlock(reader)

                        _determinePlaceholderRoles(semCode, placeholders)

                        # TODO: Inline semCode into ctxBuilder as a function
                        #       call, where all placeholders are arguments.
                        semantics = semBuilder.createCodeBlock(reader)
                    except ValueError:
                        # Error was already logged inside createCodeBlock().
                        pass
                else:
                    semantics = None
        except DelayedError:
            pass
        else:
            yield ModeEntry(
                encoding, decoding, mnemonic, semantics, placeholders,
                flagsRequired, reader.getLocation()
                )

def _formatEncodingWidth(width):
    return 'empty' if width is None else '%d bits wide' % width

def _determineEncodingWidth(entries, aux, modeName, logger):
    '''Returns the common encoding width for the given list of mode entries.
    Entries with a deviating encoding width will be logged as errors on the
    given logger and removed from the entries list.
    If the 'aux' argument is False, the first encoding element of each entry
    is checked, otherwise the auxiliary encoding elements are checked.
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
                    '%sencoding field is %s, while %s is dominant %s',
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
    encWidth = _determineEncodingWidth(entries, False, modeName, reader)
    auxEncWidth = _determineEncodingWidth(entries, True, modeName, reader)
    mode = Mode(modeName, encWidth, auxEncWidth, semType, modeLocation, entries)
    if addMode:
        modes[modeName] = mode

def _parseInstr(reader, argStr, globalBuilder, modes, wantSemantics):
    mnemBase = tuple(_parseMnemonic(argStr, None, {}, reader))

    for instr in _parseModeEntries(
            reader, globalBuilder, modes, None, mnemBase, _parseInstrSemantics,
            wantSemantics
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
                'auxiliary instruction encoding elements are %d bits wide, '
                'while first item is %d bits wide', auxEncodingWidth, encWidth,
                location=instr.auxEncodingLocation
                )
        yield instr

def parseInstrSet(pathname, logger=None, wantSemantics=True):
    if logger is None:
        logger = getLogger('parse-instr')
        logger.setLevel(WARNING)

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

        encWidth = _determineEncodingWidth(instructions, False, None, reader)

        reader.summarize()

    logger.debug('regs: %s', ', '.join(
        '%s = %r' % item for item in sorted(globalNamespace.items())
        ))

    if reader.errors == 0:
        anyAux = any(len(instr.encoding) >= 2 for instr in instructions)
        auxEncWidth = encWidth if anyAux else None
        return InstructionSet(encWidth, auxEncWidth, instructions)
    else:
        return None

def checkInstrSet(pathname, logger):
    logger.info('checking: %s', pathname)
    instrSet = parseInstrSet(pathname, logger)
