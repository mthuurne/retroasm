from .codeblock_builder import GlobalCodeBlockBuilder, LocalCodeBlockBuilder
from .context import GlobalContext, NameExistsError
from .expression_builder import buildExpression, buildReference
from .expression_parser import (
    AssignmentNode, BranchNode, DeclarationNode, DefinitionNode, EmptyNode,
    FlagTestNode, IdentifierNode, LabelNode, NumberNode, parseContext,
    parseExpr, parseExprList, parseStatement
    )
from .function_builder import createFunc
from .linereader import BadInput, DefLineReader, DelayedError
from .mode import Immediate, Mode
from .storage import IOChannel, Variable, namePat
from .types import ReferenceType, parseType, parseTypeDecl

from collections import OrderedDict
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

def _parseIO(reader, argStr, context):
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
                context.define(name, channel, reader.getLocation())
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
                    # a dummy value in the local context.
                    arg = None
                args[argName] = arg
    return args

_reFuncHeader = re.compile(
    r'(?:' + _typeTok + r'\s)?' + _nameTok + r'\((.*)\)$'
    )

def _parseFunc(reader, argStr, builder):
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

    # Parse body lines.
    func = createFunc(reader, funcName, retType, args, builder)

    # Store function in global context.
    try:
        builder.context.define(funcName, func, headerLocation)
    except NameExistsError as ex:
        reader.error('error declaring function: %s', ex, location=ex.location)

    func.dump()
    print()

def _parseModeContext(ctxStr, ctxLoc, ctxBuilder, modes, reader):
    flagsRequired = set()
    for node in parseContext(ctxStr, ctxLoc):
        if isinstance(node, (DeclarationNode, DefinitionNode)):
            decl = node if isinstance(node, DeclarationNode) else node.decl
            name = decl.name.name
            nameLoc = decl.name.location
            typeName = decl.type.name

            mode = modes.get(typeName)
            if mode is not None:
                typ = mode.type
            else:
                try:
                    typ = parseType(typeName)
                except ValueError:
                    reader.error(
                        'there is no type or mode named "%s"' % typeName,
                        location=decl.type.location
                        )
                    continue
            if isinstance(typ, ReferenceType):
                ctxBuilder.emitReferenceArgument(name, typ.type, nameLoc)
            else:
                # TODO: Should a context element that does
                #       not occur in the encoding still be
                #       considered an immediate?
                immediate = Immediate(name, typ, decl.treeLocation)
                value = ctxBuilder.emitFixedValue(immediate, typ)
                ctxBuilder.defineReference(name, value, nameLoc)

            if isinstance(node, DefinitionNode):
                try:
                    expr = buildReference(node.value, ctxBuilder)
                except BadInput as ex:
                    reader.error(
                        'error in context: %s' % ex,
                        location=ex.location
                        )
        elif isinstance(node, FlagTestNode):
            flagsRequired.add(node.name)
        else:
            assert False, node

    return flagsRequired

def _parseModeSemantics(semStr, semLoc, ctxBuilder, modeType):
    semantics = parseExpr(semStr, semLoc)
    if isinstance(modeType, ReferenceType):
        ref = buildReference(semantics, ctxBuilder)
        if modeType is not None:
            if ref.type != modeType.type:
                raise BadInput(
                    'semantics type %s does not match mode type %s'
                    % (ref.type, modeType.type),
                    location=semLoc
                    )
        ctxBuilder.defineReference('ret', ref, semLoc)
    else:
        expr = buildExpression(semantics, ctxBuilder)
        # Note that modeType can be None because of earlier errors.
        if modeType is not None:
            retRef = ctxBuilder.emitVariable('ret', modeType, semLoc)
            retRef.emitStore(expr, semLoc)

def _rejectNodeClasses(node, badClasses):
    if isinstance(node, badClasses):
        raise BadInput(
            '%s is not allowed here' % node.__class__.__name__[:-4].lower(),
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

_reDotSep = re.compile(r'\s*(?:\.\s*|$)')

def _parseModeEntries(reader, globalBuilder, modes, modeType, parseSem):
    for line in reader.iterBlock():
        # Split mode line into 4 fields.
        fields = list(reader.splitOn(_reDotSep.finditer(line)))
        if len(fields) < 2:
            reader.error('field separator "." missing in mode line')
            continue
        if len(fields) > 4:
            reader.error('too many fields (%d) in mode line' % len(fields))
            continue
        fields += [('', None)] * (4 - len(fields))
        (encStr, encLoc), (mnemStr, mnemLoc), (semStr, semLoc), \
                (ctxStr, ctxLoc) = fields

        try:
            with reader.checkErrors():
                ctxBuilder = LocalCodeBlockBuilder(globalBuilder)

                # Parse context.
                if ctxStr:
                    try:
                        flagsRequired = _parseModeContext(
                            ctxStr, ctxLoc, ctxBuilder, modes, reader
                            )
                    except BadInput as ex:
                        reader.error(
                            'error in context: %s' % ex, location=ex.location
                            )
                        # To avoid error spam, skip this line.
                        continue
                else:
                    flagsRequired = set()

                # Parse encoding.
                if encStr:
                    try:
                        encoding = parseExprList(encStr, encLoc)
                        for encElem in encoding:
                            # TODO: Encoding is not allowed to add anything.
                            #       Function call inlining could alter state.
                            buildExpression(encElem, ctxBuilder)
                    except BadInput as ex:
                        reader.error(
                            'error in encoding: %s' % ex, location=ex.location
                            )
                else:
                    encoding = NumberNode(0, 0, encLoc)

                # Parse mnemonic.
                # TODO: We have no infrastructure for mnemonics yet.
                mnemonic = (mnemStr, mnemLoc)

                # Parse semantics.
                if not semStr:
                    # Parse mnemonic as semantics.
                    semStr = mnemStr
                    semLoc = mnemLoc
                try:
                    parseSem(semStr, semLoc, ctxBuilder, modeType)
                except BadInput as ex:
                    reader.error(
                        'error in semantics: %s' % ex, location=ex.location
                        )
        except DelayedError:
            pass
        else:
            context = ctxBuilder.context
            yield encoding, mnemonic, ctxBuilder, context, flagsRequired

_reModeHeader = re.compile(r'mode\s+' + _typeTok + r'\s' + _nameTok + r'$')

def _parseMode(reader, globalBuilder, modes):
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
            'bad mode type: %s' % ex,
            location=reader.getLocation(match.span(1))
            )
        modeType = None

    mode = modes.get(modeName)
    if mode is None:
        mode = Mode(modeName, modeType, reader.getLocation())
        try:
            parseType(modeName)
        except ValueError:
            modes[modeName] = mode
        else:
            reader.error(
                'mode name "%s" conflicts with type' % modeName,
                location=reader.getLocation(match.span(2))
                )

    for entry in _parseModeEntries(
            reader, globalBuilder, modes, modeType, _parseModeSemantics
            ):
        mode.addEntry(*entry)

def _parseInstr(reader, argStr, globalBuilder, modes):
    mnemBase = argStr

    for entry in _parseModeEntries(
            reader, globalBuilder, modes, None, _parseInstrSemantics
            ):
        pass

def parseInstrSet(pathname):
    globalContext = GlobalContext()
    builder = GlobalCodeBlockBuilder(globalContext)
    modes = {}

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
                _parseIO(reader, argStr, globalContext)
            elif defType == 'func':
                _parseFunc(reader, argStr, builder)
            elif defType == 'mode':
                _parseMode(reader, builder, modes)
            elif defType == 'instr':
                _parseInstr(reader, argStr, builder, modes)
            else:
                reader.error('unknown definition type "%s"', defType)
                reader.skipBlock()
        reader.summarize()

    logger.debug('regs: %s', ', '.join(
        '%s = %r' % item for item in sorted(globalContext.items())
        ))

    return None

def checkInstrSet(pathname):
    logger.info('checking: %s', pathname)
    instrSet = parseInstrSet(pathname)
