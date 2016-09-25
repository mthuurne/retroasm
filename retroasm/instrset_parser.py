from .codeblock_builder import GlobalCodeBlockBuilder
from .context import NameExistsError
from .expression_builder import buildStorage
from .expression_parser import IdentifierNode, parseExpr, parseExprList
from .function_builder import createFunc
from .linereader import BadInput, DefLineReader, DelayedError
from .mode import Immediate
from .storage import IOChannel, Register, namePat
from .types import parseType, parseTypeDecl

from collections import OrderedDict
from logging import getLogger
import re

logger = getLogger('parse-instr')

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
                    reg = Register(regName, regType)
                except ValueError as ex:
                    reader.error(str(ex))
                    continue
                try:
                    builder.emitRegister(reg, reader.getLocation())
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
                alias = buildStorage(tree, builder)
            except BadInput as ex:
                reader.error(str(ex), location=ex.location)
                continue
            if alias.width != aliasType.width:
                reader.error(
                    'alias is declared as %d bits wide but its definition is '
                    '%d bits wide', aliasType.width, alias.width
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

_nameTok = r'\s*(' + namePat + r')\s*'
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
                addrType = parseType(addrTypeStr)
            except ValueError as ex:
                reader.error(str(ex))
                continue
            channel = IOChannel(name, elemType.width, addrType.width)
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

_typeTok = r'\s*(' + namePat + r'&?)\s*'
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

_reModeHeader = re.compile(_nameTok + r'$')
_reCommaSep = re.compile(r'\s*(?:\,\s*|$)')
_reDotSep = re.compile(r'\s*(?:\.\s*|$)')

def _parseMode(reader, argStr, globalBuilder, modes):
    # Parse header line.
    match = _reModeHeader.match(argStr)
    if not match:
        reader.error('invalid mode header')
        reader.skipBlock()
        return
    modeName, = match.groups()
    mode = modes.get(modeName)
    if mode is None:
        try:
            parseType(modeName)
        except ValueError as ex:
            pass
        else:
            reader.warning('mode name "%s" is also valid as a type' % modeName)
        mode = []
        modes[modeName] = mode

    def checkIdentifiers(exprTree, knownNames):
        for node in exprTree:
            if isinstance(node, IdentifierNode):
                name = node.name
                if name not in knownNames:
                    reader.error(
                        'unknown identifier "%s"' % name,
                        location=node.location
                        )

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
                # Parse context.
                knownNames = set()
                immediates = {}
                includedModes = {}
                if ctxStr:
                    for ctxElem, ctxElemLoc in reader.splitOn(
                            _reCommaSep.finditer(line, *ctxLoc.span)):
                        try:
                            ctxType, name = ctxElem.split()
                        except ValueError:
                            reader.error(
                                'context element not of the form '
                                '"<mode> <name>"',
                                location=ctxElemLoc
                                )
                            continue
                        if name in knownNames:
                            reader.error(
                                'duplicate placeholder ("%s")' % name,
                                location=ctxElemLoc
                                )
                            continue
                        else:
                            knownNames.add(name)

                        includedMode = modes.get(ctxType)
                        if includedMode is not None:
                            includedModes[name] = includedMode
                        else:
                            try:
                                typ = parseType(ctxType)
                            except ValueError:
                                reader.error(
                                    'there is no type or mode '
                                    'named "%s"' % ctxType,
                                    location=ctxElemLoc
                                    )
                                continue
                            else:
                                immediates[name] = Immediate(
                                    name, typ, ctxElemLoc
                                    )

                knownNames |= globalBuilder.context.keys()

                # Parse encoding.
                try:
                    encoding = parseExprList(encStr, encLoc)
                    for encElem in encoding:
                        checkIdentifiers(encElem, knownNames)
                except BadInput as ex:
                    reader.error(
                        'error in encoding: %s' % ex, location=ex.location
                        )

                # Parse mnemonic.
                # TODO: We have no infrastructure for mnemonics yet.
                mnemonic = (mnemStr, mnemLoc)

                # Parse semantics.
                if not semStr:
                    # Parse mnemonic as semantics.
                    semStr = mnemStr
                    semLoc = mnemLoc
                try:
                    semantics = parseExpr(semStr, semLoc)
                    checkIdentifiers(semantics, knownNames)
                except BadInput as ex:
                    reader.error(
                        'error in semantics: %s' % ex, location=ex.location
                        )
        except DelayedError:
            pass
        else:
            mode.append(
                (encoding, mnemonic, semantics, immediates, includedModes)
                )

def parseInstrSet(pathname):
    with DefLineReader.open(pathname, logger) as reader:
        builder = GlobalCodeBlockBuilder()
        modes = {}
        for header in reader:
            if not header:
                pass
            elif header[0] == '=':
                parts = header[1:].split(maxsplit=1)
                if len(parts) == 0:
                    reader.error('expected definition type after "="')
                    reader.skipBlock()
                else:
                    defType = parts[0]
                    argStr = '' if len(parts) == 1 else parts[1]
                    if defType == 'reg':
                        _parseRegs(reader, argStr, builder)
                    elif defType == 'io':
                        _parseIO(reader, argStr, builder.context)
                    elif defType == 'func':
                        _parseFunc(reader, argStr, builder)
                    elif defType == 'mode':
                        _parseMode(reader, argStr, builder, modes)
                    else:
                        reader.error('unknown definition type "%s"', defType)
                        reader.skipBlock()
            else:
                reader.error('expected definition block (starting with "=")')
                reader.skipBlock()
        reader.summarize()

    logger.debug('regs: %s', ', '.join(
        '%s = %s' % (key, repr(value))
        for key, value in sorted(builder.context.items())
        ))

    return None

def checkInstrSet(pathname):
    logger.info('checking: %s', pathname)
    instrSet = parseInstrSet(pathname)
