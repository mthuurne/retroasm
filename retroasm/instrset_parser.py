from .codeblock_builder import CodeBlockBuilder
from .expression_builder import BadExpression, buildStorage
from .expression_parser import ParseError, parseExpr
from .function_builder import createFunc
from .linereader import DefLineReader, DelayedError
from .storage import (
    Concatenation, IOChannel, ReferencedValue, Register, namePat
    )
from .types import parseType, parseTypeDecl
from .utils import checkType

from collections import OrderedDict
from functools import partial
from logging import getLogger
import re

logger = getLogger('parse-instr')

class _GlobalContextBuilder:

    def __init__(self, reader):
        self.reader = reader
        self.exprs = {}
        self.lineno = {}

    def __getitem__(self, key):
        return self.exprs[key]

    def __setitem__(self, key, value):
        deliver = self.reserve(key)
        deliver(value)

    def reserve(self, key):
        '''Reserves a name in the global context.
        If the name is already taken, an error is logged and None is returned.
        If the name was available, it is reserved and a function is returned
        that, when called with a single argument, will store that argument in
        the global context as the value for the reserved key.
        '''
        checkType(key, str, 'global name')
        oldLineno = self.lineno.get(key)
        if oldLineno is None:
            self.lineno[key] = self.reader.lineno
            return partial(self.exprs.__setitem__, key)
        else:
            self.reader.error(
                'global name "%s" redefined; first definition was on line %d'
                % (key, oldLineno)
                )

    def items(self):
        return self.exprs.items()

def _parseRegs(reader, argStr, context):
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
                else:
                    context[regName] = reg
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
            builder = CodeBlockBuilder(context, reader)
            try:
                tree = parseExpr(parts[1], reader.getLocation())
                alias = buildStorage(tree, builder)
            except (ParseError, BadExpression) as ex:
                reader.error(str(ex))
                continue
            if alias.type is not aliasType:
                reader.error(
                    'alias has declared type %s but actual type %s'
                    % (aliasType, alias.type)
                    )
            elif builder.constants:
                # TODO: Handle this better.
                reader.error('alias produces constants')
            elif builder.nodes:
                # TODO: Handle this better.
                reader.error('alias produces nodes')
            else:
                def unwrap(storage):
                    # pylint: disable=cell-var-from-loop
                    if isinstance(storage, ReferencedValue):
                        return builder.references[storage.rid]
                    else:
                        return storage
                if isinstance(alias, Concatenation):
                    unwrapped = Concatenation(
                        unwrap(expr) for expr in alias.exprs
                        )
                else:
                    unwrapped = unwrap(alias)
                context[aliasName] = unwrapped
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
            channel = IOChannel(name, elemType, addrType)
            context[name] = channel
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
    r'(?:' + _nameTok + r'\s)?' + _nameTok + r'\((.*)\)$'
    )

def _parseFunc(reader, argStr, context):
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
            retType = parseType(retTypeStr)
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

    # Reserve the function name in the global context.
    nameReservation = context.reserve(funcName)

    # Parse body lines.
    func = createFunc(reader, funcName, retType, args, context.exprs)

    # Store function in global context.
    if nameReservation is not None:
        nameReservation(func)

    func.dump()
    print()

def parseInstrSet(pathname):
    with DefLineReader.open(pathname, logger) as reader:
        context = _GlobalContextBuilder(reader)
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
                        _parseRegs(reader, argStr, context)
                    elif defType == 'io':
                        _parseIO(reader, argStr, context)
                    elif defType == 'func':
                        _parseFunc(reader, argStr, context)
                    else:
                        reader.error('unknown definition type "%s"', defType)
                        reader.skipBlock()
            else:
                reader.error('expected definition block (starting with "=")')
                reader.skipBlock()
        reader.summarize()

    logger.debug('regs: %s', ', '.join(
        '%s = %s' % (key, repr(value))
        for key, value in sorted(context.items())
        ))

    return None

def checkInstrSet(pathname):
    logger.info('checking: %s', pathname)
    instrSet = parseInstrSet(pathname)
