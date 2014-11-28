from .expression import IOChannel, Register, namePat
from .expression_parser import parseConcat, parseLocalDecl, parseType
from .linereader import DefLineReader
from .func_parser import parseFuncBody

from collections import ChainMap
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
        if not isinstance(key, str):
            raise TypeError('global name should be str, got %s' % type(key))

        exprs = self.exprs
        oldValue = exprs.get(key)
        if oldValue is None:
            exprs[key] = value
            self.lineno[key] = self.reader.lineno
        else:
            self.reader.error(
                'global name "%s" redefined with different value; '
                'first definition was on line %d'
                % (key, self.lineno[key])
                )

    def items(self):
        return self.exprs.items()

def _parseRegs(reader, argStr, context):
    if argStr:
        reader.error('register definition should have no arguments')

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
                    'left hand side of register alias should be of the form '
                    '"<type> <name>"'
                    )
                continue
            try:
                aliasType = parseType(aliasTypeStr)
            except ValueError as ex:
                reader.error(str(ex))
                continue

            # Parse right hand side.
            try:
                alias = parseConcat(parts[1], context)
            except ValueError as ex:
                reader.error(str(ex))
                continue
            if alias.type is not aliasType:
                reader.error(
                    'alias has declared type %s but actual type %s'
                    % (aliasType, alias.type)
                    )
            else:
                context[aliasName] = alias
        else:
            reader.error('register definition line with multiple "="')

_nameTok = r'\s*(' + namePat + r')\s*'
_reIOLine = re.compile(_nameTok + r'\s' + _nameTok + r'\[' + _nameTok + r'\]$')

def _parseIO(reader, argStr, context):
    if argStr:
        reader.error('I/O definition should have no arguments')

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

_reFuncHeader = re.compile(_nameTok + r'\((.*)\)$')

def _parseFunc(reader, argStr, context):
    # Parse header line.
    args = []
    localContext = {}
    badHeader = False
    match = _reFuncHeader.match(argStr)
    if match:
        funcName, funcArgsStr = match.groups()
        duplicates = set()
        for funcArgStr in funcArgsStr.split(','):
            try:
                typeStr, argName = funcArgStr.split()
            except ValueError:
                reader.error(
                    'function argument "%s" not of the form "<type> <name>"',
                    funcArgStr
                    )
                badHeader = True
            else:
                if argName in localContext:
                    duplicates.add(argName)
                else:
                    try:
                        arg = parseLocalDecl(typeStr, argName)
                    except ValueError as ex:
                        reader.error(
                            'bad function argument "%s": %s', argName, ex
                            )
                        badHeader = True
                        # We still want to check for duplicates, so store
                        # a dummy value in the local context.
                        arg = None
                    localContext[argName] = arg
                    args.append(arg)
        for argName in sorted(duplicates):
            reader.error(
                'multiple arguments to function "%s" are named "%s"',
                funcName, argName
                )
            badHeader = True
    else:
        reader.error('invalid function header line')
        badHeader = True
    if badHeader:
        # Avoid issuing meaningless errors.
        reader.skipBlock()
        return

    # Parse body lines.
    combinedContext = ChainMap(localContext, context.exprs)
    body = parseFuncBody(reader, reader.iterBlock(), combinedContext)

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
