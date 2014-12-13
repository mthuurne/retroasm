from .expression import IOChannel, Register, namePat
from .expression_parser import parseConcat, parseExpr, parseLocalDecl, parseType
from .linereader import DefLineReader
from .func_parser import createFunc

from collections import ChainMap, OrderedDict
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
            raise TypeError('global name must be str, got %s' % type(key))

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
    '''Parses a function header, returning a pair consisting of the function
    name and an OrderedDict of its arguments.
    When errors are encountered, they are logged and ValueError is raised.
    '''
    ok = True
    def error(msg, *args):
        nonlocal ok
        ok = False
        log.error(msg, *args)

    args = OrderedDict()
    duplicates = set()
    for argStr in argsStr.split(','):
        try:
            typeStr, argName = argStr.split()
        except ValueError:
            error(
                'function argument "%s" not of the form "<type> <name>"',
                argStr
                )
        else:
            if argName in args:
                duplicates.add(argName)
            else:
                try:
                    arg = parseLocalDecl(typeStr, argName)
                except ValueError as ex:
                    error('bad function argument "%s": %s', argName, ex)
                    # We still want to check for duplicates, so store
                    # a dummy value in the local context.
                    arg = None
                args[argName] = arg
    for argName in sorted(duplicates):
        error('multiple arguments are named "%s"', argName)

    if ok:
        return args
    else:
        raise ValueError('error parsing function header; see log for details')

def _parseAssignments(log, lines, context):
    '''Parses the given lines as a series of assignments, yields the
    assignments as pairs of expressions.
    The full sequence of lines is parsed, even in the presence of errors.
    Errors are appended to the given log as they are discovered.
    If there were any errors ValueError is raised at the end.
    '''
    ok = True
    def error(msg, *args):
        nonlocal ok
        ok = False
        log.error(msg, *args)

    for line in lines:
        parts = line.split(':=')
        if len(parts) < 2:
            error('no assignment in line')
        elif len(parts) > 2:
            error('multiple assignments in a single line')
        else:
            lhsStr, rhsStr = parts
            try:
                lhs = parseExpr(lhsStr, context)
            except ValueError as ex:
                error('error in left hand side of assignment: %s', str(ex))
                continue
            try:
                rhs = parseExpr(rhsStr, context)
            except ValueError as ex:
                error('error in right hand side of assignment: %s', str(ex))
                continue
            yield lhs, rhs

    if not ok:
        raise ValueError('error parsing function body; see log for details')

_reFuncHeader = re.compile(_nameTok + r'\((.*)\)$')

def _parseFunc(reader, argStr, context):
    # Parse header line.
    match = _reFuncHeader.match(argStr)
    if not match:
        reader.error('invalid function header line')
        reader.skipBlock()
        return
    funcName, funcArgsStr = match.groups()

    # Parse arguments.
    try:
        args = _parseFuncArgs(reader, funcArgsStr)
    except ValueError:
        reader.skipBlock()
        return

    # Parse body lines.
    localContext = dict(args)
    combinedContext = ChainMap(localContext, context.exprs)
    assignments = _parseAssignments(reader, reader.iterBlock(), combinedContext)

    body = createFunc(reader, assignments)
    if body is not None:
        print()
        print('func %s(%s)' % (
            funcName,
            ', '.join(arg.formatDecl() for arg in args.values())
            ))
        for node in body:
            print('\t', node)

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
