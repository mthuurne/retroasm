from .expression_parser import parseConcat, parseType
from .linereader import DefLineReader
from .register import Register

from logging import getLogger

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
        elif oldValue == value:
            self.reader.warning(
                'global name "%s" redefined; first definition was on line %d'
                % (key, self.lineno[key])
                )
        else:
            self.reader.error(
                'global name "%s" redefined with different value; '
                'first definition was on line %d'
                % (key, self.lineno[key])
                )

    def items(self):
        return self.exprs.items()

def _parseRegs(reader, args, context):
    if len(args) != 0:
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

def parseInstrSet(pathname):
    with DefLineReader.open(pathname, logger) as reader:
        context = _GlobalContextBuilder(reader)
        for header in reader:
            if not header:
                pass
            elif header[0] == '=':
                parts = header[1:].split()
                if len(parts) == 0:
                    reader.error('expected definition type after "="')
                    reader.skipBlock()
                else:
                    defType = parts[0]
                    if defType == 'reg':
                        _parseRegs(reader, parts[1 : ], context)
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
