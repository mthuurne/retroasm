from .expression_parser import parseType
from .linereader import DefLineReader
from .register import Register

from logging import getLogger

logger = getLogger('parse-instr')

class _GlobalContextBuilder:

    def __init__(self, reader):
        self.reader = reader
        self.exprs = {}
        self.lineno = {}

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

    regs = [reg for _, reg in sorted(context.items())]
    logger.debug('regs: %s',
        ', '.join('%s %s' % (reg.type, reg.name) for reg in regs))

    return None

def checkInstrSet(pathname):
    logger.info('checking: %s', pathname)
    instrSet = parseInstrSet(pathname)
