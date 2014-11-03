from .expression_parser import parseType
from .linereader import DefLineReader
from .register import Register

from logging import getLogger

logger = getLogger('parse-instr')

def _parseRegs(reader, args, parsedRegs):
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
            if regName in parsedRegs:
                oldReg, oldLineno = parsedRegs[regName]
                if oldReg.type is regType:
                    reader.warning(
                        'register "%s" redefined; '
                        'first definition was on line %d'
                        % (regName, oldLineno)
                        )
                else:
                    reader.error(
                        'register "%s" redefined with different type; '
                        'first definition was on line %d'
                        % (regName, oldLineno)
                        )
            else:
                try:
                    reg = Register(regName, regType)
                except ValueError as ex:
                    reader.error(str(ex))
                else:
                    parsedRegs[regName] = (reg, reader.lineno)

def parseInstrSet(pathname):
    parsedRegs = {}
    with DefLineReader.open(pathname, logger) as reader:
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
                        _parseRegs(reader, parts[1 : ], parsedRegs)
                    else:
                        reader.error('unknown definition type "%s"', defType)
                        reader.skipBlock()
            else:
                reader.error('expected definition block (starting with "=")')
                reader.skipBlock()
        reader.summarize()

    regs = dict((name, str(typ)) for name, (typ, lineno) in parsedRegs.items())
    logger.debug('reg: %s', regs)

    return None

def checkInstrSet(pathname):
    logger.info('checking: %s', pathname)
    instrSet = parseInstrSet(pathname)
