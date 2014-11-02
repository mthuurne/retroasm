from .linereader import DefLineReader

from logging import getLogger
import re

logger = getLogger('parse-instr')

reRegName = re.compile('[a-z][a-z0-9]*\'?$')
reTypeName = re.compile('i[0-9]+$')

def _parseRegs(reader, args, parsedRegs):
    reader.debug('parsing register definitions')

    if len(args) != 0:
        reader.error('register definition should have no arguments')

    for line in reader.iterBlock():
        parts = line.split()

        typeName = parts[0]
        if not reTypeName.match(typeName):
            reader.error('invalid type name: "%s"', typeName)
            continue

        for regName in parts[1:]:
            if not reRegName.match(regName):
                reader.error('invalid register name: "%s"', regName)
            elif regName in parsedRegs:
                oldType, oldLineno = parsedRegs[regName]
                if oldType == typeName:
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
                parsedRegs[regName] = (typeName, reader.lineno)

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

    regs = dict((name, typ) for name, (typ, lineno) in parsedRegs.items())
    logger.debug('reg: %s', regs)

    return None

def checkInstrSet(pathname):
    logger.info('checking: %s', pathname)
    instrSet = parseInstrSet(pathname)
