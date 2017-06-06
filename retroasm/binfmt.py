from collections import namedtuple
from enum import Enum
from logging import getLogger
from pathlib import PurePath
from struct import Struct

logger = getLogger('binfmt')

ByteOrder = Enum('ByteOrder', ( # pylint: disable=invalid-name
    'undefined', 'little', 'big'
    ))

EntryPoint = namedtuple('EntryPoint', (
    'label', 'instrSetName', 'byteorder', 'offset', 'addr', 'size'
    ))
'''A point at which execution can start: label name (or None), name of the
instruction set, offset into the image, address at which the processor will see
the instruction, size of the code area in bytes (None if unknown).
'''

class BinaryFormat:
    '''Base class for binary formats.
    '''

    name = None
    '''Short identifying name for this format.'''
    description = None
    '''User-friendly name for this format.'''
    extensions = ()
    '''Sequence of file name extensions, lower case, excluding the dot.'''

    @classmethod
    def checkImage(cls, image):
        '''Checks whether the given image (read-only buffer) might be an
        instance of this binary format.
        Returns a positive number for likely matches, zero for undecided and
        a negative number for unlikely matches. The more certain, the further
        the number should be from zero, where 1000 means "very likely" and
        -1000 means "very unlikely"; values are outside that range may be
        returned but will be clipped to [-1000, 1000].
        '''
        raise NotImplementedError

    def __init__(self, image):
        self.image = image

    def iterEntryPoints(self):
        '''Iterates through the EntryPoints in this binary.
        '''
        raise NotImplementedError

class GameBoyROM(BinaryFormat):

    name = 'gbrom'
    description = 'Game Boy ROM image'
    extensions = ('gb', 'gbc')

    header = Struct('<4s48s16s2sBBBBBBBBH')
    logo = bytes((
        0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B,
        0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D,
        0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E,
        0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99,
        0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC,
        0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E
        ))

    @classmethod
    def checkImage(cls, image):
        header = _unpackStruct(image, 0x100, cls.header)
        if header is None:
            return -1000
        else:
            return 1000 if header[1] == cls.logo else -1000

    def iterEntryPoints(self):
        header = _unpackStruct(self.image, 0x100, self.header)
        if header is None:
            raise ValueError('No header')
        entry = header[0]
        if entry[0] == 0x00 and entry[1] == 0xc3:
            # TODO: Once we can properly trace, we don't need this workaround
            #       anymore.
            addr = entry[2] | (entry[3] << 8)
            yield EntryPoint(
                None, 'lr35902', ByteOrder.little, addr, addr, None
                )
        else:
            yield EntryPoint(
                None, 'lr35902', ByteOrder.little, 0x100, 0x100, 4
                )

class MSXROM(BinaryFormat):

    name = 'msxrom'
    description = 'MSX ROM image'
    extensions = ('rom',)

    header = Struct('<2sHHHH6s')

    @classmethod
    def checkImage(cls, image):
        header = _unpackStruct(image, 0, cls.header)
        if header is None:
            return -1000
        cartID, init, statement, device, text, reserved = header

        if cartID == b'AB':
            # ROM cartridge.
            score = 400
            if 0x4000 <= init < 0xC000:
                score += 200
            elif init != 0:
                score -= 500
            if not (statement == 0 or 0x4000 <= statement < 0x8000):
                score -= 100
            if not (device == 0 or 0x4000 <= device < 0x8000):
                score -= 100
            if not (text == 0 or 0x8000 <= text < 0xC000):
                score -= 100
            score += sum(10 if byte == 0 else 0 for byte in reserved)
        elif cartID == b'CD':
            # Sub ROM, only used for BIOS.
            score = 100
            if init >= 0x4000:
                score -= 200
            if statement >= 0x4000:
                score -= 200
            if device >= 0x4000:
                score -= 200
            if text >= 0x4000:
                score -= 200
            score += sum(10 if byte == 0 else 0 for byte in reserved[1:])
        else:
            score = -1000

        if image.size() % 8192 != 0:
            score -= 500

        return score

    def iterEntryPoints(self):
        header = _unpackStruct(self.image, 0, self.header)
        if header is None:
            raise ValueError('No header')
        cartID, init, statement, device, text, reserved = header

        # Figure out address at which ROM is mapped into memory.
        if cartID == b'AB':
            if self.image.size() <= 0x4000:
                baseFreqs = [0] * 4
                for addr in (init, statement, device, text):
                    if addr != 0:
                        baseFreqs[addr >> 14] += 1
                if baseFreqs[1] == 0 and baseFreqs[2] != 0:
                    base = 0x8000
                else:
                    base = 0x4000
            else:
                base = 0x4000
        elif cartID == b'CD':
            base = 0x0000
        else:
            raise ValueError('Unknown cartridge type')

        def ep(name, addr):
            return EntryPoint(
                name, 'z80', ByteOrder.little, addr - base, addr, None
                )
        if init != 0:
            yield ep('init', init)
        if statement != 0:
            yield ep('statement', statement)
        if device != 0:
            yield ep('device', device)
        # Note: TEXT points to tokenized MSX-BASIC and is therefore not an
        #       entry point.

# Build a dictionary of binary formats using introspection.
def _discoverBinaryFormats(localObjects):
    for obj in localObjects:
        if isinstance(obj, type) and issubclass(obj, BinaryFormat):
            if obj is not BinaryFormat:
                yield obj.name, obj
_formatsByName = dict(_discoverBinaryFormats(locals().values()))

def iterBinaryFormatNames():
    '''Iterates through the names of supported binary formats, in no particular
    order.
    '''
    return _formatsByName.keys()

def getBinaryFormat(name):
    '''Looks up a binary format by its name attribute.
    Returns the BinaryFormat with the given value for its name attribute.
    Raises KeyError if there is no match.
    '''
    return _formatsByName[name]

def _detectBinaryFormats(image, names, extMatches):
    logger.debug(
        'Binary format autodetection, extension %s:',
        'matches' if extMatches else 'does not match'
        )
    if len(names) == 0:
        logger.debug('  no formats')
        return None

    boost = 100 if extMatches else 0
    checkResults = []
    for name in names:
        binfmt = _formatsByName[name]
        likely = min(1000, max(-1000, binfmt.checkImage(image))) + boost
        checkResults.append((likely, name))
        logger.debug('  format "%s": score %d', name, likely)

    likely, name = max(checkResults)
    if likely > 0:
        logger.debug('Best match: %s', name)
        return _formatsByName[name]
    else:
        logger.debug('No match')
        return None

def detectBinaryFormat(image, fileName=None):
    '''Attempts to autodetect the binary format of the given image.
    If a file name is given, its extension will be used to first test the
    formats matching that extension, as well as considering those formats
    to be more likely matches.
    Returns a BinaryFormat instance on success, None on failure.
    '''
    names = set(_formatsByName.keys())

    if fileName is not None:
        # First try formats for which the file name extension matches.
        binPath = PurePath(fileName)
        ext = binPath.suffix.lstrip('.').lower()
        namesForExt = set(
            binfmt.name
            for binfmt in _formatsByName.values()
            if ext in binfmt.extensions
            )
        binfmt = _detectBinaryFormats(image, namesForExt, True)
        if binfmt is not None:
            return binfmt
        names -= namesForExt

    # Try other formats.
    return _detectBinaryFormats(image, names, False)

def _unpackStruct(image, offset, struct):
    '''Unpacks the given struct from the given offset of the given image.
    Returns the unpacked data, or None if the image did not contain enough
    data at the given offset.
    '''
    end = offset + struct.size
    return None if end > image.size() else struct.unpack(image[offset:end])
