from io import SEEK_END
from logging import getLogger
from pathlib import PurePath
from struct import Struct

logger = getLogger('binfmt')

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
    def checkFile(cls, file):
        '''Checks whether the given open file object might be an instance of
        this binary format.
        The file object will stay open.
        The seek position is undefined both before and after.
        Returns a positive number for likely matches, zero for undecided and
        a negative number for unlikely matches. The more certain, the further
        the number should be from zero, where 1000 means "very likely" and
        -1000 means "very unlikely"; values are outside that range may be
        returned but will be clipped to [-1000, 1000].
        '''
        raise NotImplementedError

class RawGameBoyROM(BinaryFormat):

    name = 'gbrom'
    description = 'raw Game Boy ROM image'
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
    def checkFile(cls, file):
        header = _readStruct(file, 0x100, cls.header)
        if header is None:
            return -1000
        else:
            return 1000 if header[1] == cls.logo else -1000

class RawMSXROM(BinaryFormat):

    name = 'msxrom'
    description = 'raw MSX ROM image'
    extensions = ('rom',)

    header = Struct('<2sHHHH6s')

    @classmethod
    def checkFile(cls, file):
        header = _readStruct(file, 0, cls.header)
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

        if score > -1000:
            if _getFileSize(file) % 8192 != 0:
                score -= 500

        return score

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

def _iterBinaryFormatsForExtension(ext):
    suffix = binPath.suffix.lstrip('.').lower()
    for binfmt in _formatsByName.values():
        if ext in binfmt.extensions:
            yield binfmt

def _detectBinaryFormats(file, names, extMatches):
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
        likely = min(1000, max(-1000, binfmt.checkFile(file))) + boost
        checkResults.append((likely, name))
        logger.debug('  format "%s": score %d', name, likely)

    likely, name = max(checkResults)
    if likely > 0:
        logger.debug('Best match: %s', name)
        return _formatsByName[name]
    else:
        logger.debug('No match')
        return None

def detectBinaryFormat(file):
    '''Attempts to autodetect the binary format of the given file object.
    Returns a BinaryFormat instance on success, None on failure.
    The file object will stay open.
    The seek position is undefined both before and after.
    Raises OSError on I/O errors.
    '''

    # First try formats for which the file name extension matches.
    binPath = PurePath(file.name)
    ext = binPath.suffix.lstrip('.').lower()
    namesForExt = set(
        binfmt.name
        for binfmt in _formatsByName.values()
        if ext in binfmt.extensions
        )
    binfmt = _detectBinaryFormats(file, namesForExt, True)
    if binfmt is not None:
        return binfmt

    # Try other formats.
    otherNames = set(_formatsByName.keys()) - namesForExt
    return _detectBinaryFormats(file, otherNames, False)

def _readStruct(file, offset, struct):
    '''Reads the given struct from the given offset of the given open file.
    Returns the unpacked data, or None if the file did not contain enough
    data at the given offset.
    '''
    file.seek(offset)
    length = struct.size
    data = file.read(length)
    if len(data) == length:
        return struct.unpack(data)
    else:
        return None

def _getFileSize(file):
    '''Returns the file size of the given open file.
    Changes the seek position.
    '''
    file.seek(0, SEEK_END)
    return file.tell()
