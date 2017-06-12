class Formatter:

    def formatInt(self, value, typ):
        return (
            '{:d}'
            if value < 16 or typ.signed else
            '${:0%dx}' % ((typ.width + 3) // 4)
            ).format(value)

    def formatMnemonic(self, mnemonic):
        parts = []
        for mnemElem in mnemonic:
            if isinstance(mnemElem, str):
                parts.append(mnemElem)
            else:
                parts.append(self.formatInt(*mnemElem))
        return ' '.join(parts)

    def formatUnknown(self, encoded):
        return '??? %08X' % encoded
