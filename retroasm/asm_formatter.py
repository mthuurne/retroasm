from .mode import PlaceholderRole

class Formatter:

    margin = 20
    operationWidth = 8

    def __init__(self):
        self._lineFormat = '{:%d}{:%d}{}' % (self.margin, self.operationWidth)

    def formatInt(self, value, typ):
        return (
            '{:d}'
            if value < 16 or typ.signed else
            '${:0%dx}' % ((typ.width + 3) // 4)
            ).format(value)

    def _formatOperands(self, operands):
        prevWord = False
        parts = []
        for operand in operands:
            isWord = operand[0].isalnum() or operand[0] in '_$%'
            if prevWord and isWord:
                parts.append(' ')
            parts.append(operand)
            prevWord = isWord
        return ''.join(parts)

    def formatLabel(self, label):
        return label + ':'

    def formatMnemonic(self, mnemonic, labels):
        parts = []
        for mnemElem in mnemonic:
            if isinstance(mnemElem, str):
                parts.append(mnemElem)
            else:
                value, typ = mnemElem
                # TODO: Role detection needs to be re-implemented.
                roles = frozenset()
                label = (
                    labels.get(value)
                    if PlaceholderRole.code_addr in roles or
                        PlaceholderRole.data_addr in roles else
                    None
                    )
                if label is None:
                    parts.append(self.formatInt(value, typ))
                else:
                    parts.append(label)

        localLabel = ''
        return self._lineFormat.format(
            localLabel, parts[0], self._formatOperands(parts[1:])
            ).rstrip()

    def formatData(self, encoded, typ):
        directive = {8: 'db', 16: 'dw', 32: 'dd', 64: 'dq'}[typ.width]
        mnemonic = [directive, (encoded, typ)]
        return self.formatMnemonic(mnemonic, {})
