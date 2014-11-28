from .expression_parser import parseExpr

def parseFuncBody(log, lines, context):
    for line in lines:
        parts = line.split(':=')
        if len(parts) < 2:
            log.error('no assignment in line')
        elif len(parts) > 2:
            log.error('multiple assignments in a single line')
        else:
            lhsStr, rhsStr = parts
            try:
                lhs = parseExpr(lhsStr, context)
                print(lhs, ':', repr(lhs))
            except ValueError as ex:
                log.error('error in left hand side of assignment: %s', str(ex))
                continue
            try:
                rhs = parseExpr(rhsStr, context)
                print(rhs, ':', repr(rhs))
            except ValueError as ex:
                log.error('error in right hand side of assignment: %s', str(ex))
                continue
            print(lhs, ':=', rhs)
