from .expression import (
    AddOperator, AndOperator, Complement, IntLiteral, LShift, LVShift,
    MultiExpression, Negation, OrOperator, RShift, RVShift, SignExtension,
    SignTest, XorOperator, optSlice
    )
from .types import maskForWidth, widthForMask

def _simplifyAlgebraic(cls, exprs):
    '''Simplify the given list of expressions using algebraic properties of the
    given MultiExpression subclass.
    Returns True if the expression list was changed, False otherwise.
    '''
    changed = False

    # Merge subexpressions of the same type into this expression.
    i = 0
    while i < len(exprs):
        expr = exprs[i]
        if expr.__class__ is cls:
            del exprs[i]
            exprs += expr.exprs
            changed = True
        else:
            i += 1

    # Move all literals to the end.
    i = 0
    firstLiteral = len(exprs)
    while firstLiteral != 0 and exprs[firstLiteral - 1].__class__ is IntLiteral:
        firstLiteral -= 1
    i = 0
    while i < firstLiteral:
        expr = exprs[i]
        if expr.__class__ is IntLiteral:
            del exprs[i]
            exprs.append(expr)
            firstLiteral -= 1
            changed = True
        else:
            i += 1

    # Merge literals.
    if len(exprs) - firstLiteral >= 2:
        value = cls.combineLiterals(*(
            expr.value for expr in exprs[firstLiteral:]
            ))
        exprs[firstLiteral:] = [IntLiteral(value)]
        changed = True

    # Check remaining literal.
    if len(exprs) - firstLiteral == 1:
        value = exprs[-1].value

        # If the absorber is present, the result is the absorber.
        absorber = cls.absorber
        if value == absorber:
            if len(exprs) != 1:
                del exprs[:-1]
                changed = True
            return changed

        # Remove identity literal.
        identity = cls.identity
        if value == identity:
            del exprs[-1]
            changed = True

    if cls.idempotent:
        # Remove duplicate values.
        i = 0
        while i + 1 < firstLiteral:
            expr = exprs[i]
            i += 1
            j = i
            while j < firstLiteral:
                if exprs[j] == expr:
                    del exprs[j]
                    firstLiteral -= 1
                    changed = True
                else:
                    j += 1

    return changed

def _simplifyList(exprs):
    '''Simplify the given list of expressions individually.
    Returns True if any of the expressions was replaced by a simpler equivalent,
    False otherwise.
    '''
    changed = False
    for i, expr in enumerate(exprs):
        simplified = simplifyExpression(expr)
        if simplified is not expr:
            exprs[i] = simplified
            changed = True
    return changed

def _simplifyComposed(composed):
    exprs = list(composed.exprs)

    # Perform basic simplifications until we get no more improvements from them.
    changed = _simplifyAlgebraic(composed.__class__, exprs)
    while True:
        if not _simplifyList(exprs):
            break
        changed = True
        if not _simplifyAlgebraic(composed.__class__, exprs):
            break

    changed |= _customSimplifiers[type(composed)](composed, exprs)

    if len(exprs) == 0:
        return IntLiteral(composed.identity)
    elif len(exprs) == 1:
        return exprs[0]
    elif changed:
        return composed.__class__(*exprs)
    else:
        return composed

def _customSimplifyAnd(node, exprs):
    # pylint: disable=protected-access
    if len(exprs) < 2:
        return False

    # Remove mask literal from subexpressions; we'll re-add it later if needed.
    orgMaskLiteral = exprs[-1]
    if orgMaskLiteral.__class__ is IntLiteral:
        explicitMask = orgMaskLiteral.value
        del exprs[-1]
    else:
        explicitMask = -1
        orgMaskLiteral = None

    exprMask = node.computeMask(exprs)
    mask = exprMask & explicitMask

    # Try to simplify individual subexpressions by applying bit mask.
    changed = False
    for i, expr in enumerate(exprs):
        masked = _simplifyMasked(expr, mask)
        if masked is not expr:
            exprs[i] = masked
            changed = True

    # Append mask if it is not redundant.
    if mask != exprMask:
        # Non-simplified expressions should remain the same objects.
        maskChanged = orgMaskLiteral is None or mask != explicitMask
        exprs.append(IntLiteral(mask) if maskChanged else orgMaskLiteral)
    else:
        maskChanged = orgMaskLiteral is not None

    # If _simplifyMasked() resulted in simplfications, force earlier steps to
    # run again.
    if changed:
        alt = AndOperator(*exprs)
        alt._tryDistributeAndOverOr = node._tryDistributeAndOverOr
        exprs[:] = [simplifyExpression(alt)]
        return True

    if node._tryDistributeAndOverOr:
        myComplexity = node.nodeComplexity \
            + sum(expr.complexity for expr in exprs)
        for i, expr in enumerate(exprs):
            if expr.__class__ is OrOperator:
                # Distribute AND over OR.
                andExprs = exprs[:i] + exprs[i+1:]
                alt = OrOperator(*(
                    AndOperator(term, *andExprs)
                    for term in expr.exprs
                    ))
                alt._tryDistributeOrOverAnd = False
                alt = simplifyExpression(alt)
                if alt.complexity < myComplexity:
                    exprs[:] = [alt]
                    return True

    return maskChanged

def _customSimplifyOr(node, exprs):
    # pylint: disable=protected-access
    if not exprs:
        return False

    if node._tryDistributeOrOverAnd:
        myComplexity = node.nodeComplexity \
            + sum(expr.complexity for expr in exprs)
        for i, expr in enumerate(exprs):
            if expr.__class__ is AndOperator:
                # Distribute OR over AND.
                orExprs = exprs[:i] + exprs[i+1:]
                alt = AndOperator(*(
                    OrOperator(term, *orExprs)
                    for term in expr.exprs
                    ))
                alt._tryDistributeAndOverOr = False
                alt = simplifyExpression(alt)
                if alt.complexity < myComplexity:
                    exprs[:] = [alt]
                    return True

    return False

def _customSimplifyXor(node, exprs): # pylint: disable=unused-argument
    changed = False

    # Remove duplicate expression pairs: A ^ A == 0.
    i = 0
    while i < len(exprs):
        expr = exprs[i]
        try:
            j = exprs.index(expr, i + 1)
        except ValueError:
            i += 1
        else:
            del exprs[j]
            del exprs[i]
            changed = True

    if not exprs:
        return changed

    # TODO: Distribution over AND and OR.
    return changed

def _customSimplifyAdd(node, exprs): # pylint: disable=unused-argument
    changed = False

    # Remove pairs of A and -A.
    complIdx = 0
    while complIdx < len(exprs):
        compl = exprs[complIdx]
        if compl.__class__ is not Complement:
            complIdx += 1
            continue
        try:
            idx = exprs.index(compl.expr)
        except ValueError:
            complIdx += 1
        else:
            del exprs[idx]
            if idx < complIdx:
                complIdx -= 1
            del exprs[complIdx]
            changed = True

    return changed

_customSimplifiers = {
    AndOperator: _customSimplifyAnd,
    OrOperator: _customSimplifyOr,
    XorOperator: _customSimplifyXor,
    AddOperator: _customSimplifyAdd,
    }

def _simplifyComplement(complement):
    expr = simplifyExpression(complement.expr)
    cls = expr.__class__
    if cls is IntLiteral:
        return IntLiteral(-expr.value)
    elif cls is Complement:
        return expr.expr
    elif cls is AddOperator:
        # Distribute complement over addition terms:
        #   -(x + y + z) = -x + -y + -z
        return simplifyExpression(AddOperator(
            *(Complement(term) for term in expr.exprs)
            ))
    elif expr is complement.expr:
        return complement
    else:
        return Complement(expr)

def _testBit(expr, bit):
    '''Returns True if the given bit of the given expression is certainly set,
    or False if it is unknown or certainly unset.
    '''
    masked = _simplifyMasked(expr, 1 << bit)
    return masked.__class__ is IntLiteral and masked.value != 0

def _simplifyNegation(negation):
    expr = simplifyExpression(negation.expr)
    cls = expr.__class__

    if cls is IntLiteral:
        return IntLiteral(int(not expr.value))
    elif cls is LShift or cls is Complement:
        return _simplifyNegation(Negation(expr.expr))
    elif cls is Negation:
        if expr.expr.mask == 1:
            return expr.expr

    if expr is not negation.expr:
        negation = Negation(expr)

    # If any bit of the expression's value is 1, the value is non-zero.
    if expr.mask >= 0:
        # Test each individual bit position that could possibly be set.
        mask = expr.mask
        bit = 0
        while mask >> bit:
            if (mask >> bit) & 1:
                if _testBit(expr, bit):
                    return IntLiteral(0)
            bit += 1
    else:
        # The expression's value likely has leading ones, so pick an arbitrary
        # high bit position and test that.
        if _testBit(expr, 1025):
            return IntLiteral(0)

    alt = None
    if cls is AddOperator:
        if all(term.mask >= 0 for term in expr.exprs):
            # If all terms are non-negative, one non-zero term will take the
            # result above zero.
            alt = simplifyExpression(AndOperator(*(
                Negation(term) for term in expr.exprs
                )))
    elif cls is OrOperator:
        # OR produces zero iff all of its terms are zero.
        alt = simplifyExpression(AndOperator(*(
            Negation(term) for term in expr.exprs
            )))
    elif cls is RShift:
        subExpr = expr.expr
        if subExpr.__class__ in (AndOperator, OrOperator, XorOperator):
            # Distribute RShift over bitwise operator.
            alt = simplifyExpression(Negation(subExpr.__class__(*(
                RShift(term, expr.offset) for term in subExpr.exprs
                ))))

    if alt is not None and alt.complexity < negation.complexity:
        return alt
    else:
        return negation

def _simplifySignTest(signTest):
    expr = simplifyExpression(signTest.expr)
    if expr.mask >= 0:
        # Negative values must have a negative mask.
        return IntLiteral(0)
    cls = expr.__class__
    if cls is IntLiteral:
        return IntLiteral(1 if expr.value < 0 else 0)
    elif cls is SignExtension:
        return simplifyExpression(optSlice(expr.expr, expr.width - 1, 1))
    elif expr is signTest.expr:
        return signTest
    else:
        return SignTest(expr)

def _simplifySignExtension(signExtend):
    width = signExtend.width
    mask = maskForWidth(width)
    expr = _simplifyMasked(simplifyExpression(signExtend.expr), mask)

    if expr.__class__ is IntLiteral:
        value = expr.value & mask
        value -= (value << 1) & (1 << width)
        return IntLiteral(value)

    # If the sign is known, we can replace the sign extension operator.
    if width != 0:
        signMask = 1 << (width - 1)
        sign = _simplifyMasked(expr, signMask)
        if sign.__class__ is IntLiteral:
            nonSign = _simplifyMasked(expr, mask & ~signMask)
            if sign.value & signMask:
                return simplifyExpression(
                    OrOperator(nonSign, IntLiteral(-1 << (width - 1)))
                    )
            else:
                return nonSign

    if expr is signExtend.expr:
        return signExtend
    else:
        return SignExtension(expr, width)

def _simplifyLShift(lshift):
    expr = simplifyExpression(lshift.expr)

    offset = lshift.offset
    if offset == 0:
        # No actual shift occurs.
        return expr

    cls = expr.__class__
    if cls is IntLiteral:
        return IntLiteral(expr.value << offset)
    elif cls is LShift:
        # Combine both shifts into one.
        return simplifyExpression(LShift(expr.expr, offset + expr.offset))
    elif cls is RShift:
        roffset = expr.offset
        masked = AndOperator(expr.expr, IntLiteral(expr.mask << roffset))
        if roffset < offset:
            # Left shift wins.
            return simplifyExpression(LShift(masked, offset - roffset))
        elif roffset == offset:
            # Left and right shift cancel each other out.
            return simplifyExpression(masked)
        else:
            # Right shift wins.
            return simplifyExpression(RShift(masked, roffset - offset))
    elif cls is AndOperator or cls is OrOperator:
        alt = cls(*(LShift(term, offset) for term in expr.exprs))
        alt = simplifyExpression(alt)
        if alt.complexity <= lshift.complexity:
            return alt

    if expr is lshift.expr:
        return lshift
    else:
        return LShift(expr, offset)

def _simplifyRShift(rshift):
    expr = simplifyExpression(rshift.expr)

    offset = rshift.offset
    if offset == 0:
        # No actual shift occurs.
        return expr

    if expr.mask >> offset == 0:
        # Entire subexpression is discarded by the shift.
        return IntLiteral(0)

    cls = expr.__class__
    if cls is IntLiteral:
        return IntLiteral(expr.value >> offset)
    elif cls is LShift:
        loffset = expr.offset
        if loffset < offset:
            # Right shift wins.
            return simplifyExpression(RShift(expr.expr, offset - loffset))
        elif loffset == offset:
            # Left and right shift cancel each other out.
            return expr.expr
        else:
            # Left shift wins.
            return simplifyExpression(LShift(expr.expr, loffset - offset))
    elif cls is RShift:
        # Combine both shifts into one.
        return simplifyExpression(RShift(expr.expr, offset + expr.offset))
    elif cls is AndOperator or cls is OrOperator:
        alt = type(expr)(
            *(RShift(term, offset) for term in expr.exprs)
            )
        alt = simplifyExpression(alt)
        if alt.complexity < rshift.complexity:
            return alt

    if expr is rshift.expr:
        return rshift
    else:
        return RShift(expr, offset)

def _simplifyLVShift(lvshift):
    offset = simplifyExpression(lvshift.offset)
    if offset.__class__ is IntLiteral:
        # The offset is constant; convert to LShift.
        return _simplifyLShift(LShift(lvshift.expr, offset.value))

    # Note: Various other simplifications are possible, but I don't know which
    #       ones occur often enough in practice to be worth including.
    expr = simplifyExpression(lvshift.expr)
    if expr is lvshift.expr and offset is lvshift.offset:
        return lvshift
    else:
        return LVShift(expr, offset)

def _simplifyRVShift(rvshift):
    offset = simplifyExpression(rvshift.offset)
    if offset.__class__ is IntLiteral:
        # The offset is constant; convert to RShift.
        return _simplifyRShift(RShift(rvshift.expr, offset.value))

    # Note: Various other simplifications are possible, but I don't know which
    #       ones occur often enough in practice to be worth including.
    expr = simplifyExpression(rvshift.expr)
    if expr is rvshift.expr and offset is rvshift.offset:
        return rvshift
    else:
        return RVShift(expr, offset)

def _simplifyMasked(expr, mask):
    '''Returns a simplified version of the given expression, such that it
    has the same value when the given mask is applied to it. If no such
    simplification can be found, the original expression object is returned.
    Only mask-related simplifications are examined: typically callers will
    already have performed simplification of the full expression.
    '''
    if (expr.mask & mask) == 0:
        # All potentially set bits are zeroed after masking.
        return IntLiteral(0)

    cls = expr.__class__
    if cls is IntLiteral:
        # Apply mask to value.
        value = expr.value
        maskedValue = value & mask
        if value != maskedValue:
            return IntLiteral(maskedValue)
    elif cls is LShift:
        # Shift mask in the opposite direction.
        subExpr = expr.expr
        offset = expr.offset
        subMasked = _simplifyMasked(subExpr, mask >> offset)
        if subMasked is not subExpr:
            return simplifyExpression(LShift(subMasked, offset))
    elif cls is RShift:
        # Shift mask in the opposite direction.
        subExpr = expr.expr
        offset = expr.offset
        subMasked = _simplifyMasked(subExpr, mask << offset)
        if subMasked is not subExpr:
            return simplifyExpression(RShift(subMasked, offset))
    elif cls in (AndOperator, OrOperator, XorOperator, AddOperator):
        # Apply mask to each term.
        if cls is AddOperator:
            # Note: Only take truncation at the front into account.
            #       While truncation could be done inside holes as well,
            #       implementing that might not be worth the effort.
            termMask = maskForWidth(widthForMask(mask))
        else:
            termMask = mask
        terms = []
        changed = False
        for term in expr.exprs:
            maskedTerm = _simplifyMasked(term, termMask)
            terms.append(maskedTerm)
            changed |= maskedTerm is not term
        if (cls is AndOperator or cls is OrOperator) and len(terms) >= 2:
            last = terms[-1]
            if last.__class__ is IntLiteral and last.value == mask:
                if cls is AndOperator:
                    # Eliminate inner mask that is equal to the applied mask.
                    del terms[-1]
                    changed = True
                elif cls is OrOperator:
                    # Eliminate OR that is all-ones in masked area.
                    return IntLiteral(-1)
        if changed:
            return simplifyExpression(cls(*terms))
    elif cls is Complement:
        subExpr = expr.expr
        subMasked = _simplifyMasked(subExpr, maskForWidth(widthForMask(mask)))
        if subMasked is not subExpr:
            return simplifyExpression(Complement(subMasked))

    return expr

_simplifiers = {
    AndOperator: _simplifyComposed,
    OrOperator: _simplifyComposed,
    XorOperator: _simplifyComposed,
    AddOperator: _simplifyComposed,
    Complement: _simplifyComplement,
    Negation: _simplifyNegation,
    SignTest: _simplifySignTest,
    SignExtension: _simplifySignExtension,
    LShift: _simplifyLShift,
    RShift: _simplifyRShift,
    LVShift: _simplifyLVShift,
    RVShift: _simplifyRVShift,
    }

def simplifyExpression(expr):
    '''Returns an equivalent expression that is simpler (fewer nodes), or the
    given expression object itself if no simplification was found.
    Simplified expressions can have reduced width.
    '''
    if expr.mask == 0:
        # The only value that matches a 0 mask is 0.
        if expr.__class__ is IntLiteral:
            assert expr.value == 0, expr.value
            return expr
        else:
            return IntLiteral(0)

    simplifier = _simplifiers.get(type(expr))
    if simplifier is None:
        return expr
    else:
        return simplifier(expr)
