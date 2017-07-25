from .expression import (
    AddOperator, AndOperator, Complement, IntLiteral, LShift, LVShift, Negation,
    OrOperator, RShift, RVShift, SimplifiableComposedExpression, SignExtension,
    SignTest, XorOperator, optSlice
    )
from .types import maskForWidth, widthForMask

def complexity(expr):
    '''Returns a postive number that reflects the complexity of the given
    expression: the higher the number, the more complex the expression.
    This is used to compare simplification candidates.
    '''
    if isinstance(expr, IntLiteral):
        return 1
    elif isinstance(expr, SimplifiableComposedExpression):
        return expr.nodeComplexity + sum(
            complexity(subExpr) for subExpr in expr.exprs
            )
    elif isinstance(expr, (
            Complement, Negation, SignExtension, LShift, RShift)):
        return 1 + complexity(expr.expr)
    else:
        return 2

def _simplifyAlgebraic(cls, exprs):
    '''Simplify the given list of expressions using algebraic properties of the
    given SimplifiableComposedExpression subclass.
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
    while firstLiteral != 0 and isinstance(exprs[firstLiteral - 1], IntLiteral):
        firstLiteral -= 1
    i = 0
    while i < firstLiteral:
        expr = exprs[i]
        if isinstance(expr, IntLiteral):
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
    _simplifyAlgebraic(composed.__class__, exprs)
    while True:
        if not _simplifyList(exprs):
            break
        if not _simplifyAlgebraic(composed.__class__, exprs):
            break

    _customSimplifiers[type(composed)](composed, exprs)

    if len(exprs) == 0:
        substitute = composed.emptySubstitute
        assert substitute is not None
        return simplifyExpression(substitute)
    elif len(exprs) == 1:
        return exprs[0]
    elif len(exprs) == len(composed.exprs) \
            and all(new is old for new, old in zip(exprs, composed.exprs)):
        return composed
    else:
        return composed.__class__(*exprs)

def _customSimplifyAnd(node, exprs):
    # pylint: disable=protected-access
    if len(exprs) < 2:
        return

    def simplifyRestricted(alt):
        assert isinstance(alt, AndOperator), alt
        if not node._tryDistributeAndOverOr:
            alt._tryDistributeAndOverOr = False
        return simplifyExpression(alt)

    mask = node.computeMask(exprs)

    # Remove mask literal from subexpressions; we'll re-add it later if needed.
    orgMaskLiteral = exprs[-1]
    if isinstance(orgMaskLiteral, IntLiteral):
        del exprs[-1]
    else:
        orgMaskLiteral = None

    # Try to simplify individual subexpressions by applying bit mask.
    changed = False
    maskLiteral = IntLiteral(mask)
    for i, expr in enumerate(exprs):
        if len(exprs) >= 2:
            masked = simplifyRestricted(AndOperator(expr, maskLiteral))
            if masked is not expr and complexity(masked) <= complexity(expr):
                exprs[i] = expr = masked
                changed = True
        masked = _simplifyMasked(expr, mask)
        if masked is not expr:
            exprs[i] = expr = masked
            changed = True
    if changed:
        # Force earlier simplification steps to run again.
        alt = simplifyRestricted(AndOperator(*(exprs + [maskLiteral])))
        exprs[:] = [simplifyExpression(alt)]
        return

    # Append mask if it is not redundant.
    if mask != -1 and mask != node.computeMask(exprs):
        # Non-simplified expressions should remain the same objects.
        maskChanged = orgMaskLiteral is None or mask != orgMaskLiteral.value
        exprs.append(IntLiteral(mask) if maskChanged else orgMaskLiteral)

    if node._tryDistributeAndOverOr:
        myComplexity = node.nodeComplexity \
            + sum(complexity(expr) for expr in exprs)
        for i, expr in enumerate(exprs):
            if isinstance(expr, OrOperator):
                # Distribute AND over OR.
                andExprs = exprs[:i] + exprs[i+1:]
                alt = OrOperator(*(
                    AndOperator(term, *andExprs)
                    for term in expr.exprs
                    ))
                alt._tryDistributeOrOverAnd = False
                alt = simplifyExpression(alt)
                if complexity(alt) < myComplexity:
                    exprs[:] = [alt]
                    return

def _customSimplifyOr(node, exprs):
    # pylint: disable=protected-access
    if not exprs:
        return

    myComplexity = node.nodeComplexity + sum(complexity(expr) for expr in exprs)
    for i, expr in enumerate(exprs):
        if isinstance(expr, AndOperator) and node._tryDistributeOrOverAnd:
            # Distribute OR over AND.
            orExprs = exprs[:i] + exprs[i+1:]
            alt = AndOperator(*(
                OrOperator(term, *orExprs)
                for term in expr.exprs
                ))
            alt._tryDistributeAndOverOr = False
            alt = simplifyExpression(alt)
            if complexity(alt) < myComplexity:
                exprs[:] = [alt]
                return

def _customSimplifyXor(node, exprs): # pylint: disable=unused-argument
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

    if not exprs:
        return

    # TODO: Distribution over AND and OR.

def _customSimplifyAdd(node, exprs): # pylint: disable=unused-argument
    # Remove pairs of A and -A.
    complIdx = 0
    while complIdx < len(exprs):
        compl = exprs[complIdx]
        if not isinstance(compl, Complement):
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

_customSimplifiers = {
    AndOperator: _customSimplifyAnd,
    OrOperator: _customSimplifyOr,
    XorOperator: _customSimplifyXor,
    AddOperator: _customSimplifyAdd,
    }

def _simplifyComplement(complement):
    expr = simplifyExpression(complement.expr)
    if isinstance(expr, IntLiteral):
        return IntLiteral(-expr.value)
    elif isinstance(expr, Complement):
        return expr.expr
    elif isinstance(expr, AddOperator):
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
    return isinstance(masked, IntLiteral) and masked.value != 0

def _simplifyNegation(negation):
    expr = simplifyExpression(negation.expr)

    if isinstance(expr, IntLiteral):
        return IntLiteral(int(not expr.value))
    elif isinstance(expr, (LShift, Complement)):
        return _simplifyNegation(Negation(expr.expr))
    elif isinstance(expr, Negation):
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
    if isinstance(expr, AddOperator):
        if all(term.mask >= 0 for term in expr.exprs):
            # If all terms are non-negative, one non-zero term will take the
            # result above zero.
            alt = simplifyExpression(AndOperator(*(
                Negation(term) for term in expr.exprs
                )))
    elif isinstance(expr, OrOperator):
        # OR produces zero iff all of its terms are zero.
        alt = simplifyExpression(AndOperator(*(
            Negation(term) for term in expr.exprs
            )))
    elif isinstance(expr, RShift):
        subExpr = expr.expr
        if isinstance(subExpr, (AndOperator, OrOperator, XorOperator)):
            # Distribute RShift over bitwise operator.
            alt = simplifyExpression(Negation(subExpr.__class__(*(
                RShift(term, expr.offset) for term in subExpr.exprs
                ))))

    if alt is not None and complexity(alt) < complexity(negation):
        return alt
    else:
        return negation

def _simplifySignTest(signTest):
    expr = simplifyExpression(signTest.expr)
    if expr.mask >= 0:
        # Negative values must have a negative mask.
        return IntLiteral(0)
    elif isinstance(expr, IntLiteral):
        return IntLiteral(1 if expr.value < 0 else 0)
    elif isinstance(expr, SignExtension):
        return simplifyExpression(optSlice(expr.expr, expr.width - 1, 1))
    elif expr is signTest.expr:
        return signTest
    else:
        return SignTest(expr)

def _simplifySignExtension(signExtend):
    width = signExtend.width
    mask = maskForWidth(width)
    expr = _simplifyMasked(simplifyExpression(signExtend.expr), mask)

    if isinstance(expr, IntLiteral):
        value = expr.value & mask
        value -= (value << 1) & (1 << width)
        return IntLiteral(value)

    # If the sign is known, we can replace the sign extension operator.
    if width != 0:
        signMask = 1 << (width - 1)
        sign = _simplifyMasked(expr, signMask)
        if isinstance(sign, IntLiteral):
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

    if isinstance(expr, IntLiteral):
        return IntLiteral(expr.value << offset)
    elif isinstance(expr, LShift):
        # Combine both shifts into one.
        return simplifyExpression(LShift(expr.expr, offset + expr.offset))
    elif isinstance(expr, RShift):
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
    elif isinstance(expr, (AndOperator, OrOperator)):
        alt = type(expr)(
            *(LShift(term, offset) for term in expr.exprs)
            )
        alt = simplifyExpression(alt)
        if complexity(alt) <= complexity(lshift):
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

    if isinstance(expr, IntLiteral):
        return IntLiteral(expr.value >> offset)
    elif isinstance(expr, LShift):
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
    elif isinstance(expr, RShift):
        # Combine both shifts into one.
        return simplifyExpression(RShift(expr.expr, offset + expr.offset))
    elif isinstance(expr, (AndOperator, OrOperator)):
        alt = type(expr)(
            *(RShift(term, offset) for term in expr.exprs)
            )
        alt = simplifyExpression(alt)
        if complexity(alt) < complexity(rshift):
            return alt

    if expr is rshift.expr:
        return rshift
    else:
        return RShift(expr, offset)

def _simplifyLVShift(lvshift):
    offset = simplifyExpression(lvshift.offset)
    if isinstance(offset, IntLiteral):
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
    if isinstance(offset, IntLiteral):
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

    if isinstance(expr, IntLiteral):
        # Apply mask to value.
        value = expr.value
        maskedValue = value & mask
        if value != maskedValue:
            return IntLiteral(maskedValue)
    elif isinstance(expr, LShift):
        # Shift mask in the opposite direction.
        subExpr = expr.expr
        offset = expr.offset
        subMasked = _simplifyMasked(subExpr, mask >> offset)
        if subMasked is not subExpr:
            return simplifyExpression(LShift(subMasked, offset))
    elif isinstance(expr, RShift):
        # Shift mask in the opposite direction.
        subExpr = expr.expr
        offset = expr.offset
        subMasked = _simplifyMasked(subExpr, mask << offset)
        if subMasked is not subExpr:
            return simplifyExpression(RShift(subMasked, offset))
    elif isinstance(expr, (AndOperator, OrOperator, XorOperator, AddOperator)):
        # Apply mask to each term.
        if isinstance(expr, AddOperator):
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
        if isinstance(expr, (AndOperator, OrOperator)) and len(terms) >= 2:
            last = terms[-1]
            if isinstance(last, IntLiteral) and last.value == mask:
                if isinstance(expr, AndOperator):
                    # Eliminate inner mask that is equal to the applied mask.
                    del terms[-1]
                    changed = True
                elif isinstance(expr, OrOperator):
                    # Eliminate OR that is all-ones in masked area.
                    return IntLiteral(-1)
        if changed:
            return simplifyExpression(type(expr)(*terms))
    elif isinstance(expr, Complement):
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
        if isinstance(expr, IntLiteral):
            assert expr.value == 0, expr.value
            return expr
        else:
            return IntLiteral(0)

    simplifier = _simplifiers.get(type(expr))
    if simplifier is None:
        return expr
    else:
        return simplifier(expr)
