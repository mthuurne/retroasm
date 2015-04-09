from .expression import (
    AddOperator, AndOperator, Complement, IntLiteral, LShift, OrOperator,
    RShift, SimplifiableComposedExpression, Truncation, XorOperator
    )
from .types import maskForWidth, unlimited

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
    elif isinstance(expr, (Complement, LShift, RShift, Truncation)):
        return 1 + complexity(expr.expr)
    else:
        return 2

def _simplifyAlgebraic(cls, exprs):
    '''Simplify the given list of expressions using algebraic properties of the
    given SimplifiableComposedExpression subclass.
    Returns True if the expression list was changed, False otherwise.
    '''
    changed = False

    if cls.associative:
        # Merge subexpressions of the same type into this expression.
        i = 0
        while i < len(exprs):
            expr = exprs[i]
            if expr.__class__ is cls:
                subExprs = expr.exprs
                exprs[i:i+1] = subExprs
                changed = True
            else:
                i += 1

    if cls.associative and cls.commutative:
        # Move all literals to the end.
        # This makes the later merge step more effective.
        numExprs = len(exprs)
        i = 0
        while i < numExprs:
            expr = exprs[i]
            if isinstance(expr, IntLiteral):
                del exprs[i]
                exprs.append(expr)
                numExprs -= 1
                changed = True
            else:
                i += 1

    if cls.associative or len(exprs) == 2:
        # Merge literals.
        i = 1
        while i < len(exprs):
            expr1 = exprs[i - 1]
            if not isinstance(expr1, IntLiteral):
                i += 1
                continue
            expr2 = exprs[i]
            if not isinstance(expr2, IntLiteral):
                i += 2
                continue
            expr = cls.combineLiterals(expr1, expr2)
            if expr is None:
                i += 1
            else:
                exprs[i-1:i+1] = [expr]
                changed = True

    absorber = cls.absorber
    if absorber is not None:
        absorber = simplifyExpression(absorber)
        # If any absorber is present, the result is the absorber.
        if any(expr == absorber for expr in exprs):
            changed |= len(exprs) != 1
            exprs[:] = [absorber]
            return changed

    identity = cls.identity
    if identity is not None:
        # Remove identity values.
        numExprs = len(exprs)
        i = 0
        while i < numExprs:
            if exprs[i] == identity:
                del exprs[i]
                numExprs -= 1
                changed = True
            else:
                i += 1

    if cls.idempotent:
        # Remove duplicate values.
        numExprs = len(exprs)
        i = 0
        while i + 1 < numExprs:
            expr = exprs[i]
            i += 1
            j = i
            while j < numExprs:
                if exprs[j] == expr:
                    del exprs[j]
                    numExprs -= 1
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
        if not node._tryMaskToShift:
            alt._tryMaskToShift = False
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
    truncWidth = maskLiteral.width
    for i, expr in enumerate(exprs):
        if len(exprs) >= 2:
            masked = simplifyRestricted(AndOperator(expr, maskLiteral))
            if masked is not expr and complexity(masked) <= complexity(expr):
                exprs[i] = expr = masked
                changed = True
        if expr.width > truncWidth:
            trunc = _simplifyTruncation(Truncation(expr, truncWidth))
            if trunc is not expr and complexity(trunc) <= complexity(expr):
                exprs[i] = expr = trunc
                changed = True
    if changed:
        # Force earlier simplification steps to run again.
        alt = simplifyRestricted(AndOperator(*(exprs + [maskLiteral])))
        exprs[:] = [simplifyExpression(alt)]
        return

    # Append mask if it is not redundant.
    if mask != -1 and mask != node.computeMask(exprs):
        if orgMaskLiteral is not None and mask == orgMaskLiteral.value:
            # Non-simplified expressions should remain the same objects.
            exprs.append(orgMaskLiteral)
        else:
            exprs.append(IntLiteral(mask))

    myComplexity = node.nodeComplexity + sum(complexity(expr) for expr in exprs)

    assert mask != 0, node
    trailingZeroes = 0
    while (mask >> trailingZeroes) & 1 == 0:
        trailingZeroes += 1
    if trailingZeroes != 0 and node._tryMaskToShift:
        clone = AndOperator(*exprs)
        clone._tryMaskToShift = False
        alt = simplifyExpression(LShift(
            RShift(clone, trailingZeroes),
            trailingZeroes
            ))
        if complexity(alt) < myComplexity:
            exprs[:] = [alt]
            return

    if node._tryDistributeAndOverOr:
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
        mask = maskForWidth(expr.width)
        masked = AndOperator(expr.expr, IntLiteral(mask << roffset))
        masked._tryMaskToShift = False # pylint: disable=protected-access
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
        if not getattr(expr, '_tryMaskToShift', True):
            alt._tryMaskToShift = False # pylint: disable=protected-access
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

    width = expr.width - offset
    if width <= 0:
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
    elif isinstance(expr, Truncation):
        # Truncate after shifting: this maps better to the slice semantics.
        return simplifyExpression(
            Truncation(RShift(expr.expr, offset), width)
            )
    elif isinstance(expr, (AndOperator, OrOperator)):
        alt = type(expr)(
            *(RShift(term, offset) for term in expr.exprs)
            )
        if not getattr(expr, '_tryMaskToShift', True):
            alt._tryMaskToShift = False # pylint: disable=protected-access
        alt = simplifyExpression(alt)
        if complexity(alt) < complexity(rshift):
            return alt

    if expr is rshift.expr:
        return rshift
    else:
        return RShift(expr, offset)

def _simplifyTruncation(truncation):
    width = truncation.width
    assert width is not unlimited, truncation
    if width == 0:
        # Every zero-width expression is equivalent to an empty bitstring.
        return IntLiteral(0)

    # Note that simplification can reduce the width of the subexpression,
    # so do subexpression simplification before checking the width.
    expr = simplifyExpression(truncation.expr)

    # If we're truncating beyond the subexpression's width, reduce the
    # truncation width.
    if expr.width <= width:
        # The subexpression already fits: no truncation needed.
        return expr

    if isinstance(expr, IntLiteral):
        return expr if expr.width <= width else \
            IntLiteral(expr.value & maskForWidth(width))
    elif isinstance(expr, LShift):
        offset = expr.offset
        if offset >= width:
            # Result contains nothing but trailing zeroes.
            return IntLiteral(0)
        else:
            # Truncate before left-shifting.
            trunc = Truncation(expr.expr, width - offset)
            return simplifyExpression(LShift(trunc, offset))
    elif isinstance(expr, RShift):
        subExpr = expr.expr
        offset = expr.offset
        alt = simplifyExpression(Truncation(subExpr, width + offset))
        if complexity(alt) < complexity(subExpr):
            return simplifyExpression(
                Truncation(RShift(alt, offset), width)
                )
    elif isinstance(expr, Truncation):
        # Combine both truncations into one.
        return simplifyExpression(Truncation(expr.expr, width))
    elif isinstance(expr, (AndOperator, OrOperator)):
        alt = simplifyExpression(type(expr)(
            *(Truncation(term, width) for term in expr.exprs)
            ))
        if complexity(alt) < complexity(expr):
            return simplifyExpression(Truncation(alt, width))
    elif isinstance(expr, AddOperator):
        # Eliminate inner truncations that are not narrower than the outer
        # trunctation.
        terms = []
        changed = False
        for term in expr.exprs:
            if isinstance(term, Truncation) and term.width >= width:
                terms.append(term.expr)
                changed = True
            else:
                terms.append(term)
        if changed:
            return simplifyExpression(
                Truncation(AddOperator(*terms), width)
                )
        # Distribute truncation over terms.
        terms = []
        changed = False
        for term in expr.exprs:
            alt = simplifyExpression(Truncation(term, width))
            if complexity(alt) < complexity(term) or (
                    isinstance(term, IntLiteral) and alt.width < term.width):
                term = alt
                changed = True
            terms.append(term)
        if changed:
            return Truncation(AddOperator(*terms), width)
    elif isinstance(expr, Complement):
        # Apply truncation to subexpr.
        alt = simplifyExpression(Complement(Truncation(expr.expr, width)))
        if complexity(alt) < complexity(expr):
            return Truncation(alt, width)

    if expr is truncation.expr:
        return truncation
    else:
        return Truncation(expr, width)

_simplifiers = {
    AndOperator: _simplifyComposed,
    OrOperator: _simplifyComposed,
    XorOperator: _simplifyComposed,
    AddOperator: _simplifyComposed,
    Complement: _simplifyComplement,
    LShift: _simplifyLShift,
    RShift: _simplifyRShift,
    Truncation: _simplifyTruncation,
    }

def simplifyExpression(expr):
    '''Returns an equivalent expression that is simpler (fewer nodes), or the
    given expression object itself if no simplification was found.
    Simplified expressions can have reduced width.
    '''
    simplifier = _simplifiers.get(type(expr))
    if simplifier is None:
        return expr
    else:
        return simplifier(expr)
