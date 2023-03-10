from __future__ import annotations

from collections.abc import Callable
from typing import Any, cast

from .expression import (
    AddOperator,
    AndOperator,
    Complement,
    Expression,
    IntLiteral,
    LShift,
    LVShift,
    MultiExpression,
    Negation,
    OrOperator,
    RShift,
    RVShift,
    SignExtension,
    SignTest,
    XorOperator,
    opt_slice,
)
from .types import mask_for_width, width_for_mask


def _simplifyAlgebraic(cls: type[MultiExpression], exprs: list[Expression]) -> bool:
    """
    Simplify the given list of expressions using algebraic properties of the
    given MultiExpression subclass.
    Returns True if the expression list was changed, False otherwise.
    """
    changed = False

    # Merge subexpressions of the same type into this expression.
    i = 0
    while i < len(exprs):
        expr = exprs[i]
        if isinstance(expr, cls):
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
        match exprs[i]:
            case IntLiteral() as expr:
                del exprs[i]
                exprs.append(expr)
                firstLiteral -= 1
                changed = True
            case _:
                i += 1

    # Merge literals.
    if len(exprs) - firstLiteral >= 2:
        value = cls.combine_literals(
            *(cast(IntLiteral, expr).value for expr in exprs[firstLiteral:])
        )
        exprs[firstLiteral:] = [IntLiteral(value)]
        changed = True

    # Check remaining literal.
    if len(exprs) - firstLiteral == 1:
        value = cast(IntLiteral, exprs[-1]).value

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


def _simplifyList(exprs: list[Expression]) -> bool:
    """
    Simplify the given list of expressions individually.
    Returns True if any of the expressions was replaced by a simpler equivalent,
    False otherwise.
    """
    changed = False
    for i, expr in enumerate(exprs):
        simplified = simplifyExpression(expr)
        if simplified is not expr:
            exprs[i] = simplified
            changed = True
    return changed


def _simplifyComposed(composed: MultiExpression) -> Expression:
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


def _customSimplifyAnd(node: AndOperator, exprs: list[Expression]) -> bool:
    # pylint: disable=protected-access
    if len(exprs) < 2:
        return False

    # Remove mask literal from subexpressions; we'll re-add it later if needed.
    orgMaskLiteral: IntLiteral | None
    match exprs[-1]:
        case IntLiteral(value=explicitMask) as orgMaskLiteral:
            del exprs[-1]
        case _:
            explicitMask = -1
            orgMaskLiteral = None

    exprMask = node.compute_mask(exprs)
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
        if orgMaskLiteral is None:
            maskChanged = True
            exprs.append(IntLiteral(mask))
        else:
            maskChanged = mask != explicitMask
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
        myComplexity = node.node_complexity + sum(expr.complexity for expr in exprs)
        for i, expr in enumerate(exprs):
            match expr:
                case OrOperator(exprs=terms):
                    # Distribute AND over OR.
                    andExprs = exprs[:i] + exprs[i + 1 :]
                    distAlt = OrOperator(
                        *(AndOperator(term, *andExprs) for term in terms)
                    )
                    distAlt._tryDistributeOrOverAnd = False
                    distAltSimp = simplifyExpression(distAlt)
                    if distAltSimp.complexity < myComplexity:
                        exprs[:] = [distAltSimp]
                        return True

    return maskChanged


def _customSimplifyOr(node: OrOperator, exprs: list[Expression]) -> bool:
    # pylint: disable=protected-access
    if not exprs:
        return False

    if node._tryDistributeOrOverAnd:
        myComplexity = node.node_complexity + sum(expr.complexity for expr in exprs)
        for i, expr in enumerate(exprs):
            match expr:
                case AndOperator(exprs=terms):
                    # Distribute OR over AND.
                    orExprs = exprs[:i] + exprs[i + 1 :]
                    distAlt = AndOperator(
                        *(OrOperator(term, *orExprs) for term in terms)
                    )
                    distAlt._tryDistributeAndOverOr = False
                    distAltSimp = simplifyExpression(distAlt)
                    if distAltSimp.complexity < myComplexity:
                        exprs[:] = [distAltSimp]
                        return True

    return False


def _customSimplifyXor(
    node: XorOperator, exprs: list[Expression]  # pylint: disable=unused-argument
) -> bool:
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


def _customSimplifyAdd(
    node: AddOperator, exprs: list[Expression]  # pylint: disable=unused-argument
) -> bool:
    changed = False

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
            changed = True

    return changed


_customSimplifiers: dict[
    type[MultiExpression], Callable[[Any, list[Expression]], bool]
] = {
    AndOperator: _customSimplifyAnd,
    OrOperator: _customSimplifyOr,
    XorOperator: _customSimplifyXor,
    AddOperator: _customSimplifyAdd,
}


def _simplifyComplement(complement: Complement) -> Expression:
    match simplifyExpression(complement.expr):
        case IntLiteral(value=value):
            return IntLiteral(-value)
        case Complement(expr=expr):
            return expr
        case AddOperator(exprs=terms):
            # Distribute complement over addition terms:
            #   -(x + y + z) = -x + -y + -z
            return simplifyExpression(
                AddOperator(*(Complement(term) for term in terms))
            )
        case expr:
            if expr is complement.expr:
                return complement
            else:
                return Complement(expr)


def _testBit(expr: Expression, bit: int) -> bool:
    """
    Returns True if the given bit of the given expression is certainly set,
    or False if it is unknown or certainly unset.
    """
    masked = _simplifyMasked(expr, 1 << bit)
    return isinstance(masked, IntLiteral) and masked.value != 0


def _simplifyNegation(negation: Negation) -> Expression:
    match simplifyExpression(negation.expr):
        case IntLiteral(value=value):
            return IntLiteral(int(not value))
        case LShift(expr=expr) | Complement(expr=expr):
            return _simplifyNegation(Negation(expr))
        case Negation(expr=expr) if expr.mask == 1:
            return expr
        case expr:
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
    match expr:
        case AddOperator(exprs=terms) if all(term.mask >= 0 for term in terms):
            # If all terms are non-negative, one non-zero term will take the
            # result above zero.
            alt = simplifyExpression(AndOperator(*(Negation(term) for term in terms)))
        case OrOperator(exprs=terms):
            # OR produces zero iff all of its terms are zero.
            alt = simplifyExpression(AndOperator(*(Negation(term) for term in terms)))
        case RShift(expr=subExpr, offset=offset):
            match subExpr:
                case AndOperator(exprs=terms) | OrOperator(exprs=terms) | XorOperator(
                    exprs=terms
                ):
                    # Distribute RShift over bitwise operator.
                    alt = simplifyExpression(
                        Negation(
                            subExpr.__class__(*(RShift(term, offset) for term in terms))
                        )
                    )
    if alt is not None and alt.complexity < negation.complexity:
        return alt
    else:
        return negation


def _simplifySignTest(signTest: SignTest) -> Expression:  # type: ignore[return]
    match simplifyExpression(signTest.expr):
        case Expression(mask=mask) if mask >= 0:
            # Negative values must have a negative mask.
            return IntLiteral(0)
        case IntLiteral(value=value):
            return IntLiteral(1 if value < 0 else 0)
        case SignExtension(expr=expr, width=width):
            return simplifyExpression(opt_slice(expr, width - 1, 1))
        case expr:
            if expr is signTest.expr:
                return signTest
            else:
                return SignTest(expr)


def _simplifySignExtension(signExtend: SignExtension) -> Expression:
    width = signExtend.width
    mask = mask_for_width(width)

    match _simplifyMasked(simplifyExpression(signExtend.expr), mask):
        case IntLiteral(value=value) as expr:
            value &= mask
            value -= (value << 1) & (1 << width)
            return IntLiteral(value)
        case expr:
            pass

    # If the sign is known, we can replace the sign extension operator.
    if width != 0:
        signMask = 1 << (width - 1)
        match _simplifyMasked(expr, signMask):
            case IntLiteral(value=value):
                nonSign = _simplifyMasked(expr, mask & ~signMask)
                if value & signMask:
                    return simplifyExpression(
                        OrOperator(nonSign, IntLiteral(-1 << (width - 1)))
                    )
                else:
                    return nonSign

    if expr is signExtend.expr:
        return signExtend
    else:
        return SignExtension(expr, width)


def _simplifyLShift(lshift: LShift) -> Expression:
    expr = simplifyExpression(lshift.expr)

    offset = lshift.offset
    if offset == 0:
        # No actual shift occurs.
        return expr

    match expr:
        case IntLiteral(value=value):
            return IntLiteral(value << offset)
        case LShift(expr=expr, offset=loffset):
            # Combine both shifts into one.
            return simplifyExpression(LShift(expr, offset + loffset))
        case RShift(expr=expr, mask=mask, offset=roffset):
            masked = AndOperator(expr, IntLiteral(mask << roffset))
            if roffset < offset:
                # Left shift wins.
                return simplifyExpression(LShift(masked, offset - roffset))
            elif roffset == offset:
                # Left and right shift cancel each other out.
                return simplifyExpression(masked)
            else:
                # Right shift wins.
                return simplifyExpression(RShift(masked, roffset - offset))
        case AndOperator() | OrOperator():
            alt = simplifyExpression(
                expr.__class__(*(LShift(term, offset) for term in expr.exprs))
            )
            if alt.complexity <= lshift.complexity:
                return alt

    if expr is lshift.expr:
        return lshift
    else:
        return LShift(expr, offset)


def _simplifyRShift(rshift: RShift) -> Expression:
    expr = simplifyExpression(rshift.expr)

    offset = rshift.offset
    if offset == 0:
        # No actual shift occurs.
        return expr

    if expr.mask >> offset == 0:
        # Entire subexpression is discarded by the shift.
        return IntLiteral(0)

    match expr:
        case IntLiteral(value=value):
            return IntLiteral(value >> offset)
        case LShift(offset=loffset):
            if loffset < offset:
                # Right shift wins.
                return simplifyExpression(RShift(expr.expr, offset - loffset))
            elif loffset == offset:
                # Left and right shift cancel each other out.
                return expr.expr
            else:
                # Left shift wins.
                return simplifyExpression(LShift(expr.expr, loffset - offset))
        case RShift(expr=expr, offset=roffset):
            # Combine both shifts into one.
            return simplifyExpression(RShift(expr, offset + roffset))
        case AndOperator() | OrOperator():
            alt = simplifyExpression(
                expr.__class__(*(RShift(term, offset) for term in expr.exprs))
            )
            if alt.complexity < rshift.complexity:
                return alt

    if expr is rshift.expr:
        return rshift
    else:
        return RShift(expr, offset)


def _simplifyLVShift(lvshift: LVShift) -> Expression:
    match simplifyExpression(lvshift.offset):
        case IntLiteral(value=value):
            # The offset is constant; convert to LShift.
            return _simplifyLShift(LShift(lvshift.expr, value))
        case offset:
            pass

    # Note: Various other simplifications are possible, but I don't know which
    #       ones occur often enough in practice to be worth including.
    expr = simplifyExpression(lvshift.expr)
    if expr is lvshift.expr and offset is lvshift.offset:
        return lvshift
    else:
        return LVShift(expr, offset)


def _simplifyRVShift(rvshift: RVShift) -> Expression:
    match simplifyExpression(rvshift.offset):
        case IntLiteral(value=value):
            # The offset is constant; convert to RShift.
            return _simplifyRShift(RShift(rvshift.expr, value))
        case offset:
            pass

    # Note: Various other simplifications are possible, but I don't know which
    #       ones occur often enough in practice to be worth including.
    expr = simplifyExpression(rvshift.expr)
    if expr is rvshift.expr and offset is rvshift.offset:
        return rvshift
    else:
        return RVShift(expr, offset)


def _simplifyMasked(expr: Expression, mask: int) -> Expression:
    """
    Returns a simplified version of the given expression, such that it
    has the same value when the given mask is applied to it. If no such
    simplification can be found, the original expression object is returned.
    Only mask-related simplifications are examined: typically callers will
    already have performed simplification of the full expression.
    """
    if expr.mask & mask == 0:
        # All potentially set bits are zeroed after masking.
        return IntLiteral(0)

    match expr:
        case IntLiteral(value=value):
            # Apply mask to value.
            maskedValue = value & mask
            if value != maskedValue:
                return IntLiteral(maskedValue)
        case LShift(expr=subExpr, offset=offset):
            # Shift mask in the opposite direction.
            subMasked = _simplifyMasked(subExpr, mask >> offset)
            if subMasked is not subExpr:
                return simplifyExpression(LShift(subMasked, offset))
        case RShift(expr=subExpr, offset=offset):
            # Shift mask in the opposite direction.
            subMasked = _simplifyMasked(subExpr, mask << offset)
            if subMasked is not subExpr:
                return simplifyExpression(RShift(subMasked, offset))
        case AndOperator() | OrOperator() | XorOperator() | AddOperator():
            # Apply mask to each term.
            match expr:
                case AddOperator():
                    # Note: Only take truncation at the front into account.
                    #       While truncation could be done inside holes as well,
                    #       implementing that might not be worth the effort.
                    termMask = mask_for_width(width_for_mask(mask))
                case _:
                    termMask = mask
            terms = []
            changed = False
            for term in expr.exprs:
                maskedTerm = _simplifyMasked(term, termMask)
                terms.append(maskedTerm)
                changed |= maskedTerm is not term
            match expr:
                case AndOperator():
                    match terms:
                        case [_, *_, IntLiteral(value=value)] if value == mask:
                            # Eliminate inner mask that is equal to the applied mask.
                            del terms[-1]
                            changed = True
                case OrOperator():
                    match terms:
                        case [_, *_, IntLiteral(value=value)] if value == mask:
                            # Eliminate OR that is all-ones in masked area.
                            return IntLiteral(-1)
            if changed:
                return simplifyExpression(expr.__class__(*terms))
        case Complement(expr=subExpr):
            subMasked = _simplifyMasked(subExpr, mask_for_width(width_for_mask(mask)))
            if subMasked is not subExpr:
                return simplifyExpression(Complement(subMasked))

    return expr


_simplifiers: dict[type[Expression], Callable[[Any], Expression]] = {
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


def simplifyExpression(expr: Expression) -> Expression:
    """
    Returns an equivalent expression that is simpler (fewer nodes), or the
    given expression object itself if no simplification was found.
    Simplified expressions can have reduced width.
    """
    if expr.mask == 0:
        # The only value that matches a 0 mask is 0.
        match expr:
            case IntLiteral(value=value):
                assert value == 0, value
                return expr
            case _:
                return IntLiteral(0)

    simplifier = _simplifiers.get(type(expr))
    if simplifier is None:
        return expr
    else:
        return simplifier(expr)
