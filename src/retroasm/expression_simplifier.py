from __future__ import annotations

from collections.abc import Callable
from typing import Any, cast

from .expression import (
    AddOperator,
    AndOperator,
    BadValue,
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


def _simplify_algebraic(cls: type[MultiExpression], exprs: list[Expression]) -> bool:
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
    first_literal = len(exprs)
    while first_literal != 0 and isinstance(exprs[first_literal - 1], IntLiteral):
        first_literal -= 1
    i = 0
    while i < first_literal:
        match exprs[i]:
            case IntLiteral() as expr:
                del exprs[i]
                exprs.append(expr)
                first_literal -= 1
                changed = True
            case _:
                i += 1

    # Merge literals.
    if len(exprs) - first_literal >= 2:
        value = cls.combine_literals(
            *(cast(IntLiteral, expr).value for expr in exprs[first_literal:])
        )
        exprs[first_literal:] = [IntLiteral(value)]
        changed = True

    # Check remaining literal.
    if len(exprs) - first_literal == 1:
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
        while i + 1 < first_literal:
            expr = exprs[i]
            i += 1
            j = i
            while j < first_literal:
                if exprs[j] == expr:
                    del exprs[j]
                    first_literal -= 1
                    changed = True
                else:
                    j += 1

    # Make the order of terms somewhat predictable.
    # This does not produce canonical expressions, but it should be enough to
    # at least compare expressions while unit testing.
    order_before = exprs.copy()
    exprs.sort(key=lambda expr: -expr.complexity)
    exprs.sort(key=lambda expr: -expr.offset if isinstance(expr, LShift) else 0)
    changed |= exprs != order_before

    return changed


def _simplify_list(exprs: list[Expression]) -> bool:
    """
    Simplify the given list of expressions individually.
    Returns True if any of the expressions was replaced by a simpler equivalent,
    False otherwise.
    """
    changed = False
    for i, expr in enumerate(exprs):
        simplified = simplify_expression(expr)
        if simplified is not expr:
            exprs[i] = simplified
            changed = True
    return changed


def _simplify_composed(composed: MultiExpression) -> Expression:
    exprs = list(composed.exprs)

    # Perform basic simplifications until we get no more improvements from them.
    changed = _simplify_algebraic(composed.__class__, exprs)
    while True:
        if not _simplify_list(exprs):
            break
        changed = True
        if not _simplify_algebraic(composed.__class__, exprs):
            break

    changed |= _custom_simplifiers[type(composed)](composed, exprs)

    if len(exprs) == 0:
        return IntLiteral(composed.identity)
    elif len(exprs) == 1:
        return exprs[0]
    elif changed:
        return composed.__class__(*exprs)
    else:
        return composed


def _custom_simplify_and(node: AndOperator, exprs: list[Expression]) -> bool:
    if len(exprs) < 2:
        return False

    # Remove mask literal from subexpressions; we'll re-add it later if needed.
    org_mask_literal: IntLiteral | None
    match exprs[-1]:
        case IntLiteral(value=explicit_mask) as org_mask_literal:
            del exprs[-1]
        case _:
            explicit_mask = -1
            org_mask_literal = None

    expr_mask = node.compute_mask(exprs)
    mask = expr_mask & explicit_mask

    # Try to simplify individual subexpressions by applying bit mask.
    changed = False
    for i, expr in enumerate(exprs):
        masked = _simplify_masked(expr, mask)
        if masked is not expr:
            exprs[i] = masked
            changed = True

    # Append mask if it is not redundant.
    if mask != expr_mask:
        # Non-simplified expressions should remain the same objects.
        if org_mask_literal is None:
            mask_changed = True
            exprs.append(IntLiteral(mask))
        else:
            mask_changed = mask != explicit_mask
            exprs.append(IntLiteral(mask) if mask_changed else org_mask_literal)
    else:
        mask_changed = org_mask_literal is not None

    # If _simplifyMasked() resulted in simplfications, force earlier steps to
    # run again.
    if changed:
        alt = AndOperator(*exprs)
        alt._try_distribute_and_over_or = node._try_distribute_and_over_or
        exprs[:] = [simplify_expression(alt)]
        return True

    if node._try_distribute_and_over_or:
        my_complexity = node.node_complexity + sum(expr.complexity for expr in exprs)
        for i, expr in enumerate(exprs):
            match expr:
                case OrOperator(exprs=terms):
                    # Distribute AND over OR.
                    and_exprs = exprs[:i] + exprs[i + 1 :]
                    dist_alt = OrOperator(
                        *(AndOperator(term, *and_exprs) for term in terms)
                    )
                    dist_alt._try_distribute_or_over_and = False
                    dist_alt_simp = simplify_expression(dist_alt)
                    if dist_alt_simp.complexity < my_complexity:
                        exprs[:] = [dist_alt_simp]
                        return True

    return mask_changed


def _custom_simplify_or(node: OrOperator, exprs: list[Expression]) -> bool:
    if not exprs:
        return False

    if node._try_distribute_or_over_and:
        my_complexity = node.node_complexity + sum(expr.complexity for expr in exprs)
        for i, expr in enumerate(exprs):
            match expr:
                case AndOperator(exprs=terms):
                    # Distribute OR over AND.
                    or_exprs = exprs[:i] + exprs[i + 1 :]
                    dist_alt = AndOperator(
                        *(OrOperator(term, *or_exprs) for term in terms)
                    )
                    dist_alt._try_distribute_and_over_or = False
                    dist_alt_simp = simplify_expression(dist_alt)
                    if dist_alt_simp.complexity < my_complexity:
                        exprs[:] = [dist_alt_simp]
                        return True

    return False


def _custom_simplify_xor(
    node: XorOperator,  # pylint: disable=unused-argument
    exprs: list[Expression],
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


def _custom_simplify_add(
    node: AddOperator,  # pylint: disable=unused-argument
    exprs: list[Expression],
) -> bool:
    changed = False

    # Remove pairs of A and -A.
    compl_idx = 0
    while compl_idx < len(exprs):
        compl = exprs[compl_idx]
        if not isinstance(compl, Complement):
            compl_idx += 1
            continue
        try:
            idx = exprs.index(compl.expr)
        except ValueError:
            compl_idx += 1
        else:
            del exprs[idx]
            if idx < compl_idx:
                compl_idx -= 1
            del exprs[compl_idx]
            changed = True

    return changed


_custom_simplifiers: dict[
    type[MultiExpression], Callable[[Any, list[Expression]], bool]
] = {
    AndOperator: _custom_simplify_and,
    OrOperator: _custom_simplify_or,
    XorOperator: _custom_simplify_xor,
    AddOperator: _custom_simplify_add,
}


def _simplify_complement(complement: Complement) -> Expression:
    match simplify_expression(complement.expr):
        case IntLiteral(value=value):
            return IntLiteral(-value)
        case Complement(expr=expr):
            return expr
        case AddOperator(exprs=terms):
            # Distribute complement over addition terms:
            #   -(x + y + z) = -x + -y + -z
            return simplify_expression(
                AddOperator(*(Complement(term) for term in terms))
            )
        case expr:
            if expr is complement.expr:
                return complement
            else:
                return Complement(expr)


def _test_bit(expr: Expression, bit: int) -> bool:
    """
    Returns True if the given bit of the given expression is certainly set,
    or False if it is unknown or certainly unset.
    """
    masked = _simplify_masked(expr, 1 << bit)
    return isinstance(masked, IntLiteral) and masked.value != 0


def _simplify_negation(negation: Negation) -> Expression:
    match simplify_expression(negation.expr):
        case IntLiteral(value=value):
            return IntLiteral(int(not value))
        case LShift(expr=expr) | Complement(expr=expr):
            return _simplify_negation(Negation(expr))
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
                if _test_bit(expr, bit):
                    return IntLiteral(0)
            bit += 1
    else:
        # The expression's value likely has leading ones, so pick an arbitrary
        # high bit position and test that.
        if _test_bit(expr, 1025):
            return IntLiteral(0)

    alt = None
    match expr:
        case AddOperator(exprs=terms) if all(term.mask >= 0 for term in terms):
            # If all terms are non-negative, one non-zero term will take the
            # result above zero.
            alt = simplify_expression(AndOperator(*(Negation(term) for term in terms)))
        case OrOperator(exprs=terms):
            # OR produces zero iff all of its terms are zero.
            alt = simplify_expression(AndOperator(*(Negation(term) for term in terms)))
        case RShift(expr=subExpr, offset=offset):
            match subExpr:
                case (
                    AndOperator(exprs=terms)
                    | OrOperator(exprs=terms)
                    | XorOperator(exprs=terms)
                ):
                    # Distribute RShift over bitwise operator.
                    alt = simplify_expression(
                        Negation(
                            subExpr.__class__(*(RShift(term, offset) for term in terms))
                        )
                    )
    if alt is not None and alt.complexity < negation.complexity:
        return alt
    else:
        return negation


def _simplify_sign_test(sign_test: SignTest) -> Expression:
    match simplify_expression(sign_test.expr):
        case Expression(mask=mask) if mask >= 0:
            # Negative values must have a negative mask.
            return IntLiteral(0)
        case IntLiteral(value=value):
            return IntLiteral(1 if value < 0 else 0)
        case SignExtension(expr=expr, width=width):
            return simplify_expression(opt_slice(expr, width - 1, 1))
        case expr:
            if expr is sign_test.expr:
                return sign_test
            else:
                return SignTest(expr)


def _simplify_sign_extension(sign_extend: SignExtension) -> Expression:
    width = sign_extend.width
    mask = mask_for_width(width)

    match _simplify_masked(simplify_expression(sign_extend.expr), mask):
        case IntLiteral(value=value) as expr:
            value &= mask
            value -= (value << 1) & (1 << width)
            return IntLiteral(value)
        case expr:
            pass

    # If the sign is known, we can replace the sign extension operator.
    if width != 0:
        sign_mask = 1 << (width - 1)
        match _simplify_masked(expr, sign_mask):
            case IntLiteral(value=value):
                non_sign = _simplify_masked(expr, mask & ~sign_mask)
                if value & sign_mask:
                    return simplify_expression(
                        OrOperator(non_sign, IntLiteral(-1 << (width - 1)))
                    )
                else:
                    return non_sign

    if expr is sign_extend.expr:
        return sign_extend
    else:
        return SignExtension(expr, width)


def _simplify_lshift(lshift: LShift) -> Expression:
    expr = simplify_expression(lshift.expr)

    offset = lshift.offset
    if offset == 0:
        # No actual shift occurs.
        return expr

    match expr:
        case IntLiteral(value=value):
            return IntLiteral(value << offset)
        case LShift(expr=expr, offset=loffset):
            # Combine both shifts into one.
            return simplify_expression(LShift(expr, offset + loffset))
        case RShift(expr=expr, mask=mask, offset=roffset):
            masked = AndOperator(expr, IntLiteral(mask << roffset))
            if roffset < offset:
                # Left shift wins.
                return simplify_expression(LShift(masked, offset - roffset))
            elif roffset == offset:
                # Left and right shift cancel each other out.
                return simplify_expression(masked)
            else:
                # Right shift wins.
                return simplify_expression(RShift(masked, roffset - offset))
        case AndOperator() | OrOperator():
            alt = simplify_expression(
                expr.__class__(*(LShift(term, offset) for term in expr.exprs))
            )
            if alt.complexity <= lshift.complexity:
                return alt

    if expr is lshift.expr:
        return lshift
    else:
        return LShift(expr, offset)


def _simplify_rshift(rshift: RShift) -> Expression:
    expr = simplify_expression(rshift.expr)

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
                return simplify_expression(RShift(expr.expr, offset - loffset))
            elif loffset == offset:
                # Left and right shift cancel each other out.
                return expr.expr
            else:
                # Left shift wins.
                return simplify_expression(LShift(expr.expr, loffset - offset))
        case RShift(expr=expr, offset=roffset):
            # Combine both shifts into one.
            return simplify_expression(RShift(expr, offset + roffset))
        case AndOperator() | OrOperator():
            alt = simplify_expression(
                expr.__class__(*(RShift(term, offset) for term in expr.exprs))
            )
            if alt.complexity < rshift.complexity:
                return alt

    if expr is rshift.expr:
        return rshift
    else:
        return RShift(expr, offset)


def _simplify_lvshift(lvshift: LVShift) -> Expression:
    match simplify_expression(lvshift.offset):
        case IntLiteral(value=value):
            # The offset is constant; convert to LShift.
            try:
                lshift = LShift(lvshift.expr, value)
            except ValueError as ex:
                return BadValue(f"bad left-shift: {ex}")
            else:
                return _simplify_lshift(lshift)
        case offset:
            pass

    # Note: Various other simplifications are possible, but I don't know which
    #       ones occur often enough in practice to be worth including.
    expr = simplify_expression(lvshift.expr)
    if expr is lvshift.expr and offset is lvshift.offset:
        return lvshift
    else:
        return LVShift(expr, offset)


def _simplify_rvshift(rvshift: RVShift) -> Expression:
    match simplify_expression(rvshift.offset):
        case IntLiteral(value=value):
            # The offset is constant; convert to RShift.
            try:
                rshift = RShift(rvshift.expr, value)
            except ValueError as ex:
                return BadValue(f"bad right-shift: {ex}")
            else:
                return _simplify_rshift(rshift)
        case offset:
            pass

    # Note: Various other simplifications are possible, but I don't know which
    #       ones occur often enough in practice to be worth including.
    expr = simplify_expression(rvshift.expr)
    if expr is rvshift.expr and offset is rvshift.offset:
        return rvshift
    else:
        return RVShift(expr, offset)


def _simplify_masked(expr: Expression, mask: int) -> Expression:
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
            masked_value = value & mask
            if value != masked_value:
                return IntLiteral(masked_value)
        case LShift(expr=subExpr, offset=offset):
            # Shift mask in the opposite direction.
            sub_masked = _simplify_masked(subExpr, mask >> offset)
            if sub_masked is not subExpr:
                return simplify_expression(LShift(sub_masked, offset))
        case RShift(expr=subExpr, offset=offset):
            # Shift mask in the opposite direction.
            sub_masked = _simplify_masked(subExpr, mask << offset)
            if sub_masked is not subExpr:
                return simplify_expression(RShift(sub_masked, offset))
        case AndOperator() | OrOperator() | XorOperator() | AddOperator():
            # Apply mask to each term.
            match expr:
                case AddOperator():
                    # Note: Only take truncation at the front into account.
                    #       While truncation could be done inside holes as well,
                    #       implementing that might not be worth the effort.
                    term_mask = mask_for_width(width_for_mask(mask))
                case _:
                    term_mask = mask
            terms = []
            changed = False
            for term in expr.exprs:
                masked_term = _simplify_masked(term, term_mask)
                terms.append(masked_term)
                changed |= masked_term is not term
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
                return simplify_expression(expr.__class__(*terms))
        case Complement(expr=subExpr):
            sub_masked = _simplify_masked(subExpr, mask_for_width(width_for_mask(mask)))
            if sub_masked is not subExpr:
                return simplify_expression(Complement(sub_masked))

    return expr


_simplifiers: dict[type[Expression], Callable[[Any], Expression]] = {
    AndOperator: _simplify_composed,
    OrOperator: _simplify_composed,
    XorOperator: _simplify_composed,
    AddOperator: _simplify_composed,
    Complement: _simplify_complement,
    Negation: _simplify_negation,
    SignTest: _simplify_sign_test,
    SignExtension: _simplify_sign_extension,
    LShift: _simplify_lshift,
    RShift: _simplify_rshift,
    LVShift: _simplify_lvshift,
    RVShift: _simplify_rvshift,
}


def simplify_expression(expr: Expression) -> Expression:
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
