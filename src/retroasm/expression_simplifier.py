from __future__ import annotations

from collections.abc import Callable
from typing import Any

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
    MultiplyOperator,
    Negation,
    OrOperator,
    RShift,
    RVShift,
    SignExtension,
    SignTest,
    XorOperator,
    opt_slice,
)
from .symbol import ImmediateValue, SymbolValue
from .types import is_power_of_two, mask_for_width, width_for_mask


def _simplify_int_literal(literal: IntLiteral, mask: int) -> IntLiteral:
    # Apply mask to value.
    value = literal.value
    masked_value = value & mask
    if (masked_value := value & mask) != value:
        return IntLiteral(masked_value)
    return literal


def _simplify_composed(composed: MultiExpression, mask: int) -> Expression:
    multi_expr_cls = type(composed)

    match composed:
        case AndOperator():
            # We don't care about bits that will be reset by other terms.
            # However, we do have to be careful not to remove the resetting of a particular
            # bit altogether by assuming for every term that another term will take care of
            # the reset. We will ensure that when computing the literal value later.
            term_mask = mask & composed.mask
        case OrOperator() | XorOperator():
            term_mask = mask
        case AddOperator() | MultiplyOperator():
            term_mask = mask_for_width(width_for_mask(mask))
        case _:
            assert False, composed

    def init_collect() -> tuple[Callable[[Expression], None], Callable[[], list[Expression]]]:
        if multi_expr_cls.idempotent:
            # Pick a collection that filters out duplicates.
            expr_set: set[Expression] = set()
            return expr_set.add, lambda: list(expr_set)
        else:
            # Pick a collection that preserves duplicates.
            expr_list: list[Expression] = []
            return expr_list.append, lambda: expr_list

    add_expr, get_exprs = init_collect()
    literal = multi_expr_cls.identity & term_mask

    def append_subexpr(expr: Expression) -> None:
        if isinstance(expr, IntLiteral):
            nonlocal literal
            literal = multi_expr_cls.int_operator(literal, expr.value) & term_mask
        else:
            add_expr(expr)

    for expr in composed.exprs:
        # Simplify the subexpression individually.
        simplified = simplify_expression(expr, term_mask)

        if isinstance(simplified, multi_expr_cls):
            # Merge subexpressions of the same type into this expression.
            for subexpr in simplified.exprs:
                append_subexpr(subexpr)
        else:
            append_subexpr(simplified)
    exprs = get_exprs()
    if multi_expr_cls is AndOperator:
        # TODO: In the case of AND, the term mask depends on the terms, so it is possible
        #       that simplifying a term narrows the term mask which could enable further
        #       simplifications in other terms. However, none of the current test cases
        #       require repeating this simplification step for non-literal terms.
        term_mask &= AndOperator.compute_mask(exprs)
        literal &= term_mask

    # Make the order of terms somewhat predictable.
    # This does not produce canonical expressions, but it should be enough to
    # at least compare expressions while unit testing.
    exprs.sort(key=lambda expr: expr.name if isinstance(expr, SymbolValue) else "")
    exprs.sort(key=lambda expr: -expr.complexity)
    exprs.sort(key=lambda expr: -expr.offset if isinstance(expr, LShift) else 0)

    # Handle special literal cases.
    literal_opt: int | None = literal
    if multi_expr_cls.absorber is not None and literal == multi_expr_cls.absorber & term_mask:
        # Every other term will be absorbed by the literal.
        exprs = []
    if multi_expr_cls.identity is not None and literal == multi_expr_cls.identity & mask:
        # Omit identity literal.
        literal_opt = None
    if multi_expr_cls is AndOperator:
        # TODO: We already computed this mask.
        if literal_opt == AndOperator.compute_mask(exprs):
            literal_opt = None

    if literal_opt is not None:
        if (
            composed.exprs
            and isinstance(last := composed.exprs[-1], IntLiteral)
            and last.value == literal_opt
        ):
            # Preserve the instance.
            # TODO: Maybe IntLiteral should have unique instances.
            exprs.append(last)
        else:
            exprs.append(IntLiteral(literal_opt))

    # Perform simplifications specific to this operator.
    _custom_simplifiers[multi_expr_cls](exprs, mask)

    match len(exprs):
        case 0:
            return IntLiteral(multi_expr_cls.identity)
        case 1:
            return exprs[0]
        case num_exprs:
            if num_exprs == len(composed.exprs) and all(
                new_expr is old_expr for new_expr, old_expr in zip(exprs, composed.exprs)
            ):
                return composed
            else:
                return multi_expr_cls(*exprs)


def _custom_simplify_and(exprs: list[Expression], applied_mask: int) -> None:
    # TODO: The code to drop a redundant literal could go here, but it seems easier
    #       to have it in _simplify_composed() where the literal is already separated.
    return


def _custom_simplify_or(exprs: list[Expression], applied_mask: int) -> None:
    if not exprs:
        return

    if isinstance(literal := exprs[-1], IntLiteral):
        mask = OrOperator.compute_mask(exprs[:-1]) & ~literal.value
        # Try to simplify individual subexpressions by applying bit mask.
        changed = False
        for i, expr in enumerate(exprs[:-1]):
            masked = simplify_expression(expr, mask)
            if masked is not expr:
                exprs[i] = masked
                changed = True

        # If subexpressions were simplified, force earlier steps to run again.
        if changed:
            exprs[:] = [simplify_expression(OrOperator(*exprs), applied_mask)]
            return


def _custom_simplify_xor(exprs: list[Expression], applied_mask: int) -> None:
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

    # TODO: Distribution over AND and OR.


def _custom_simplify_add(exprs: list[Expression], applied_mask: int) -> None:
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


def _custom_simplify_multiply_complement(exprs: list[Expression], applied_mask: int) -> None:
    # Strip and count (even/odd) complements.
    complement = False
    for idx, expr in enumerate(exprs):
        if isinstance(expr, Complement):
            exprs[idx] = expr.expr
            complement = not complement
    if exprs and isinstance(exprs[-1], IntLiteral):
        # Incorporate complement into literal.
        value = exprs[-1].value
        if value == -1:
            complement = not complement
        elif value != 1:
            if complement:
                exprs[-1] = IntLiteral(-value)
            return
        # Drop literal.
        del exprs[-1]
    # Implicit literal: 1 or -1.
    if complement:
        exprs[:] = [simplify_expression(Complement(MultiplyOperator(*exprs)), applied_mask)]


def _custom_simplify_multiply_shift(exprs: list[Expression], applied_mask: int) -> None:
    if len(exprs) < 2:
        return

    # Convert term shifts into a multiplication factor.
    factor = 1
    for idx, expr in enumerate(exprs):
        if isinstance(expr, LShift):
            factor *= 1 << expr.offset
            exprs[idx] = expr.expr
    literal_value = exprs[-1].value if isinstance(exprs[-1], IntLiteral) else 1
    factor *= literal_value
    if factor == 1:
        return

    # Convert multiplication by a power of two into a left shift.
    if is_power_of_two(factor):
        offset = factor.bit_length() - 1
        if literal_value != 1:
            del exprs[-1]
        exprs[:] = [simplify_expression(LShift(MultiplyOperator(*exprs), offset), applied_mask)]
        return

    # Update literal.
    if factor != literal_value:
        if literal_value == 1:
            exprs.append(IntLiteral(factor))
        else:
            exprs[-1] = IntLiteral(factor)


def _custom_simplify_multiply(exprs: list[Expression], applied_mask: int) -> None:
    _custom_simplify_multiply_complement(exprs, applied_mask)
    _custom_simplify_multiply_shift(exprs, applied_mask)


_custom_simplifiers: dict[type[MultiExpression], Callable[[list[Expression], int], None]] = {
    AndOperator: _custom_simplify_and,
    OrOperator: _custom_simplify_or,
    XorOperator: _custom_simplify_xor,
    AddOperator: _custom_simplify_add,
    MultiplyOperator: _custom_simplify_multiply,
}


def _simplify_complement(complement: Complement, mask: int) -> Expression:
    match simplify_expression(complement.expr, mask_for_width(width_for_mask(mask))):
        case IntLiteral(value=value):
            return IntLiteral(-value & mask)
        case Complement(expr=expr):
            return expr
        case AddOperator(exprs=terms):
            # Distribute complement over addition terms:
            #   -(x + y + z) = -x + -y + -z
            return simplify_expression(AddOperator(*(Complement(term) for term in terms)), mask)
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
    masked = simplify_expression(expr, 1 << bit)
    return isinstance(masked, IntLiteral) and masked.value != 0


def _simplify_negation(negation: Negation, mask: int) -> Expression:
    if mask & 1 == 0:
        return IntLiteral(0)

    match simplify_expression(negation.expr):
        case IntLiteral(value=value):
            return IntLiteral(int(not value))
        case LShift(expr=expr) | Complement(expr=expr):
            return _simplify_negation(Negation(expr), 1)
        case Negation(expr=expr) if expr.mask == 1:
            return expr
        case expr:
            if expr is not negation.expr:
                negation = Negation(expr)

    # If any bit of the expression's value is 1, the value is non-zero.
    if (negated_mask := expr.mask) >= 0:
        # Test each individual bit position that could possibly be set.
        bit = 0
        while negated_mask >> bit:
            if (negated_mask >> bit) & 1:
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
                        Negation(subExpr.__class__(*(RShift(term, offset) for term in terms)))
                    )
    if alt is not None and alt.complexity < negation.complexity:
        return alt
    else:
        return negation


def _simplify_sign_test(sign_test: SignTest, mask: int) -> Expression:
    if mask & 1 == 0:
        return IntLiteral(0)

    match simplify_expression(sign_test.expr):
        case Expression(mask=expr_mask) if expr_mask >= 0:
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


def _simplify_sign_extension(sign_extend: SignExtension, mask: int) -> Expression:
    width = sign_extend.width
    mask &= mask_for_width(width)

    match simplify_expression(sign_extend.expr, mask):
        case IntLiteral(value=value) as expr:
            value &= mask
            value -= (value << 1) & (1 << width)
            return IntLiteral(value)
        case ImmediateValue(type=typ) as imm if typ.signed and typ.width <= width:
            # Besides efficiency, this simplification is also enables easy detection of
            # value placeholders without an assigned value.
            return imm
        case expr:
            pass

    # If the sign is known, we can replace the sign extension operator.
    # Note: width cannot be 0 because then the subexpression would simplify to a literal 0.
    sign_mask = 1 << (width - 1)
    match simplify_expression(expr, sign_mask):
        case IntLiteral(value=value):
            non_sign = simplify_expression(expr, mask & ~sign_mask)
            if value & sign_mask:
                return simplify_expression(OrOperator(non_sign, IntLiteral(-1 << (width - 1))))
            else:
                return non_sign

    if expr is sign_extend.expr:
        return sign_extend
    else:
        return SignExtension(expr, width)


def _simplify_lshift(lshift: LShift, mask: int) -> Expression:
    offset = lshift.offset
    expr = simplify_expression(lshift.expr, mask >> offset)

    if offset == 0:
        # No actual shift occurs.
        return expr

    match expr:
        case IntLiteral(value=value):
            return IntLiteral(value << offset)
        case LShift(expr=expr, offset=loffset):
            # Combine both shifts into one.
            return simplify_expression(LShift(expr, offset + loffset), mask)
        case RShift(expr=expr, mask=mask, offset=roffset):
            masked = AndOperator(expr, IntLiteral(mask << roffset))
            if roffset < offset:
                # Left shift wins.
                return simplify_expression(LShift(masked, offset - roffset), mask)
            elif roffset == offset:
                # Left and right shift cancel each other out.
                return simplify_expression(masked, mask)
            else:
                # Right shift wins.
                return simplify_expression(RShift(masked, roffset - offset), mask)
        case AndOperator() | OrOperator():
            alt = simplify_expression(
                expr.__class__(*(LShift(term, offset) for term in expr.exprs)), mask
            )
            if alt.complexity <= lshift.complexity:
                return alt
        case MultiplyOperator(exprs=exprs):
            if isinstance(exprs[-1], IntLiteral):
                # Merge shift into multiplication literal.
                return simplify_expression(
                    MultiplyOperator(
                        *(tuple(exprs[:-1]) + (IntLiteral(exprs[-1].value << offset),))
                    ),
                    mask,
                )

    if expr is lshift.expr:
        return lshift
    else:
        return LShift(expr, offset)


def _simplify_rshift(rshift: RShift, mask: int) -> Expression:
    offset = rshift.offset
    expr = simplify_expression(rshift.expr, mask << offset)

    if offset == 0:
        # No actual shift occurs.
        return expr

    match expr:
        case IntLiteral(value=value):
            return IntLiteral(value >> offset)
        case LShift(offset=loffset):
            if loffset < offset:
                # Right shift wins.
                return simplify_expression(RShift(expr.expr, offset - loffset), mask)
            elif loffset == offset:
                # Left and right shift cancel each other out.
                return expr.expr
            else:
                # Left shift wins.
                return simplify_expression(LShift(expr.expr, loffset - offset), mask)
        case RShift(expr=expr, offset=roffset):
            # Combine both shifts into one.
            return simplify_expression(RShift(expr, offset + roffset), mask)
        case AndOperator() | OrOperator():
            alt = simplify_expression(
                expr.__class__(*(RShift(term, offset) for term in expr.exprs)), mask
            )
            if alt.complexity < rshift.complexity:
                return alt

    if expr is rshift.expr:
        return rshift
    else:
        return RShift(expr, offset)


def _simplify_lvshift(lvshift: LVShift, mask: int) -> Expression:
    match simplify_expression(lvshift.offset):
        case IntLiteral(value=value):
            # The offset is constant; convert to LShift.
            try:
                lshift = LShift(lvshift.expr, value)
            except ValueError as ex:
                return BadValue(f"bad left-shift: {ex}")
            else:
                return _simplify_lshift(lshift, mask)
        case offset:
            pass

    # Note: Various other simplifications are possible, but I don't know which
    #       ones occur often enough in practice to be worth including.
    expr = simplify_expression(lvshift.expr)
    if expr is lvshift.expr and offset is lvshift.offset:
        return lvshift
    else:
        return LVShift(expr, offset)


def _simplify_rvshift(rvshift: RVShift, mask: int) -> Expression:
    match simplify_expression(rvshift.offset):
        case IntLiteral(value=value):
            # The offset is constant; convert to RShift.
            try:
                rshift = RShift(rvshift.expr, value)
            except ValueError as ex:
                return BadValue(f"bad right-shift: {ex}")
            else:
                return _simplify_rshift(rshift, mask)
        case offset:
            pass

    # Note: Various other simplifications are possible, but I don't know which
    #       ones occur often enough in practice to be worth including.
    expr = simplify_expression(rvshift.expr)
    if expr is rvshift.expr and offset is rvshift.offset:
        return rvshift
    else:
        return RVShift(expr, offset)


_simplifiers: dict[type[Expression], Callable[[Any, int], Expression]] = {
    IntLiteral: _simplify_int_literal,
    AndOperator: _simplify_composed,
    OrOperator: _simplify_composed,
    XorOperator: _simplify_composed,
    AddOperator: _simplify_composed,
    MultiplyOperator: _simplify_composed,
    Complement: _simplify_complement,
    Negation: _simplify_negation,
    SignTest: _simplify_sign_test,
    SignExtension: _simplify_sign_extension,
    LShift: _simplify_lshift,
    RShift: _simplify_rshift,
    LVShift: _simplify_lvshift,
    RVShift: _simplify_rvshift,
}


def simplify_expression(expr: Expression, mask: int = -1) -> Expression:
    """
    Return an equivalent expression that is simpler (fewer nodes),
    or the given expression object itself if no simplification was found.
    If a mask is given, only bits that are set in the mask are considered relevant:
    the simplified expression can have other values for other bits.
    """
    if mask & expr.mask == 0:
        # The only value that matches a 0 mask is 0.
        match expr:
            case IntLiteral(value=0):
                return expr
            case _:
                return IntLiteral(0)

    simplifier = _simplifiers.get(type(expr))
    if simplifier is None:
        return expr
    else:
        return simplifier(expr, mask)
