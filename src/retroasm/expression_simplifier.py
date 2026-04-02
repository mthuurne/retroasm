from __future__ import annotations

from collections import defaultdict
from collections.abc import Callable, Iterable, Iterator
from typing import Any, Sequence, cast

from .expression import (
    _SHIFT_LIMIT_BITS,
    AddOperator,
    AndOperator,
    BadValue,
    Complement,
    CompositeExpression,
    Expression,
    IntLiteral,
    LShift,
    LVShift,
    MultiExpression,
    MultiplyOperator,
    OrOperator,
    RShift,
    RVShift,
    Select,
    SignExtension,
    SignTest,
    XorOperator,
    ZeroTest,
    opt_slice,
)
from .symbol import ImmediateValue, SymbolValue
from .types import is_power_of_two, mask_for_width, width_for_mask


def _is_var(expr: Expression) -> bool:
    """
    Is the given expression a mathematical variable?
    This is not directly related to local variable storages.
    Examples of mathematical variables are loaded values and symbol values.
    """
    return not isinstance(expr, (IntLiteral, CompositeExpression.__value__))


def _exclude_index[T](items: Iterable[T], exclude: int) -> Iterator[T]:
    """Yield items from the given iterable, in order, skipping over the given index."""
    for idx, item in enumerate(items):
        if idx != exclude:
            yield item


def _pick_alternative(alternatives: Iterable[Expression]) -> Expression:
    """
    Pick the simplest form among the base expression and the given alternatives.
    If there is a tie, prefer the alternative that occurs first.
    """
    simplest = None
    complexity: int | None = None
    for alt in alternatives:
        alt_complexity = alt.complexity
        if complexity is None or alt_complexity < complexity:
            simplest = alt
            complexity = alt_complexity
    if simplest is None:
        raise ValueError("no alternatives")
    return simplest


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
            # Use 'dict' rather than 'set' because it preserves insertion order,
            # which is necessary to reproduce golden test output.
            expr_dict: dict[Expression, None] = {}
            return expr_dict.setdefault, lambda: list(expr_dict)
        else:
            # Pick a collection that preserves duplicates.
            expr_list: list[Expression] = []
            return expr_list.append, lambda: expr_list

    subexprs = composed.exprs
    literal = multi_expr_cls.identity & term_mask
    while True:
        add_expr, get_exprs = init_collect()

        def append_subexpr(expr: Expression) -> None:
            if isinstance(expr, IntLiteral):
                nonlocal literal
                literal = multi_expr_cls.int_operator(literal, expr.value) & term_mask
            else:
                add_expr(expr)

        for expr in subexprs:
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
            # The term mask depends on the terms, so it is possible that simplifying a term
            # narrows the term mask which could enable further simplifications in other terms.
            new_term_mask = term_mask & AndOperator.compute_mask(exprs)
            if new_term_mask == term_mask:
                break
            term_mask = new_term_mask
            literal &= term_mask
        else:
            break

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
    elif multi_expr_cls is AndOperator:
        # TODO: We already computed the combined exprs mask.
        if literal_opt == AndOperator.compute_mask(exprs) & mask:
            literal_opt = None

    if literal_opt is not None:
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


def _find_booleans(exprs: Sequence[Expression]) -> Iterator[int]:
    for idx, expr in enumerate(exprs):
        if expr.mask == 1:
            yield idx


def _find_zero_tests(exprs: Sequence[Expression], booleans: Sequence[int]) -> Iterator[int]:
    for idx in booleans:
        expr = exprs[idx]
        if isinstance(expr, ZeroTest):
            yield idx


def _find_negated_terms(
    exprs: Sequence[Expression], booleans: Sequence[int], negations: Sequence[int]
) -> Iterator[tuple[int, int]]:
    for neg_idx in negations:
        negated = cast(ZeroTest, exprs[neg_idx]).expr
        for bool_idx in booleans:
            if neg_idx != bool_idx and exprs[bool_idx] == negated:
                yield neg_idx, bool_idx


def _decompose_zero_sum(terms: Sequence[Expression]) -> Iterator[tuple[Expression, Expression]]:
    """
    Decompose a given sum that equals zero into `(A, X)` pairs where `A` is a variable
    from `terms` and `X` is value of `A`.
    """
    for idx, term in enumerate(terms):
        match term:
            case Complement(expr=expr) if _is_var(expr):
                yield (expr, AddOperator(*_exclude_index(terms, idx)))
            case expr if _is_var(expr):
                yield (
                    expr,
                    simplify_expression(Complement(AddOperator(*_exclude_index(terms, idx)))),
                )


def _iter_equality_checks(negated: XorOperator) -> Iterator[tuple[Expression, Expression]]:
    """
    Decompose a negated XOR operator `!(A ^ X)` into `(A, X)` pairs where `A` is a variable.
    """
    negated_exprs = negated.exprs
    for idx, term in enumerate(negated_exprs):
        if _is_var(term):
            others = list(_exclude_index(negated_exprs, idx))
            equivalent = others[0] if len(others) == 1 else XorOperator(*others)
            yield term, equivalent
    if len(negated_exprs) == 2:
        expr1, expr2 = negated_exprs
        if isinstance(expr1, AddOperator) and isinstance(expr2, AddOperator):
            yield from _decompose_zero_sum(
                list(expr1.exprs) + [Complement(t) for t in expr2.exprs]
            )


def _find_equality_checks(
    exprs: Sequence[Expression], booleans: Sequence[int]
) -> Iterator[tuple[int, Expression, Expression]]:
    """
    Look for expressions that check whether a variable has a particular value.
    For each that we find, yield a triple of the expression index, the variable being checked
    and its equivalent value.
    """
    for idx in booleans:
        match exprs[idx]:
            case ZeroTest(expr=XorOperator() as negated):
                for var, equivalent in _iter_equality_checks(negated):
                    yield idx, var, equivalent
            case ZeroTest(expr=negated) if _is_var(negated):
                yield idx, negated, IntLiteral(0)
            case expr if _is_var(expr):
                yield idx, expr, IntLiteral(1)


def _custom_simplify_and(exprs: list[Expression], _applied_mask: int) -> None:
    booleans = list(_find_booleans(exprs))
    negations = list(_find_zero_tests(exprs, booleans))

    # If any Boolean is AND-ed with its negation, the end result is 0.
    for _ in _find_negated_terms(exprs, booleans, negations):
        exprs[:] = [IntLiteral(0)]
        return

    # If any Boolean is an equality check, replace an expression by the value it's equal to.
    # This doesn't change the AND expression's value because it the AND result is guaranteed
    # to be 0 if the equality check fails.
    alts = []
    for eq_idx, var, replacement in _find_equality_checks(exprs, booleans):
        # It seems counter-intuitive to replace a variable, which could be a low-complexity
        # expression, in the hope of lowering overall complexity. However, we're more likely
        # to get substitution matches on variables and substitution matches can enable further
        # simplifications.
        def replace(
            expr: Expression, var: Expression = var, replacement: Expression = replacement
        ) -> Expression | None:
            return replacement if expr == var else None

        # Require at least one term to be simplified by the substitution, to avoid infinite
        # recursion when the substitution can be done in both directions, like A == B.
        simplified = False
        terms = []
        for idx, expr in enumerate(exprs):
            if idx == eq_idx:
                terms.append(expr)
            else:
                new_expr = expr.substitute(replace)
                if new_expr is not expr:
                    new_expr = simplify_expression(new_expr, 1)
                    simplified |= new_expr.complexity < expr.complexity
                terms.append(new_expr)
        if simplified:
            alts.append(simplify_expression(AndOperator(*terms), 1))
    if alts:
        # Note: We don't check that the best alternative has a lower complexity score than
        #       the original expression. The assumption is that the elimination of a variable
        #       has more analysis potential, even if it doesn't pay off immediately.
        exprs[:] = [_pick_alternative(alts)]


def _custom_simplify_or(exprs: list[Expression], applied_mask: int) -> None:
    if not exprs:
        return

    if isinstance(literal := exprs[-1], IntLiteral):
        mask = applied_mask & ~literal.value
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

    # Replace Booleans OR-ed with their negation by "true" literal.
    booleans = list(_find_booleans(exprs))
    negations = list(_find_zero_tests(exprs, booleans))
    to_remove = set()
    for neg_idx, bool_idx in _find_negated_terms(exprs, booleans, negations):
        to_remove.add(neg_idx)
        to_remove.add(bool_idx)
    if to_remove:
        for idx in sorted(to_remove, reverse=True):
            del exprs[idx]
        exprs.append(IntLiteral(1))
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

    if not exprs:
        return
    if isinstance(exprs[-1], IntLiteral):
        terms = exprs[:-1]
        literal = exprs[-1].value
    else:
        terms = exprs
        literal = 0
    booleans = list(_find_booleans(terms))
    negations = list(_find_zero_tests(terms, booleans))

    # XOR-ing a Boolean with 1 is equivalent to a zero test.
    if len(terms) == 1 and terms[0].mask == 1 and literal == 1:
        # Introduce a zero test to replace the XOR.
        # Note: We could do a similar replacement when there are more than two terms,
        #       but as the XOR can't be dropped then, the complexity wouldn't decrease.
        exprs[:] = [simplify_expression(ZeroTest(*terms), applied_mask)]
        return
    # Strip zero tests of Booleans by adding/modifying the XOR literal.
    num_negated_bools = 0
    for idx in negations:
        term = cast(ZeroTest, terms[idx])
        if term.expr.mask == 1:
            terms[idx] = term.expr
            num_negated_bools += 1
    if num_negated_bools:
        literal ^= num_negated_bools % 2
        if literal != 0:
            terms.append(IntLiteral(literal))
        exprs[:] = [simplify_expression(XorOperator(*terms), applied_mask)]
        return

    # Note that because double negations and duplicate expression pairs were removed,
    # there can be no overlap between these pairs.
    to_remove = set()
    num_pairs = 0
    for neg_idx, bool_idx in _find_negated_terms(exprs, booleans, negations):
        to_remove.add(neg_idx)
        to_remove.add(bool_idx)
        num_pairs += 1
    assert len(to_remove) == 2 * num_pairs, exprs
    if num_pairs:
        for idx in sorted(to_remove, reverse=True):
            del exprs[idx]
        if num_pairs % 2:
            exprs.append(IntLiteral(1))
        exprs[:] = [simplify_expression(XorOperator(*exprs), applied_mask)]
        return

    # TODO: Distribution over AND and OR.


def _custom_simplify_add(exprs: list[Expression], applied_mask: int) -> None:
    # Count how often each term occurs.
    counts: dict[Expression, int] = defaultdict(int)
    for term in exprs:
        match term:
            case Complement(expr=expr):
                counts[expr] -= 1
            case MultiplyOperator(exprs=(*subs, IntLiteral(value=const))):
                expr = subs[0] if len(subs) == 1 else MultiplyOperator(*subs)
                counts[expr] += const
            case LShift(expr=expr, offset=offset) if offset < _SHIFT_LIMIT_BITS:
                counts[expr] += 1 << offset
            case expr:
                counts[expr] += 1
    # Note that items with a value of 0 are included in len(counts), but a value of 0 will
    # only happen when more than one term affected that key, so the length still shrinks.
    if len(counts) < len(exprs):
        # Construct a new sum with fewer terms.
        exprs[:] = [
            simplify_expression(
                AddOperator(
                    *(
                        MultiplyOperator(expr, IntLiteral(count))
                        for expr, count in counts.items()
                    )
                ),
                applied_mask,
            )
        ]
        return


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


def _simplify_negation(negation: ZeroTest, mask: int) -> Expression:
    if mask & 1 == 0:
        return IntLiteral(0)

    subexpr = simplify_expression(negation.expr)
    match subexpr:
        case IntLiteral(value=value):
            return IntLiteral(int(not value))
        case LShift(expr=expr) | Complement(expr=expr):
            return _simplify_negation(ZeroTest(expr), 1)
        case ZeroTest(expr=expr) if expr.mask == 1:
            return expr
        case XorOperator() as negated:
            for var, equivalent in _iter_equality_checks(negated):
                match equivalent:
                    case AddOperator(exprs=terms):
                        try:
                            idx = terms.index(var)
                        except ValueError:
                            pass
                        else:
                            # An expression is equal to a sum containing itself iff the other
                            # terms of the sum add up to zero.
                            return simplify_expression(
                                ZeroTest(AddOperator(*_exclude_index(terms, idx)))
                            )
        case MultiplyOperator(exprs=terms):
            return simplify_expression(OrOperator(*(ZeroTest(term) for term in terms)), 1)

    # If any bit of the expression's value is 1, the value is non-zero.
    if (negated_mask := subexpr.mask) >= 0:
        # Test each individual bit position that could possibly be set.
        bit = 0
        while negated_mask >> bit:
            if (negated_mask >> bit) & 1:
                if _test_bit(subexpr, bit):
                    return IntLiteral(0)
            bit += 1
    else:
        # The expression's value likely has leading ones, so pick an arbitrary
        # high bit position and test that.
        if _test_bit(subexpr, 1025):
            return IntLiteral(0)

    alts: list[Expression] = [negation if subexpr is negation.expr else ZeroTest(subexpr)]
    match subexpr:
        case AddOperator(exprs=terms) if all(term.mask >= 0 for term in terms):
            # If all terms are non-negative, one non-zero term will take the
            # result above zero.
            alts.append(simplify_expression(ZeroTest(OrOperator(*terms))))
        case AddOperator(exprs=terms):
            # Simplify !(A - B) to !(A ^ B).
            for idx, term in enumerate(terms):
                alts.insert(
                    0,
                    simplify_expression(
                        ZeroTest(
                            XorOperator(
                                Complement(term),
                                AddOperator(*terms[:idx], *terms[idx + 1 :]),
                            )
                        )
                    ),
                )
        case OrOperator(exprs=terms):
            # OR produces zero iff all of its terms are zero.
            alts.append(simplify_expression(AndOperator(*(ZeroTest(term) for term in terms))))
        case RShift(expr=subexpr, offset=offset):
            match subexpr:
                case (
                    AndOperator(exprs=terms)
                    | OrOperator(exprs=terms)
                    | XorOperator(exprs=terms)
                ):
                    # Distribute RShift over bitwise operator.
                    alts.append(
                        simplify_expression(
                            ZeroTest(
                                subexpr.__class__(*(RShift(term, offset) for term in terms))
                            )
                        )
                    )

    return _pick_alternative(alts)


def _simplify_sign_test(sign_test: SignTest, mask: int) -> Expression:
    if mask & 1 == 0:
        return IntLiteral(0)

    subexpr = simplify_expression(sign_test.expr)

    alts: list[Expression] = [sign_test if subexpr is sign_test.expr else SignTest(subexpr)]
    match subexpr:
        case Expression(mask=expr_mask) if expr_mask >= 0:
            # Negative values must have a negative mask.
            return IntLiteral(0)
        case IntLiteral(value=value):
            return IntLiteral(1 if value < 0 else 0)
        case SignExtension(expr=expr, width=width):
            return simplify_expression(opt_slice(expr, width - 1, 1))
        case Complement(expr=expr):
            # Simplify using: (-X < 0) = (X != 0 && !(X < 0)).
            # While this doesn't seem simpler at first glance, it can be reduced quite a bit
            # when X is an unsigned value.
            alts.append(
                simplify_expression(
                    AndOperator(ZeroTest(ZeroTest(expr)), ZeroTest(SignTest(expr)))
                )
            )
        case AddOperator(exprs=(*terms, IntLiteral(value=literal))):
            # Note that the case where the overall mask is non-negative has already been
            # dealt with, so the terms mask is negative or the literal is, or both.
            terms_sum = simplify_expression(AddOperator(*terms))
            if (terms_mask := terms_sum.mask) >= 0:
                # The terms total cannot exceed its mask.
                if terms_mask + literal < 0:
                    return IntLiteral(1)
                if terms_mask + literal == 0:
                    alts.insert(0, simplify_expression(ZeroTest(ZeroTest(subexpr))))
            else:
                compl = simplify_expression(Complement(terms_sum))
                if (compl_mask := compl.mask) >= 0:
                    if literal < 0:
                        return IntLiteral(1)
                    if literal >= -compl_mask:
                        return IntLiteral(0)

    return _pick_alternative(alts)


def _simplify_sign_extension(sign_extend: SignExtension, mask: int) -> Expression:
    width = sign_extend.width
    if width == 0:
        # If the width is 0, there is no sign bit.
        return IntLiteral(0)
    if width_for_mask(mask) <= width:
        # The extended sign will be cut off later, so drop the sign extension altogether.
        return simplify_expression(sign_extend.expr, mask)

    # Some bits of the extended sign matter, collapse those onto the sign bit.
    sign_index = width - 1
    sign_mask = 1 << sign_index
    sub_mask = (mask & mask_for_width(width)) | sign_mask

    match simplify_expression(sign_extend.expr, sub_mask):
        case IntLiteral(value=value) as expr:
            value &= sub_mask
            value -= (value << 1) & (1 << width)
            return IntLiteral(value)
        case ImmediateValue(type=typ) as imm if typ.signed and typ.width <= width:
            # Besides efficiency, this simplification is also enables easy detection of
            # value placeholders without an assigned value.
            return imm
        case LShift(expr=shifted, offset=offset):
            # If offset >= width, the result would have been literal 0.
            assert offset < width, sign_extend
            return simplify_expression(
                LShift(SignExtension(shifted, width - offset), offset), mask
            )
        case expr:
            pass

    if sub_mask == sign_mask:
        # Only the sign bit determines the output.
        # Since 'expr' is not an IntLiteral, the sign is unknown in this case.
        if mask > 0 and mask.bit_count() == 1:
            result_index = mask.bit_length() - 1
            # The case where 'mask' doesn't include an extended sign bit was already handled.
            assert result_index > sign_index, (sign_extend, mask)
            return simplify_expression(LShift(expr, result_index - sign_index), mask)
    else:
        # If the sign is known, we can replace the sign extension operator.
        match simplify_expression(expr, sign_mask):
            case IntLiteral(value=value):
                non_sign = simplify_expression(expr, sub_mask & ~sign_mask)
                if value & sign_mask:
                    return simplify_expression(
                        OrOperator(non_sign, IntLiteral(-1 << sign_index))
                    )
                else:
                    return non_sign

    if expr is sign_extend.expr:
        return sign_extend
    else:
        return SignExtension(expr, width)


def _simplify_lshift(lshift: LShift, mask: int) -> Expression:
    offset = lshift.offset
    subexpr = simplify_expression(lshift.expr, mask >> offset)

    if offset == 0:
        # No actual shift occurs.
        return subexpr

    alts: list[Expression] = [lshift if subexpr is lshift.expr else LShift(subexpr, offset)]
    match subexpr:
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
        case AndOperator(exprs=terms) | OrOperator(exprs=terms):
            # Prefer having the shifts deeper in the tree.
            alts.insert(
                0,
                simplify_expression(
                    subexpr.__class__(*(LShift(term, offset) for term in terms)), mask
                ),
            )
        case MultiplyOperator(exprs=(*terms, IntLiteral(value=literal))):
            # Merge shift into multiplication literal.
            return simplify_expression(
                MultiplyOperator(*terms, IntLiteral(literal << offset)), mask
            )

    return _pick_alternative(alts)


def _simplify_rshift(rshift: RShift, mask: int) -> Expression:
    offset = rshift.offset
    subexpr = simplify_expression(rshift.expr, mask << offset)

    if offset == 0:
        # No actual shift occurs.
        return subexpr

    alts: list[Expression] = [rshift if subexpr is rshift.expr else RShift(subexpr, offset)]
    match subexpr:
        case IntLiteral(value=value):
            return IntLiteral(value >> offset)
        case LShift(expr=expr, offset=loffset):
            if loffset < offset:
                # Right shift wins.
                return simplify_expression(RShift(expr, offset - loffset), mask)
            elif loffset == offset:
                # Left and right shift cancel each other out.
                return expr
            else:
                # Left shift wins.
                return simplify_expression(LShift(expr, loffset - offset), mask)
        case RShift(expr=expr, offset=roffset):
            # Combine both shifts into one.
            return simplify_expression(RShift(expr, offset + roffset), mask)
        case AndOperator(exprs=terms) | OrOperator(exprs=terms):
            # Prefer having the shifts deeper in the tree.
            alts.insert(
                0,
                simplify_expression(
                    subexpr.__class__(*(RShift(term, offset) for term in terms)), mask
                ),
            )
        case SignExtension(expr=expr, width=width) if offset >= width:
            # Only sign bits remain after shift. Pick the minimal offset where this happens,
            # to make equivalent expressions identical.
            return simplify_expression(RShift(SignExtension(expr, width), width - 1), mask)

    return _pick_alternative(alts)


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


def _simplify_select(select: Select, mask: int) -> Expression:
    exprs = tuple(simplify_expression(expr, mask) for expr in select.exprs)

    if len(exprs) == 1:
        # There is only one option.
        (expr,) = exprs
        return expr

    expr = exprs[0]
    if all(e2 == expr for e2 in exprs[1:]):
        # All options are equal.
        return expr

    if all(e1 is e2 for e1, e2 in zip(exprs, select.exprs, strict=True)):
        return select
    else:
        return Select(*exprs)


_simplifiers: dict[type[Expression], Callable[[Any, int], Expression]] = {
    IntLiteral: _simplify_int_literal,
    AndOperator: _simplify_composed,
    OrOperator: _simplify_composed,
    XorOperator: _simplify_composed,
    AddOperator: _simplify_composed,
    MultiplyOperator: _simplify_composed,
    Complement: _simplify_complement,
    ZeroTest: _simplify_negation,
    SignTest: _simplify_sign_test,
    SignExtension: _simplify_sign_extension,
    LShift: _simplify_lshift,
    RShift: _simplify_rshift,
    LVShift: _simplify_lvshift,
    RVShift: _simplify_rvshift,
    Select: _simplify_select,
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
        return IntLiteral(0)

    simplifier = _simplifiers.get(type(expr))
    if simplifier is None:
        return expr
    else:
        return simplifier(expr, mask)
