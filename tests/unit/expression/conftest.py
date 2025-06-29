from __future__ import annotations

from dataclasses import dataclass
from inspect import cleandoc

import pytest

from retroasm.codeblock_builder import SemanticsCodeBlockBuilder
from retroasm.expression import Expression
from retroasm.expression_simplifier import simplify_expression
from retroasm.input import BadInput, InputLocation
from retroasm.namespace import LocalNamespace
from retroasm.parser.expression_builder import build_expression
from retroasm.parser.expression_parser import parse_expr
from retroasm.reference import symbol_reference
from retroasm.types import IntType

from ..docstring import unpack_docstring


def expression_from_string(text: str) -> Expression:
    location = InputLocation.from_string(text)
    node = parse_expr(location)
    builder = SemanticsCodeBlockBuilder()
    namespace = LocalNamespace(None)
    namespace.define("A", symbol_reference("A", IntType.int))
    namespace.define("B", symbol_reference("B", IntType.int))
    namespace.define("F", symbol_reference("F", IntType.u(1)))
    namespace.define("H", symbol_reference("H", IntType.u(8)))
    namespace.define("L", symbol_reference("L", IntType.u(8)))
    namespace.define("Z", symbol_reference("Z", IntType.u(0)))
    return build_expression(node, namespace, builder)


@dataclass
class Equation:
    lhs: Expression
    rhs: Expression

    def check_simplify(self) -> None:
        """
        Assert that the left hand side and right hand side simplify
        to the same expression.
        """
        lsimp = simplify_expression(self.lhs)
        rsimp = simplify_expression(self.rhs)
        assert str(lsimp) == str(rsimp)
        assert lsimp == rsimp


@pytest.fixture
def expression(request: pytest.FixtureRequest) -> Expression:
    """Fixture that parses an expression string passed as a parameter."""

    expr_str: str = request.param
    try:
        return expression_from_string(expr_str)
    except BadInput as ex:
        raise pytest.FixtureLookupError(
            request.fixturename, request, f"error in expression: {ex}"
        ) from ex


@pytest.fixture
def equation(request: pytest.FixtureRequest) -> Equation:
    """Fixture that parses an equation string passed as a parameter."""

    equation: str = request.param
    try:
        try:
            equals_idx = equation.index(" = ")
        except ValueError:
            raise BadInput("no equality operator found")
        complex_expr = expression_from_string(equation[:equals_idx])
        expected_expr = expression_from_string(equation[equals_idx + 3 :])
        return Equation(complex_expr, expected_expr)
    except BadInput as ex:
        raise pytest.FixtureLookupError(
            request.fixturename, request, f"error in equation: {ex}"
        ) from ex


def pytest_generate_tests(metafunc: pytest.Metafunc) -> None:
    # Note that the fixtures are mutually exclusive in practice,
    # so we don't do double work here.
    for name in ("expression", "equation"):
        if name not in metafunc.fixturenames:
            continue
        docstring = cleandoc(metafunc.function.__doc__ or "")
        try:
            lines = [
                line
                for block in unpack_docstring(docstring, opt="expr")
                for line in block.split("\n")
                if line
            ]
        except ValueError as ex:
            raise pytest.Collector.CollectError(f"Error unpacking docstring: {ex}")
        metafunc.parametrize(name, lines, indirect=True)
