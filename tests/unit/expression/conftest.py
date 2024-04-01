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

from ..docstring import unpack_docstring


def expression_from_string(text: str) -> Expression:
    location = InputLocation.from_string(text)
    node = parse_expr(location)
    builder = SemanticsCodeBlockBuilder()
    namespace = LocalNamespace(None, builder)
    return build_expression(node, namespace)


@dataclass
class Equation:
    lhs: Expression
    rhs: Expression

    def check_simplify(self) -> None:
        """Assert that the left hand side simplifies to the right hand side."""
        assert simplify_expression(self.lhs) == self.rhs


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
    if "equation" in metafunc.fixturenames:
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
        metafunc.parametrize("equation", lines, indirect=True)
