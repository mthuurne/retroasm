from __future__ import annotations

from collections.abc import Sequence
from dataclasses import dataclass

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
class ExprSimplifyTester:
    equations: Sequence[tuple[Expression, Expression]]

    def check(self) -> None:
        for complex_expr, expected_expr in self.equations:
            assert simplify_expression(complex_expr) == expected_expr


@pytest.fixture
def docstring_tester(
    docstring: str,
    request: pytest.FixtureRequest,
) -> ExprSimplifyTester:
    """
    Fixture that parses a series of equations from the requesting test function's
    docstring.
    """

    try:
        specs = []
        for block in unpack_docstring(docstring, opt="expr"):
            for line in block.split("\n"):
                if line:
                    try:
                        equals_idx = line.index(" = ")
                    except ValueError:
                        raise BadInput("no equality operator found")
                    complex_expr = expression_from_string(line[:equals_idx])
                    expected_expr = expression_from_string(line[equals_idx + 3 :])
                    specs.append((complex_expr, expected_expr))
        if specs:
            return ExprSimplifyTester(specs)
        else:
            raise BadInput("no expressions")
    except BadInput as ex:
        raise pytest.FixtureLookupError(
            request.fixturename, request, f"error in docstring: {ex}"
        ) from ex
