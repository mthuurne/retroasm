from __future__ import annotations

from pytest import raises

from retroasm.parser.expression_parser import ExprToken, ExprTokenizer
from retroasm.parser.tokens import Tokenizer


def test_token_class_unspecialized() -> None:
    with raises(
        TypeError,
        match=r"^Tokenizer must be specialized first, "
        r"for example Tokenizer\[MyTokenEnum\]$",
    ):
        print(Tokenizer.get_token_class())


def test_token_class_specialized() -> None:
    assert Tokenizer[ExprToken].get_token_class() is ExprToken


def test_token_class_subclass() -> None:
    assert ExprTokenizer.get_token_class() is ExprToken
