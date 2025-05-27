from __future__ import annotations

from pytest import raises

from retroasm.input import BadInput, InputLocation
from retroasm.parser.tokens import TokenEnum, Tokenizer


class TestToken(TokenEnum):
    identifier = r"[A-Za-z_][A-Za-z0-9_]*"
    number = r"[0-9]+"


class TestTokenizer(Tokenizer[TestToken]):
    pass


def test_token_class_unspecialized() -> None:
    with raises(
        TypeError,
        match=r"^Tokenizer must be specialized first, "
        r"for example Tokenizer\[MyTokenEnum\]$",
    ):
        print(Tokenizer.get_token_class())


def test_token_class_specialized() -> None:
    assert Tokenizer[TestToken].get_token_class() is TestToken


def test_token_class_subclass() -> None:
    assert TestTokenizer.get_token_class() is TestToken


def test_tokenize_simple() -> None:
    tokens = TestTokenizer.scan(InputLocation.from_string("one 2 thr33"))
    assert [(typ, loc.text) for typ, loc in tokens] == [
        (TestToken.identifier, "one"),
        (TestToken.number, "2"),
        (TestToken.identifier, "thr33"),
    ]


def test_tokenize_unmatched() -> None:
    with raises(
        BadInput,
        match=r"^invalid token: \+$",
    ):
        TestTokenizer.scan(InputLocation.from_string("one + 2"))


def test_tokenizer_concat() -> None:
    tokens1 = TestTokenizer.scan(InputLocation.from_string("0 one"))
    tokens1.eat(TestToken.number, "0")
    tokens2 = TestTokenizer.scan(InputLocation.from_string("2 thr33"))
    tokens = tokens1 + tokens2
    assert [(typ, loc.text) for typ, loc in tokens] == [
        (TestToken.identifier, "one"),
        (TestToken.number, "2"),
        (TestToken.identifier, "thr33"),
    ]
