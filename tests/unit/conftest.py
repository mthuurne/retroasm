from __future__ import annotations

from inspect import cleandoc
from resource import RLIMIT_AS, getrlimit, setrlimit
from types import FunctionType

import pytest

MEM_LIMIT_GB = 1


def set_memory_limit(size: int) -> None:
    _soft, hard = getrlimit(RLIMIT_AS)
    setrlimit(RLIMIT_AS, (size, hard))


set_memory_limit(MEM_LIMIT_GB * 1024**3)


@pytest.fixture
def docstring(request: pytest.FixtureRequest) -> str:
    """Fixture that extracts a clean docstring from the test function that requests it."""

    function: FunctionType = request.node.function
    docstring = function.__doc__
    if docstring is None:
        raise pytest.FixtureLookupError(
            request.fixturename, request, "missing docstring"
        )
    return cleandoc(docstring)
