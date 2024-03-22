from __future__ import annotations

from logging import getLogger

import pytest

from retroasm.input import BadInput, DelayedError, collect_errors


def test_error_collector_nested_uncaught() -> None:
    """Errors from a nested collector propagate to its parent."""

    logger = getLogger("test")
    try:
        with collect_errors(logger) as outer_collector:
            # Will be reported as usual.
            outer_collector.error("first")

            with collect_errors(outer_collector) as inner_collector:
                # Will be reported on outer collector too.
                inner_collector.error("second")

            # Will not be executed.
            outer_collector.error("third")
    except DelayedError as err:
        assert len(err.exceptions) == 2
        first, nested = err.exceptions

        assert isinstance(first, BadInput), first
        assert str(first) == "first", first

        assert isinstance(nested, DelayedError), nested
        assert len(nested.exceptions) == 1
        (second,) = nested.exceptions

        assert isinstance(second, BadInput), second
        assert str(second) == "second", second
    else:
        pytest.fail("expected DelayedError was not raised")
