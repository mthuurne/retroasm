from __future__ import annotations

from logging import getLogger

import pytest

from retroasm.input import DelayedError, ErrorCollector


def test_error_collector_nested_caught() -> None:
    """Errors from a nested context are reported exactly once."""

    logger = getLogger("test")
    try:
        with ErrorCollector(logger).check() as collector:
            collector.error("first")

            try:
                with collector.check():
                    collector.error("second")
            except DelayedError:
                pass
            else:
                pytest.fail("expected DelayedError was not raised")

            collector.error("third")
    except DelayedError as delayed:
        assert [str(err) for err in delayed.exceptions] == ["first", "second", "third"]
    else:
        pytest.fail("expected DelayedError was not raised")


def test_error_collector_nested_uncaught() -> None:
    """
    An uncaught `DelayedError` in a nested context will cause an early exit
    of the outer context.
    """

    logger = getLogger("test")
    try:
        with ErrorCollector(logger).check() as collector:
            # Will be reported as usual.
            collector.error("first")

            with collector.check():
                # Will be reported too.
                collector.error("second")

            # Will not be executed.
            collector.error("third")
    except DelayedError as delayed:
        assert [str(err) for err in delayed.exceptions] == ["first", "second"]
    else:
        pytest.fail("expected DelayedError was not raised")
