from __future__ import annotations

from typing import TypeVar

import pytest

T = TypeVar("T")


def get_flag_marker(request: pytest.FixtureRequest, name: str, default: T) -> bool | T:
    if marker := request.node.get_closest_marker(name):
        try:
            (value,) = marker.args
        except ValueError:
            raise ValueError(
                f"Marker '{name}' should have single argument, got {len(marker.args)}"
            )
        if not isinstance(value, bool):
            raise TypeError(
                f"Argument for marker '{name}' should be 'bool', got '{type(value).__name__}'"
            )
        return value
    else:
        return default
