from __future__ import annotations

import re

from retroasm.types import Segment, Width, unlimited

re_segment = re.compile(r"^\[(\d*):(\d*)\]$")


def parse_segment(segment_str: str) -> Segment:
    match = re_segment.match(segment_str)
    assert match is not None, segment_str
    start_str, end_str = match.groups()
    start = int(start_str) if start_str else 0
    width: Width
    if end_str:
        width = int(end_str) - start
    else:
        width = unlimited
    return Segment(start, width)
