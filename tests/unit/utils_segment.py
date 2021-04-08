import re

from retroasm.types import Segment, unlimited


re_segment = re.compile(r'^\[(\d*):(\d*)\]$')

def parse_segment(segment_str):
    start_str, end_str = re_segment.match(segment_str).groups()
    start = int(start_str) if start_str else 0
    width = int(end_str) - start if end_str else unlimited
    return Segment(start, width)
