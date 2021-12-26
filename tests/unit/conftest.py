from __future__ import annotations

from resource import RLIMIT_AS, getrlimit, setrlimit

MEM_LIMIT_GB = 1


def set_memory_limit(size: int) -> None:
    soft, hard = getrlimit(RLIMIT_AS)
    setrlimit(RLIMIT_AS, (size, hard))


set_memory_limit(MEM_LIMIT_GB * 1024 ** 3)
