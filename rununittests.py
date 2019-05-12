#!/usr/bin/env python3
#
# $ ./rununittests.py <filter>
#   Runs all test cases where the dot-separated filter elements match
#   a subsequence of the <module>.<class>.<method> ID of the test case.
#
# For example:
#
# $ ./rununittests.py
#   Runs all unit tests.
#
# $ ./rununittests.py SliceTests
#   Runs all test cases in the "SliceTests" class.
#
# $ ./rununittests.py test_identity
#   Runs all test cases in methods named "test_identity".
#
# $ ./rununittests.py AndTests.test_identity
#   Runs the test case in the method named "test_identity" in the class
#   named "AndTests".

from unittest import TestSuite, TextTestRunner, defaultTestLoader
import sys

if __name__ == '__main__':
    loader = defaultTestLoader
    modules = loader.discover('tests/unit/')
    if len(sys.argv) == 1:
        toRun = modules
    else:
        toRun = TestSuite()
        filters = tuple(arg.split('.') for arg in sys.argv[1:])
        for module in modules:
            for suite in module:
                if not hasattr(suite, '__iter__'):
                    # Import errors are wrapped into TestCase instances;
                    # to show the error make sure those are run too.
                    toRun.addTest(suite)
                    continue
                for case in suite:
                    parts = case.id().split('.')
                    if any(
                            parts[i:i+len(f)] == f
                            for f in filters
                            for i in range(len(parts) - len(f) + 1)
                            ):
                        toRun.addTest(case)
    runner = TextTestRunner()
    runner.run(toRun)
