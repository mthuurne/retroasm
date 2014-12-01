#!/usr/bin/env python3

from unittest import TextTestRunner, defaultTestLoader

loader = defaultTestLoader
test = loader.discover('test/unittest/')
runner = TextTestRunner()

if __name__ == '__main__':
    runner.run(test)
