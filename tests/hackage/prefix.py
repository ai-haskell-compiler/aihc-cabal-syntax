"""Copy a fixed prefix from a compressed Hackage index."""

import gzip
import sys


def copy_prefix(source, target, length):
    if length <= 0 or length % 512:
        raise ValueError("The prefix must contain complete tar blocks.")
    while length:
        block = source.read(min(length, 1024 * 1024))
        if not block:
            raise ValueError("The index is shorter than the fixed prefix.")
        target.write(block)
        length -= len(block)


if __name__ == "__main__":
    with gzip.open(sys.argv[1], "rb") as source, open(sys.argv[2], "wb") as target:
        copy_prefix(source, target, int(sys.argv[3]))
