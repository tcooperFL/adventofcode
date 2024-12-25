# Link: https://adventofcode.com/2024/day/19

import re
from pprint import pprint
from functools import cache
from utils import timed

INPUT_FILE = "resources/2024/day19-input.txt"


def get_data() -> tuple[set[str], list[str]]:
    with open(INPUT_FILE) as f:
        towels, patterns = f.read().split("\n\n")
        return set(towels.split(", ")), patterns.split("\n")


def dedup(towels: set[str]):
    # Remove all towels that can be derived from other towels
    for t in sorted(towels, key=len, reverse=True):
        regex = "^(" + "|".join((p for p in towels if len(p) < len(t))) + ")+$"
        if re.match(regex, t):
            towels.remove(t)


@timed
def part1():
    towels, patterns = get_data()

    # So much redundancy in the towels. Trim to the minimum primitives.
    # All other towels can be derived from these.
    dedup(towels)

    # Build a regex with these patterns and count the number of matches
    regex = re.compile("^(" + "|".join(sorted(towels, key=len, reverse=True)) + ")+$")
    return [p for p in patterns if re.match(regex, p)]


def count_paths(i: int, goal: int, g: dict[int, list[int]]) -> int:

    @cache
    def search(i: int) -> int:
        if i == goal:
            return 1
        return sum(search(j) for j in g.get(i, []))

    return search(i)


def find_all_indexes(string, substring):
    return [
        i
        for i in range(len(string) - len(substring) + 1)
        if string.startswith(substring, i)
    ]


def compile_pattern(p, towels):
    g = dict()
    for t in towels:
        for i in find_all_indexes(p, t):
            g.setdefault(i, []).append(i + len(t))
    return g


@timed
def part2():
    towels, _ = get_data()
    return sum(count_paths(0, len(p), compile_pattern(p, towels)) for p in part1())


if __name__ == "__main__":
    pprint(len(part1()))
    print(part2())
