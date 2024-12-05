# Link: https://adventofcode.com/2024/day/1

from pprint import pprint
from utils import timed

INPUT_FILE = "resources/2024/day1-input.txt"


def get_lines():
    with open(INPUT_FILE) as f:
        for line in f:
            yield line


def get_data():
    col1, col2 = [], []

    for line in get_lines():
        val1, val2 = map(int, line.split())
        col1.append(val1)
        col2.append(val2)

    return col1, col2


def distance(col1, col2):
    return sum(abs(col1 - col2) for col1, col2 in zip(sorted(col1), sorted(col2)))


@timed
def part1():
    return distance(*get_data())


def score(col1, col2):
    return sum(c1 * sum(1 for c2 in col2 if c1 == c2) for c1 in col1)


@timed
def part2():
    return score(*get_data())


if __name__ == "__main__":
    print(part1())
    print(part2())
