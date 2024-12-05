# Link: https://adventofcode.com/2024/day/5

from pprint import pprint
from utils import timed

INPUT_FILE = "resources/2024/day5-input.txt"


def get_program():
    with open(INPUT_FILE) as f:
        for line in f:
            yield line.strip()


@timed
def part1():
    return "tbd"


# Part 2


@timed
def part2():
    return "tbd"


if __name__ == "__main__":
    print(part1())
    # print(part2())
