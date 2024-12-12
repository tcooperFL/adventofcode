# Link: https://adventofcode.com/2024/day/7

from utils import timed
import re
from itertools import product

INPUT_FILE = "resources/2024/day7-input.txt"


def get_equations():
    with open(INPUT_FILE) as f:
        return [
            (int(v), tuple(map(int, n)))
            for v, *n in (re.findall(r"\d+", line) for line in f)
        ]


def concatenation(i1, i2):
    # Return the integer that is the value of digits of i1 followed by i2
    return i1 * (10 ** len(str(i2))) + i2


# Given a tuple of numbers and a value, return true if the numbers can be combined in any combination of + and * to equal the value
def can_combine(value, numbers, operators) -> bool:
    ops = product(operators, repeat=len(numbers) - 1)
    for op in ops:
        result, *remaining = numbers
        for op, n in zip(op, remaining):
            result = op(result, n)
            if result > value:
                break
        if result == value:
            return True
    return False


def solve(*operators):
    return sum(
        v for v, numbers in get_equations() if can_combine(v, numbers, operators)
    )


@timed
def part1():
    return solve(int.__add__, int.__mul__)


@timed
def part2():
    return solve(int.__add__, int.__mul__, concatenation)


if __name__ == "__main__":

    print(part1())
    print(part2())
