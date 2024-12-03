# Link: https://adventofcode.com/2024/day/3

from pprint import pprint
from utils import timed
import re

INPUT_FILE = "resources/2024/day3-input.txt"


def get_program():
    with open(INPUT_FILE) as f:
        for line in f:
            yield line.strip()


def extract_mul_calls(regex):
    enabled = True
    for line in get_program():
        pattern = regex
        for match in re.findall(pattern, line):
            if match == "do()":
                enabled = True
            elif match == "don't()":
                enabled = False
            elif enabled:
                yield match


def execute_mul(instruction):
    x, y = re.findall(r"\d+", instruction)
    return int(x) * int(y)


def solve(regex):
    return sum(map(execute_mul, extract_mul_calls(regex)))


@timed
def part1():
    return solve(r"mul\(\d*,\d*\)")


@timed
def part2():
    return solve(r"mul\(\d*,\d*\)|do\(\)|don't\(\)")


if __name__ == "__main__":
    print(part1())
    print(part2())
