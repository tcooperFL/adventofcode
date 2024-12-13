# Link: https://adventofcode.com/2024/day/11

from pprint import pprint
from utils import timed
from itertools import accumulate, repeat
from functools import lru_cache

INPUT_FILE = "resources/2024/day11-input.txt"


def get_stones():
    with open(INPUT_FILE) as f:
        for line in f:
            return list(map(int, line.split()))
        

@timed
def part1(n):
    stones = get_stones()
    result = []

    for _ in range(n):
        for s in stones:
            if s == 0:
                result.append(1)
            else:
                str_s = str(s)
                if len(str_s) % 2 == 0:
                    half = len(str_s) // 2
                    result.append(int(str_s[:half]))
                    result.append(int(str_s[half:]))
                else:
                    result.append(2024 * s)

        stones = result
        result = []

    return len(stones)

# Part 2

@lru_cache(maxsize=None)
def blink(s, n):
    if n == 0:
        return 1
    
    if s == 0:
        return blink(1, n - 1)

    str_s = str(s)
    if len(str_s) % 2 == 0:
        half = len(str_s) // 2
        s1 = int(str_s[:half])
        s2 = int(str_s[half:])
        return blink(s1, n-1) + blink(s2, n-1)
    
    return blink(2024 * s, n - 1)

@timed
def part2(n):
    stones = get_stones()
    return sum(blink(s, n) for s in stones)


if __name__ == "__main__":

    print(part1(25))

    print(part2(75))
