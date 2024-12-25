# Link: https://adventofcode.com/2024/day/18

from utils import timed
from bisect import bisect

INPUT_FILE = "resources/2024/day18-input.txt"
SPACE_WIDTH = 70
SPACE_HEIGHT = 70


def load_bytes():
    with open(INPUT_FILE) as f:
        return [tuple(map(int, line.split(","))) for line in f]


def search(corrupted, i):
    visited = {*corrupted[:i]}
    queue = []
    queue.append((0, (0, 0)))
    for steps, (x, y) in queue:
        if (x, y) == (SPACE_WIDTH, SPACE_HEIGHT):
            return steps

        for x, y in ((x, y + 1), (x + 1, y), (x, y - 1), (x - 1, y)):
            if (
                0 <= x <= SPACE_WIDTH
                and 0 <= y <= SPACE_HEIGHT
                and (x, y) not in visited
            ):
                queue.append((steps + 1, (x, y)))
                visited.add((x, y))

    return 1e9


@timed
def part1():
    data = load_bytes()
    return search(data, 1024)


@timed
def part2():
    data = load_bytes()
    return data[bisect(range(len(data)), 1e9 - 1, key=lambda x: search(data, x)) - 1]


if __name__ == "__main__":
    print(part1())
    print(part2())
