# Link: https://adventofcode.com/2024/day/10

from pprint import pprint
from utils import timed
from dataclasses import dataclass

INPUT_FILE = "resources/2024/day10-input.txt"

DIRECTIONS = ((0, -1), (1, 0), (0, 1), (-1, 0))


def get_data():
    with open(INPUT_FILE) as f:
        return [
            list(map(lambda v: -1 if v == "." else int(v), line.strip())) for line in f
        ]


def create_map(data):
    m = []
    for row in data:
        m.append([-1] + row + [-1])
    m.insert(0, [-1] * len(m[0]))
    m.append([-1] * len(m[0]))
    return m


def map_to_string(m):
    return "\n".join(["".join(map(str, row)) for row in m])


def trailheads(m):
    for y, row in enumerate(m):
        for x, cell in enumerate(row):
            if cell == 0:
                yield (y, x)


@dataclass
class Trails:
    paths: int
    summits: set


def walk(m, pos, target, paths: Trails):
    for dy, dx in DIRECTIONS:
        yStep, xStep = pos[0] + dy, pos[1] + dx
        height = m[yStep][xStep]
        if height == target:
            if target == 9:
                paths.summits.add((xStep, yStep))
                paths.paths += 1
            else:
                walk(m, (yStep, xStep), target + 1, paths)


def solve():
    m = create_map(get_data())
    
    score = 0
    paths = 0

    for head in trailheads(m):
        trails = Trails(0, set())
        walk(m, head, 1, trails)
        paths += trails.paths
        score += len(trails.summits)

    return score, paths

@timed
def part1():
    return solve()[0]

@timed
def part2():
    return solve()[1]


if __name__ == "__main__":
    print(part1())
    print(part2())
