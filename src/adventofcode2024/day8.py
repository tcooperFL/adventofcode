# Link: https://adventofcode.com/2024/day/8

from pprint import pprint
from utils import timed
from itertools import permutations, count
import logging

INPUT_FILE = "resources/2024/day8-input.txt"


def get_program():
    # Load the map and return dimensions and dict with key frequency and value list of locations
    antennas = dict()
    with open(INPUT_FILE) as f:
        for r, line in enumerate(f):
            rows = r
            for c, frequency in enumerate(line.strip()):
                cols = c
                if frequency != "." and frequency != "#":
                    antennas[frequency] = antennas.get(frequency, []) + [(r, c)]
    return rows, cols, antennas


def solve(part2=False):
    yMax, xMax, antennas = get_program()
    antinodes = set()
    for frequency, locations in antennas.items():
        for (yF1, xF1), (yF2, xF2) in permutations(locations, 2):
            if part2:
                antinodes.add((yF1, xF1))
                antinodes.add((yF2, xF2))

            dx = xF2 - xF1
            dy = yF2 - yF1
            for i in count(1):
                x = xF2 + (i * dx)
                y = yF2 + (i * dy)
                if 0 <= y <= yMax and 0 <= x <= xMax:
                    logging.info(
                        f"Antinode for {frequency} at ({yF1}, {xF1}) and ({yF2}, {xF2}) has antinode at ({y}, {x})"
                    )
                    antinodes.add((y, x))
                else:
                    break
                if not part2:
                    break
    return len(antinodes)


@timed
def part1():
    return solve()


@timed
def part2():
    return solve(part2=True)


if __name__ == "__main__":
    print(part1())
    print(part2())
