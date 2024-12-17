# Link: https://adventofcode.com/2024/day/13

from utils import timed
import re


INPUT_FILE = "resources/2024/day13-input.txt"


def get_machines():
    return [
        tuple(map(int, re.findall(r"\d+", machine)))
        for machine in open(INPUT_FILE).read().split("\n\n")
    ]


def cost(ax, ay, bx, by, px, py, point_add=0):
    """
    Since we know that the intersection point results in integer values
    for the coefficients (a) and (b), we can use divmod to check for a
    remainder of 0. This would confirm that the point (P(px, py)) can
    be expressed as a linear combination of
    points (A(ax, ay)) and (B(bx, by)) with integer coefficients.
    """

    px, py = px + point_add, py + point_add
    b, brem = divmod(ay * px - ax * py, ay * bx - ax * by)
    a, arem = divmod(px - b * bx, ax)
    return 0 if arem or brem else a * 3 + b


@timed
def part1():
    return sum(cost(*m) for m in get_machines())


@timed
def part2():
    return sum(cost(*m, point_add=10000000000000) for m in get_machines())


if __name__ == "__main__":
    print(part1())
    print(part2())
