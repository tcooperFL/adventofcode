# Link: https://adventofcode.com/2024/day/14

from utils import timed
from dataclasses import dataclass
import re
from functools import reduce
from operator import mul

INPUT_FILE = "resources/2024/day14-input.txt"


@dataclass
class Grid:
    width: int = 101
    height: int = 103
    robots: list = None

    def __post_init__(self):
        if self.robots is None:
            self.robots = []

    @property
    def snapshot(self):
        state = [[0 for _ in range(self.width)] for _ in range(self.height)]
        for robot in self.robots:
            state[robot.y][robot.x] += 1
        return state

    def __repr__(self):
        return "\n".join(
            "".join("." if cell == 0 else str(cell) for cell in row)
            for row in self.snapshot
        )

    def add(self, robots):
        self.robots.extend(robots)

    def clump_value(self):
        # Iterate over the robot positions and calculate the sum of the
        # Manhattan distances between each robot and the center of the grid
        # (mid_y, mid_x).
        mid_y, mid_x = self.height // 2, self.width // 2
        return sum(
            [abs(robot.y - mid_y) + abs(robot.x - mid_x) for robot in self.robots]
        )

    def simulate(self, iterations, display=False):
        if display:
            min_sum = self.clump_value()
            for i in range(iterations):
                self.simulate(1, display=False)

                # If the clump value exceeds the current max_sum, display the grid
                new_min = self.clump_value()
                if new_min < min_sum:
                    print(f"More clumped {new_min} after iteration {i + 1}:")
                    print(self)
                    input("Press Enter to continue...")
                    min_sum = new_min

        else:
            for robot in self.robots:
                robot.run(iterations, self)
        return iterations

    def _section_score(self, minY, minX, maxY, maxX):
        # Count the number of robots in the section
        return sum(
            1
            for robot in self.robots
            if minX <= robot.x < maxX and minY <= robot.y < maxY
        )

    def _quadrants(self):
        mid_y, mid_x = self.height // 2, self.width // 2
        return [
            (0, 0, mid_y, mid_x),  # Top-left quadrant
            (0, mid_x + 1, mid_y, self.width),  # Top-right quadrant
            (mid_y + 1, 0, self.height, mid_x),  # Bottom-left quadrant
            (mid_y + 1, mid_x + 1, self.height, self.width),  # Bottom-right quadrant
        ]

    def score(self):
        # Split the grid into 4 quadrants
        # and count the number of robots in each
        # quadrant. Multiply these values to get the score.
        return reduce(
            mul, (self._section_score(*quadrant) for quadrant in self._quadrants())
        )


class Robot:
    _id_counter = 0

    def __init__(self, px, py, vx, vy):
        Robot._id_counter += 1
        self.id = Robot._id_counter
        self.x = px
        self.y = py
        self.vx = vx
        self.vy = vy

    def __repr__(self):
        return (
            f"Robot {self.id} at ({self.x}, {self.y}), velocity ({self.vx}, {self.vy})"
        )

    def run(self, iterations, grid: Grid):
        for _ in range(iterations):
            self.x = (self.x + self.vx) % grid.width
            self.y = (self.y + self.vy) % grid.height


def get_robots():
    return [
        Robot(*tuple(map(int, re.findall(r"-?\d+", line)))) for line in open(INPUT_FILE)
    ]


@timed
def part1():
    grid = Grid()
    grid.add(get_robots())
    iterations = grid.simulate(100)
    print(f"\nAfter {iterations} iterations:")
    return grid.score()


# Part 2


@timed
def part2():
    grid = Grid()
    grid.add(get_robots())
    iterations = grid.simulate(200000, display=True)
    print(f"\nAfter {iterations} iterations:")
    if grid.clumped():
        print(grid)
    else:
        print("No clumps found.")


if __name__ == "__main__":
    print(part1())  # 232253028
    # part2()  # after 8179 iterations
