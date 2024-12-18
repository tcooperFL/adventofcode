# Link: https://adventofcode.com/2024/day/15

from pprint import pprint
from utils import timed
from collections import OrderedDict

INPUT_FILE = "resources/2024/day15-input.txt"


class Warehouse:

    MOVES = {"^": (-1, 0), "v": (1, 0), "<": (0, -1), ">": (0, 1)}

    def __init__(self, matrix):
        self.matrix = matrix
        self.robot = self._find_robot()

    def __repr__(self):
        return "\n".join("".join(row) for row in self.matrix)

    def _find_robot(self):
        for row_idx, row in enumerate(self.matrix):
            for col_idx, char in enumerate(row):
                if char == "@":
                    return (row_idx, col_idx)
        return None  # Return None if '@' is not found

    def _move_box(self, y, x, dy, dx):
        # Move a box from (y, x) to (y + dy, x + dx)
        self.matrix[y][x] = "."
        self.matrix[y + dy][x + dx] = "O"

    def _make_space(self, y, x, dy, dx):
        # Make space at (y, x), a box there to (y + dy, x + dx) if necessary
        occupant = self.matrix[y][x]
        if occupant == ".":
            return True
        if occupant == "O":
            # If we can make the space, put the box there.
            if self._make_space(y + dy, x + dx, dy, dx):
                self._move_box(y, x, dy, dx)
                return True
        return False

    def _relocate_robot(self, y, x):
        self.matrix[self.robot[0]][self.robot[1]] = "."
        self.matrix[y][x] = "@"
        self.robot = (y, x)

    def move_robot(self, direction):
        dy, dx = Warehouse.MOVES[direction]
        newY = self.robot[0] + dy
        newX = self.robot[1] + dx
        if self._make_space(newY, newX, dy, dx):
            self._relocate_robot(newY, newX)

    def gps_coords(self):
        for y, row in enumerate(self.matrix):
            for x, c in enumerate(row):
                if c == "O":
                    yield (y * 100) + x


class Warehouse2(Warehouse):
    def __init__(self, matrix):
        super().__init__(matrix)
        self._expand_matrix()
        self.robot = (self.robot[0], self.robot[1] * 2)

    def _expand_matrix(self):
        replacements = {"O": "[]", ".": "..", "#": "##", "@": "@."}

        # Replace the old characters with the new characters in each row
        new_matrix = []
        for row in self.matrix:
            s = "".join(row)
            for old, new in replacements.items():
                s = s.replace(old, new)
            new_matrix.append(list(s))

        self.matrix = new_matrix

    def _move_box(self, y, x, dy, dx):
        # Move a double-width box from (y, x) to (y + dy, x + dx)
        # Erase old box
        self.matrix[y][x] = "."
        self.matrix[y][x + 1] = "."

        # Put the new box in place
        self.matrix[y + dy][x + dx] = "["
        self.matrix[y + dy][x + dx + 1] = "]"

    def _make_space(self, y, x, dy, dx) -> bool:
        # Make a space at (y, x), moving a box to (y + dy, x + dx) if necessary
        # Make (single-width) space at (y, x), moving a double-width
        # box to (y + dy, x + dx) if necessary
        moves = OrderedDict()
        if self._collect_moves(y, x, dy, dx, moves):
            for y, x, ch in moves:
                self.matrix[y][x] = ch
            return True
        return False

    def _collect_moves(self, y, x, dy, dx, moves) -> bool:
        # Make a space at (y, x), moving a box to (y + dy, x + dx) if necessary
        # Make (single-width) space at (y, x), moving a double-width
        # box to (y + dy, x + dx) if necessary
        occupant = self.matrix[y][x]
        if occupant == ".":
            return True
        if occupant == "#":
            return False
        if dy == 0:
            if occupant == "[":
                # Horizontal move, drag left
                if dx == -1:
                    if self._collect_moves(y, x - 1, 0, -1, moves):
                        moves[(y, x - 1, "[")] = None
                        moves[(y, x, "]")] = None
                        moves[(y, x + 1, ".")] = None
                        return True
                else:  # Pull from right
                    return self._collect_moves(y, x + 1, dy, 1, moves)
            elif occupant == "]":
                if dx == 1:
                    if self._collect_moves(y, x + 1, 0, 1, moves):
                        moves[(y, x + 1, "]")] = None
                        moves[(y, x, "[")] = None
                        moves[(y, x - 1, ".")] = None
                        return True
                elif dx == -1:
                    return self._collect_moves(y, x - 1, dy, dx, moves)

        else:  # dx == 0
            x = x - 1 if occupant == "]" else x
            if self._collect_moves(
                y + dy, x + dx, dy, dx, moves
            ) and self._collect_moves(y + dy, x + 1 + dx, dy, dx, moves):
                moves[(y + dy, x + dx, "[")] = None
                moves[(y + dy, x + 1 + dx, "]")] = None
                moves[(y, x + dx, ".")] = None
                moves[(y, x + 1 + dx, ".")] = None
                return True
        return False

    def gps_coords(self):
        for y, row in enumerate(self.matrix):
            for x, c in enumerate(row):
                if c == "[":
                    yield (y * 100) + x


def load_data():
    # Read the INPUT_FILE and separate it into two parts by splitting on the double newline.
    map, moves = open(INPUT_FILE).read().split("\n\n")
    map = [list(line) for line in map.split()]
    return map, moves


def solve(w, moves):
    count = 0
    # print(w)
    for c in moves:
        if c != "\n":
            w.move_robot(c)
            count += 1
            # print(w)
    print(f"Moves: {count}")
    return sum(w.gps_coords())


@timed
def part1():
    map, moves = load_data()
    return solve(Warehouse(map), moves)


@timed
def part2():
    map, moves = load_data()
    return solve(Warehouse2(map), moves)


if __name__ == "__main__":

    print(part1())  # 1492518
    print(part2())  # 1512860
