# Link: https://adventofcode.com/2024/day/16

from pprint import pprint
from utils import timed
from pyrsistent import pvector
import heapq

INPUT_FILE = "resources/2024/day16-input.txt"


def get_data():
    with open(INPUT_FILE) as f:
        return [list(line.strip()) for line in f]


def find_position(grid, target):
    for y, row in enumerate(grid):
        for x, c in enumerate(row):
            if c == target:
                return (y, x)
    return None


TURN_LEFT = {(0, -1): (1, 0), (1, 0): (0, 1), (0, 1): (-1, 0), (-1, 0): (0, -1)}
TURN_RIGHT = {(0, -1): (-1, 0), (-1, 0): (0, 1), (0, 1): (1, 0), (1, 0): (0, -1)}
TRAIL = {(-1, 0): "^", (1, 0): "v", (0, -1): "<", (0, 1): ">"}


def print_grid(grid):
    for row in grid:
        print("".join(row))


def paint_grid(grid, visited):
    for pos, direction in visited:
        grid[pos[0]][pos[1]] = TRAIL[direction]


def contents(grid, pos, turned=(0, 0)):
    return grid[pos[0] + turned[0]][pos[1] + turned[1]]


def advance(grid, pos, direction):
    # Look ahead in the grid from this position and direction until you either
    # find a wall ahead or a space to the left or right. Count the moves
    # taken to reach the next position.
    start_pos = pos
    # print(f"\nStarting at {start_pos} facing {direction}")
    left = TURN_LEFT[direction]
    right = TURN_RIGHT[direction]
    moves = 0
    next_pos = (pos[0] + direction[0], pos[1] + direction[1])

    # Until a wall is right in front of you, keep moving forward
    while contents(grid, next_pos) != "#":
        pos = next_pos
        moves += 1

        # If there is space to the left or to the right, stop your move
        if contents(grid, pos, left) != "#" or contents(grid, pos, right) != "#":
            break

        # Else keep moving forward if you can
        next_pos = (pos[0] + direction[0], pos[1] + direction[1])

    # print(f"Moved from {start_pos} to {pos} in {moves} moves")
    # print_grid(grid)

    return pos, moves


def pursue(heap, cost, pos, direction, path):
    heapq.heappush(heap, (cost, pos, direction, path.append((pos, direction))))
    global max_heap_size
    max_heap_size = max(max_heap_size, len(heap))


def solve(grid, first_only=True):
    cursor = find_position(grid, "S")
    destination = find_position(grid, "E")

    # Implement a beam search algorithm
    # Heap elements are (cost, (y, x), direction, set of visited positions)
    visited = set()
    heap = []
    path = pvector()
    pursue(heap, 0, cursor, (0, 1), path)

    best_paths = []
    best_cost = float('inf')

    while heap:
        # Always work on moving the shortest cost path next.
        cost, pos, direction, path = heapq.heappop(heap)
        if pos == destination:
            if first_only:
                return cost, path
            elif cost <= best_cost:
                best_cost = cost
                best_paths.append(path)
            else:
                return best_cost, best_paths
        
        # If we haven't been here in this direction before, add it to the heap
        if (pos, direction) not in visited:
            visited.add((pos, direction))

            # Try forward, left, or right.
            if contents(grid, pos, direction) != "#":
                new_pos, moves = advance(grid, pos, direction)
                if moves > 0:
                    pursue(heap, cost + moves, new_pos, direction, path)
            if contents(grid, pos, TURN_LEFT[direction]) != "#":
                pursue(heap, cost + 1000, pos, TURN_LEFT[direction], path)
            if contents(grid, pos, TURN_RIGHT[direction]) != "#":
                pursue(heap, cost + 1000, pos, TURN_RIGHT[direction], path)

    return None


@timed
def part1():
    grid = get_data()
    cost, path = solve(grid)
    paint_grid(grid, path)
    print_grid(grid)
    return cost


def collect_tiles(grid, p1, p2, direction, tiles):
    while p1 != p2:
        tiles.add(p1)
        p1 = (p1[0] + direction[0], p1[1] + direction[1])
    tiles.add(p2)

@timed
def part2():
    grid = get_data()
    cost, paths = solve(grid, first_only=False)

    print(f"Found {len(paths)} paths with cost {cost}")
    tiles = set()

    # Collect all the tiles in any path
    for path in paths:
        from_points = list(path)
        to_points = list(path)[1:]
        for (from_path, to_path) in zip(from_points, to_points):
            collect_tiles(grid, from_path[0], to_path[0], from_path[1], tiles)
     
    return len(tiles)



if __name__ == "__main__":

    max_heap_size = 0

    print(part1())
    print(part2())

    print(f"Max heap size: {max_heap_size}")
