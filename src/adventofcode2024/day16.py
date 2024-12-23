# Link: https://adventofcode.com/2024/day/16

# Implement Dykstra's algorithm to solve this shortest-path problem.

from utils import timed
from pyrsistent import pvector
import heapq
import logging

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
WALL = "#"
SEAT = "O"

def print_grid(grid):
    for row in grid:
        print("".join(row))


def paint_grid(grid, visited):
    for pos, direction in visited:
        grid[pos[0]][pos[1]] = SEAT


def contents(grid, pos, turned=(0, 0)):
    return grid[pos[0] + turned[0]][pos[1] + turned[1]]


def advance(grid, pos, direction):
    # Look ahead in the grid from this position and direction until you either
    # find a wall ahead or a space to the left or right. Count the moves
    # taken to reach the next position.

    left = TURN_LEFT[direction]
    right = TURN_RIGHT[direction]
    moves = 0
    next_pos = (pos[0] + direction[0], pos[1] + direction[1])

    # Until a wall is right in front of you, keep moving forward
    while contents(grid, next_pos) != WALL:
        pos = next_pos
        moves += 1

        # If there is space to the left or to the right, stop your move
        if contents(grid, pos, left) != WALL or contents(grid, pos, right) != WALL:
            break

        # Else keep moving forward if you can
        next_pos = (pos[0] + direction[0], pos[1] + direction[1])

    return pos, moves


def pursue(heap, cost, pos, direction, path):
    # Queue this path
    heapq.heappush(
        heap, (cost, pos, direction, path.append((pos, direction)))
    )


def explore_ahead(grid, heap, cost, pos, direction, path):
    # If no wall in front of us, pursue this path
    if contents(grid, pos, direction) != WALL:
        new_pos, moves = advance(grid, pos, direction)
        if moves > 0:
            pursue(heap, cost + moves, new_pos, direction, path)     

def explore_directions(grid, heap, cost, pos, direction, path):
    # Try forward, and at additiona cost: left, or right.
    explore_ahead(grid, heap, cost, pos, direction, path)
    explore_ahead(grid, heap, cost + 1000, pos, TURN_LEFT[direction], path)
    explore_ahead(grid, heap, cost + 1000, pos, TURN_RIGHT[direction], path)


def solve(grid, first_only=True):
    start_pos = find_position(grid, "S")
    end_pos = find_position(grid, "E")

    # Implement Dykstra's algorithm with the heap as a priority queue.
    # Heap elements are (cost, (y, x), direction, set of visited positions)
    visited = dict()
    heap, max_heap_size = [], 0
    path = pvector()
    pursue(heap, 0, start_pos, (0, 1), path)
    lowest_cost = float('inf')
    solutions = []

    while heap:
        # Always work on moving the shortest cost path next.
        cost, pos, direction, path = heapq.heappop(heap)
        if (pos == end_pos) and cost <= lowest_cost:
            if first_only:
                logging.info(f"Found solution with cost {cost}, max heap size {max_heap_size}")
                return cost, path
            
            solutions.append(path)
            lowest_cost = cost
        elif cost >= lowest_cost:
            # If we have found a path, now we have everything;
            logging.info(f"Found {len(solutions)} solutions with cost {lowest_cost}, max heap size {max_heap_size}")
            return lowest_cost, solutions

        # If we haven't yet been here before at a lower cost, explore
        if pos != end_pos and cost <= visited.get((pos, direction), cost):
            visited[(pos, direction)] = cost
            explore_directions(grid, heap, cost, pos, direction, path)
        
        # Track max size of our heap
        max_heap_size = max(max_heap_size, len(heap))
    return None


@timed
def part1():
    grid = get_data()
    cost, _ = solve(grid)
    return cost


def collect_tiles(p1, p2, direction, tiles):
    p = p1
    while p != p2:
        tiles.add(p)
        p = (p[0] + direction[0], p[1] + direction[1])
    tiles.add(p2)


@timed
def part2():
    grid = get_data()
    cost, solutions = solve(grid, first_only=False)

    print(f"Found {len(solutions)} solutions with cost {cost}")
    tiles = set()

    # Collect all the tiles in any path
    for paths in solutions:
        points = paths
        to_points = list(points)[1:]
        for from_path, to_path in zip(points, to_points):
            collect_tiles(from_path[0], to_path[0], to_path[1], tiles)
        if logging.getLogger().isEnabledFor(logging.INFO):
            paint_grid(grid, points)

    if logging.getLogger().isEnabledFor(logging.DEBUG):
        print_grid(grid)
    return len(tiles)


if __name__ == "__main__":

    logging.basicConfig(level=logging.INFO)
    max_heap_size = 0

    print(part1())
    print(part2())

    logging.info(f"Max heap size: {max_heap_size}")
