# Link: https://adventofcode.com/2024/day/20

from utils import timed
from itertools import product

INPUT_FILE = "resources/2024/day20-input.txt"


def get_track():
    track = {"blocked": set()}
    with open(INPUT_FILE) as f:
        for y, r in enumerate(f):
            for x, c in enumerate(r):
                match c:
                    case "#":
                        track["blocked"].add((y, x))
                    case "S":
                        track["start"] = (y, x)
                    case "E":
                        track["end"] = (y, x)
    track["width"] = x + 1
    track["height"] = y + 1
    return track


def find_path(track):
    start = track["start"]
    end = track["end"]
    blocked = set(track["blocked"])
    queue = [(start, [start])]
    visited = set()

    while queue:
        (y, x), path = queue.pop(0)
        if (y, x) == end:
            print(f"Path length: {len(path) - 1}")
            return path
        if (y, x) in visited:
            continue
        visited.add((y, x))
        for dy, dx in ((0, 1), (1, 0), (0, -1), (-1, 0)):
            ny, nx = y + dy, x + dx
            if (ny, nx) not in blocked and (ny, nx) not in visited:
                queue.append(((ny, nx), path + [(ny, nx)]))

    return []


def calculate_cheats(track, path, distance=20):
    timings = {pos: i for i, pos in enumerate(path)}
    cheats = dict()
    for (py, px), moment in timings.items():
        for dy, dx in product(range(-distance, distance + 1), repeat=2):
            steps = abs(dy) + abs(dx)
            if 0 < steps <= distance:
                landing = (py + dy, px + dx)
                new_moment = timings.get(landing, -1e9)
                savings = (new_moment - moment) - steps
                if savings >= 100:
                    cheats[savings] = cheats.get(savings, 0) + 1

    return sum(cheats[savings] for savings in cheats)


@timed
def part1():
    track = get_track()
    path = find_path(track)
    cheats = calculate_cheats(track, path, distance=2)
    return cheats


@timed
def part2():
    track = get_track()
    path = find_path(track)
    cheats = calculate_cheats(track, path, distance=20)
    return cheats


if __name__ == "__main__":
    print(part1())
    print(part2())
