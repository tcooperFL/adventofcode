# Link: https://adventofcode.com/2024/day/20

from pprint import pprint
from utils import timed

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


def calculate_cheats(track, path):
    timings = {pos: i for i, pos in enumerate(path)}
    cheats = dict()
    for (py, px), moment in timings.items():
        for dy, dx in ((0, 1), (1, 0), (0, -1), (-1, 0)):
            if (py + dy, px + dx) in track["blocked"]:
                landing = (py + 2 * dy, px + 2 * dx)
                new_moment = timings.get(landing, -1e9)
                if new_moment > moment:
                    savings = (new_moment - moment) - 2  # 2 is the cost of the jump
                    cheats[savings] = cheats.get(savings, 0) + 1

    return cheats


@timed
def part1():
    track = get_track()
    path = find_path(track)
    cheats = calculate_cheats(track, path)
    return sum(cheats[savings] for savings in cheats if savings >= 100)


# Part 2


@timed
def part2():
    # TODO: Inside calculate_cheats we look each direction where there is a wall
    # and then we check do another search up to 20 steps and collect all cheat
    # savings.
    return "tbd"


if __name__ == "__main__":
    pprint(part1())
    # print(part2())
