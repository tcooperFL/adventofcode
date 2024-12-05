# Link: https://adventofcode.com/2024/day/2

from utils import timed

INPUT_FILE = "resources/2024/day2-input.txt"


def get_reports():
    with open(INPUT_FILE) as f:
        for line in f:
            yield list(map(int, line.split()))


def level_test(pred_fn, pairs):
    for i, (x, y) in enumerate(pairs):
        if not pred_fn(x, y):
            return i


def fails_at_level(report):
    pairs = list(zip(report, report[1:]))
    level = level_test(lambda x, y: 1 <= abs(x - y) <= 3, pairs)
    if level is not None:
        return level
    if level_test(lambda x, y: x < y, pairs) is None:
        return None
    return level_test(lambda x, y: x > y, pairs)


def is_safe(report):
    return fails_at_level(report) is None


def filter_reports(reports, criteria=is_safe):
    return [r for r in reports if criteria(r)]


@timed
def part1():
    return len(filter_reports(get_reports()))


def remove_level(report, level):
    return report[:level] + report[level + 1 :]


def is_tolerated(report):
    # Do the quick tests first based on the failed level
    level = fails_at_level(report)
    if level is None:
        return True
    if fails_at_level(remove_level(report, level)) is None:
        return True
    if fails_at_level(remove_level(report, level + 1)) is None:
        return True

    # The problem is a combination, so try every level
    for i in range(len(report)):
        if i != level and i != level + 1:  # already tried these
            if fails_at_level(remove_level(report, i)) is None:
                return True
    return False


@timed
def part2():
    return len(filter_reports(get_reports(), is_tolerated))


if __name__ == "__main__":
    print(part1())  # 220
    print(part2())  # 296
