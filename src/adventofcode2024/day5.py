# Link: https://adventofcode.com/2024/day/5

from utils import timed
from itertools import takewhile

INPUT_FILE = "resources/2024/day5-input.txt"


def get_lines():
    with open(INPUT_FILE) as f:
        for line in f:
            yield line.strip()


def get_data() -> tuple[dict[int, set[int]], list[list[int]]]:
    # Return a tuple of rules and updates
    lines_iter = get_lines()
    rules: dict[int, set[int]] = dict()

    for rule in takewhile(lambda line: line != "", lines_iter):
        p1, p2 = list(map(int, rule.split("|")))
        rules.setdefault(p1, set()).add(p2)

    updates = [list(map(int, u.split(","))) for u in lines_iter]
    return rules, updates


def comes_before_any(
    p1: int, page_set: set[int], ordering_rules: dict[int, set[int]]
) -> bool:
    # Return true if there is an intersection between the page_set and ordering_rules[p1]
    return bool(page_set & ordering_rules.get(p1, set()))


def in_correct_order(pages: list[int], ordering_rules: dict[int, set[int]]) -> bool:
    # For each page, check that it should not preceed a previous page
    previous_pages: set[int] = set()
    for p in pages:
        if comes_before_any(p, previous_pages, ordering_rules):
            return False
        previous_pages.add(p)
    return True


def middle_page(pages: list[int]) -> int:
    return pages[len(pages) // 2]


def arrange(pages: list[int], ordering_rules: dict[int, set[int]]) -> list[int]:
    pool = set(pages)
    arranged = []
    while pool:
        for p in pool:
            if not comes_before_any(p, pool, ordering_rules):
                arranged.append(p)
                pool.discard(p)
                break

    return arranged[::-1]


@timed
def part1() -> int:
    rules, updates = get_data()
    return sum(middle_page(u) for u in updates if in_correct_order(u, rules))


@timed
def part2() -> int:
    rules, updates = get_data()
    incorrectly_ordered = [u for u in updates if in_correct_order(u, rules) == False]
    fixed = [arrange(u, rules) for u in incorrectly_ordered]
    return sum(middle_page(u) for u in fixed)


if __name__ == "__main__":
    print(part1())
    print(part2())
