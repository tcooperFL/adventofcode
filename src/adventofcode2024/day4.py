# Link: https://adventofcode.com/2024/day/4

from pprint import pprint
from utils import timed

INPUT_FILE = "resources/2024/day4-input.txt"


class WordSearch:
    def __init__(self, puzzle):
        self.puzzle = puzzle
        self.directions = [
            (-1, -1),
            (-1, 0),
            (-1, 1),
            (0, -1),
            (0, 1),
            (1, -1),
            (1, 0),
            (1, 1),
        ]

    def matches_at(self, start, word, direction, offset=0):
        r0, c0 = start
        dr, dc = direction
        for i, ch in enumerate(word):
            r, c = r0 + ((i - offset) * dr), c0 + ((i - offset) * dc)
            if (
                r < 0
                or r >= len(self.puzzle)
                or c < 0
                or c >= len(self.puzzle[0])
                or self.puzzle[r][c] != ch
            ):
                return 0
        return 1

    def occurrences_at(self, pos, word):
        return sum(self.matches_at(pos, word, d) for d in self.directions)

    def anchor_character(self, word):
        return word[0]

    def count_occurrences(self, word):
        count = 0
        anchor = self.anchor_character(word)
        for r in range(len(self.puzzle)):
            for c in [i for i, c in enumerate(self.puzzle[r]) if c == anchor]:
                count += self.occurrences_at((r, c), word)
        return count


class WordSearch2(WordSearch):
    def __init__(self, puzzle):
        super().__init__(puzzle)

    def anchor_character(self, word):
        # Middle character of the word
        return word[len(word) // 2]

    def occurrences_at(self, pos, word):
        # Test for any in both diagnals from the middle character
        offset = len(word) // 2
        if any(self.matches_at(pos, word, d, offset) for d in [(1, 1), (-1, -1)]):
            if any(self.matches_at(pos, word, d, offset) for d in [(1, -1), (-1, 1)]):
                return 1
        return 0


def get_puzzle():
    # Read the input file and return the puzzle in the form of a two-dimensional array of characters
    with open(INPUT_FILE) as f:
        return [line.strip() for line in f]


@timed
def part1():
    ws = WordSearch(get_puzzle())
    return ws.count_occurrences("XMAS")

@timed
def part2():
    ws = WordSearch2(get_puzzle())
    return ws.count_occurrences("MAS")


if __name__ == "__main__":
    print(part1())
    print(part2())
