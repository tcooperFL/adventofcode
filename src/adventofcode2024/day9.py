# Link: https://adventofcode.com/2024/day/9

from pprint import pprint
from utils import timed
from dataclasses import dataclass

INPUT_FILE = "resources/2024/day9-input.txt"


def get_disk_map():
    # Read the file as one line
    with open(INPUT_FILE) as f:
        return list(map(int, f.readline().strip()))


# Part 1


def create_blocks(dm: list[int]) -> list[int]:
    block_list: list[int] = []
    is_filespace = True
    id = -1
    for s in dm:
        if is_filespace:
            v = id = id + 1
        else:
            v = -1

        block_list = block_list + [v] * s
        is_filespace = not is_filespace
    return block_list


# def print_blocks(blocks):
#     print("".join("." if v == -1 else str(v) for v in blocks))


def compact(blocks):
    last = len(blocks) - 1
    for i in range(len(blocks)):
        if blocks[i] == -1:
            last = next((j for j in range(last, i, -1) if blocks[j] != -1), None)
            if last is None:
                break
            blocks[i], blocks[last] = blocks[last], blocks[i]
    return blocks


def checksum(blocks):
    return sum((i * v for i, v in enumerate(blocks) if v != -1))


@timed
def part1():
    dm = get_disk_map()
    blocks = create_blocks(dm)
    # print_blocks(blocks)
    compacted = compact(blocks)
    # print_blocks(compacted)
    return checksum(compacted)


# Part 2


@dataclass
class Block:
    file_id: int
    start: int
    size: int


def create_block_lists(dm):
    file_blocks = []
    free_blocks = []
    pos = 0
    is_filespace = True
    id = 0
    for s in dm:
        if is_filespace:
            file_blocks.append(Block(id, pos, s))
            id += 1
        else:
            free_blocks.append(Block(-1, pos, s))

        pos += s
        is_filespace = not is_filespace
    return file_blocks, free_blocks


def print_blocks(blocks):
    sorted_blocks = sorted(blocks, key=lambda b: b.start)
    print(
        "".join(
            "." * b.size if b.file_id == -1 else str(b.file_id) * b.size
            for b in sorted_blocks
        )
    )


def merge_space(spaces, hole):
    for i, space in enumerate(spaces):
        if space.start + space.size == hole.start:
            space.size += hole.size
            break
        if hole.start < space.start:
            spaces.insert(i, hole)
            break

def compact2(files, spaces):
    for file in reversed(files):
        if space := next(filter(lambda s: s.size >= file.size and s.start < file.start, spaces), None):
            # print(f"Allocating {file.file_id} to {space.start}")
            hole = Block(-1, file.start, file.size)
            file.start = space.start
            space.start += file.size
            space.size -= file.size
            merge_space(spaces, hole)
    
    return sorted(files + spaces, key=lambda b: b.start)


def checksum2(blocks):
    pos = 0
    cs = 0
    for b in blocks:
        if b.file_id != -1:
            cs += sum(i * b.file_id for i in range(pos, pos + b.size))
        pos += b.size

    return cs


@timed
def part2():
    files, spaces = create_block_lists(get_disk_map())
    # print_blocks(files + spaces)
    blocks = compact2(files, spaces)
    # print_blocks(blocks)
    return checksum2(blocks)


if __name__ == "__main__":
    pprint(part1())
    print(part2())
