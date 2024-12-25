# Link: https://adventofcode.com/2024/day/17

from utils import timed

# Based on u/Boojum's solution

INPUT_FILE = "resources/2024/day17-input.txt"

def parse_input(file):
    return list( map( int, open(file).read().split()[ -1 ].split( ',' ) ) )

def solve(program, pos, r):
    if pos < 0:
        return r
    print(f"Solve for position: {pos}, R: {r}")
    for d in range( 8 ):
        a, i = r << 3 | d, 0
        while i < len( program ):

            # Set combo operand
            operand = program[ i + 1]
            match operand:
                case 4: combo = a
                case 5: combo = b
                case 6: combo = c
                case _: combo = operand

            # Execute instruction
            opcode = program[ i ]
            match opcode:
                case 0: a >>= combo
                case 1: b ^= operand
                case 2: b = combo & 7
                case 3: i = operand - 2 if a != 0 else i
                case 4: b ^= c
                case 5: output = combo & 7; break
                case 6: b = a >> combo
                case 7: c = a >> combo
            i += 2

        if output == program[pos]:
            if result := solve(program, pos - 1, r << 3 | d):
                return result
    return False

@timed
def part2():
    program = parse_input(INPUT_FILE)
    return solve(program, len(program) - 1, 0)

if __name__ == "__main__":
    print(part2())