# Link: https://adventofcode.com/2024/day/17

import re
from pprint import pprint
from utils import timed

INPUT_FILE = "resources/2024/day17-input.txt"


class Computer:

    def __init__(self, registers, instructions):
        self.registers = registers
        self.instructions = instructions
        self.instruction_pointer = 0
        self.instruction_set = [
            Computer.adv,
            Computer.bxl,
            Computer.bst,
            Computer.jnz,
            Computer.bxc,
            Computer.out,
            Computer.bdv,
            Computer.cdv,
        ]
        self.output_values = []
        self.executed = 0
        self.halt = False
        self.output_limit = float("inf")
        self.finished = False

    @classmethod
    def create(cls, file, part2=False):
        with open(INPUT_FILE) as f:
            registers = {
                r: int(re.search(r"\d+$", f.readline()).group())
                for r in ("A", "B", "C")
            }
            f.readline()
            numbers = list(map(int, re.findall(r"\d+", f.readline())))
            instructions = list(zip(numbers[::2], numbers[1::2]))
        if part2:
            return MatchingComputer(registers, instructions, numbers)
        return Computer(registers, instructions)

    def __repr__(self):
        return (
            f"Computer\n"
            f" . Registers: {self.registers}\n"
            f" . Instructions: {self.instructions}\n"
            f" .   at {self.instruction_pointer} "
            f"({'done' if self.finished else self.instructions[self.instruction_pointer]})\n"
            f" . Output: {self.output_values}\n"
            f" . Executed {self.executed} instructions"
        )

    def run(self) -> str:
        self.output_values.clear()
        instruction_count = len(self.instructions)
        self.instruction_pointer = 0
        self.halt = False
        while self.instruction_pointer < instruction_count and not self.halt:
            current_pointer = self.instruction_pointer
            instruction = self.instructions[current_pointer]
            # Execute instruction
            self.instruction_set[instruction[0]](self, instruction[1])
            self.executed += 1
            # Move to next instruction
            if self.instruction_pointer == current_pointer:
                self.instruction_pointer += 1
        self.finished = not self.halt and True
        return ",".join(map(str, self.output_values))

    def output(self, value):
        self.output_values.append(value)

    def combo(self, instruction):
        lookup = {4: "A", 5: "B", 6: "C"}
        return self.registers.get(lookup.get(instruction, instruction), instruction)

    def adv(self, operand):
        denominator = 2 ** self.combo(operand)
        self.registers["A"] //= denominator

    def bxl(self, operand):
        self.registers["B"] ^= operand

    def bst(self, operand):
        self.registers["B"] = self.combo(operand) % 8

    def jnz(self, operand):
        if self.registers["A"] != 0:
            self.instruction_pointer = operand

    def bxc(self, _):
        self.registers["B"] ^= self.registers["C"]

    def out(self, operand):
        value = self.combo(operand) % 8
        self.output(value)

    def bdv(self, operand):
        denominator = 2 ** self.combo(operand)
        self.registers["B"] = self.registers["A"] // denominator

    def cdv(self, operand):
        denominator = 2 ** self.combo(operand)
        self.registers["C"] = self.registers["A"] // denominator


@timed
def part1():
    computer = Computer.create(INPUT_FILE)
    result = computer.run()
    pprint(computer)
    return f"Output: {result}"


if __name__ == "__main__":
    print(part1())
