from utils import timed

INPUT_FILE = "resources/2024/day6-input.txt"
DIRECTIONS = ((0,-1), (1,0), (0,1), (-1,0))

def load_data():
    # Load the map from the input file as a list of lists of characters
    with open(INPUT_FILE) as f:
        return [list(line.strip()) for line in f]

def get_starting_position(map):
    for y, row in enumerate(map):
        if "^" in row:
            return row.index("^"), y

def create_map(data):
    m = []
    for row in data:
        m.append(["!"] + row + ["!"])
    m.insert(0, ["!"] * len(m[0]))
    m.append(["!"] * len(m[0]))
    return m

def run_simulation(xGuard, yGuard, dirGuard, m, isPart2=False):
    visitedPositions = {(xGuard, yGuard, dirGuard)} if isPart2 else {(xGuard, yGuard)}#set to avoid duplicates
    while True:
        nextItem = m[yGuard+DIRECTIONS[dirGuard][1]][xGuard+DIRECTIONS[dirGuard][0]]
        if nextItem == "!":
            break
        elif nextItem == "#":
            dirGuard = (dirGuard+1)%4
            continue

        xGuard += DIRECTIONS[dirGuard][0]
        yGuard += DIRECTIONS[dirGuard][1]

        if isPart2:
            position = (xGuard, yGuard, dirGuard)
            if position in visitedPositions:
                return "loop found"
            visitedPositions.add(position)
        else:
            visitedPositions.add((xGuard, yGuard))

    return visitedPositions
     
@timed
def part1():
    m = create_map(load_data())
    xGuard, yGuard = get_starting_position(m)
    return list(run_simulation(xGuard, yGuard, 0, m))

@timed
def part2():
    m = create_map(load_data())
    xGuard, yGuard = get_starting_position(m)

    total = 0
    for coord in run_simulation(xGuard, yGuard, 0, m):
        x = coord[0]
        y = coord[1]
        m[y][x] = "#"
        if run_simulation(xGuard, yGuard, 0, m, True) == "loop found":
            total += 1
        m[y][x] = "."
    return total

if __name__ == "__main__":

    # print(part1())
    print(part2())