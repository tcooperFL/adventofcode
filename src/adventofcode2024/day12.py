# Link: https://adventofcode.com/2024/day/12

from utils import timed

INPUT_FILE = "resources/2024/day12-input.txt"


class Region:
    _id_counter = 1
    _all_regions: dict[int, object] = dict()

    def __init__(self, m, y, x):
        self.map = m
        self.label = m[y][x]
        self.plots = {(y, x)}
        self.perimeter = 4
        self.id = Region._id_counter
        Region._id_counter += 1
        m[y][x] = self
        Region._all_regions[self.id] = self

    def absorb(self, r):
        # If the labels are the same, merge the regions and return the merged region
        # else return the region that was passed in
        if self.label == r.label:
            if self.id != r.id:
                for y, x in r.plots:
                    # Replace the region in all plots in the added region with this region
                    # and replace the region in the map with this region
                    self.plots.add((y, x))
                    self.map[y][x] = self
                self.perimeter += r.perimeter
                del Region._all_regions[r.id]

            self.perimeter -= 2
            return self
        return r

    def _extend_side(
        self, side: dict[int, list[tuple[int, int]]], fixed: int, leading: int
    ):
        # Edges are kept in sorted order.
        if edges := side.get(fixed):
            first_edge = edges[0]
            # If ahead, insert a new edge
            if leading + 1 < first_edge[0]:
                edges.insert(0, (leading, leading + 1))
            else:
                # Linear search for a bucket to extend or create a new one
                for i in range(len(edges)):
                    edge = edges[i]
                    if edge[1] == leading:
                        edges[i] = (edge[0], leading + 1)
                        break
                else:
                    side[fixed].append((leading, leading + 1))
        else:
            side[fixed] = [(leading, leading + 1)]

    @property
    def num_sides(self):
        sides = {"top": {}, "bottom": {}, "left": {}, "right": {}}
        plots = sorted(self.plots, key=lambda p: (p[1], p[0]))  # needed?

        for y, x in plots:
            if (y - 1, x) not in self.plots:
                self._extend_side(sides["top"], y, x)
            if (y + 1, x) not in self.plots:
                self._extend_side(sides["bottom"], y + 1, x)
            if (y, x - 1) not in self.plots:
                self._extend_side(sides["left"], x, y)
            if (y, x + 1) not in self.plots:
                self._extend_side(sides["right"], x + 1, y)

        return sum(len(edge) for side in sides.values() for edge in side.values())

    def price_by_perimeter(self):
        return len(self.plots) * self.perimeter

    def price_by_sides(self):
        return len(self.plots) * self.num_sides

    @classmethod
    def reset_regions(cls):
        cls._id_counter = 1
        cls._all_regions.clear()

    @classmethod
    def get_all_regions(cls):
        return cls._all_regions.values()


def get_plots():
    with open(INPUT_FILE) as f:
        return [list(line.strip()) for line in f]


def combine_regions():
    m = get_plots()

    # Group all adjacent plots with matching labels into regions and return them.
    Region.reset_regions()

    for y in range(len(m)):
        for x in range(len(m[0])):
            r = Region(m, y, x)

            # Try merge to the left
            if x > 0:
                r = m[y][x - 1].absorb(r)

            # Then try to merge up.
            if y > 0:
                m[y - 1][x].absorb(r)

    return Region.get_all_regions()


@timed
def solution():
    regions = combine_regions()
    return {
        "Part 1": sum(r.price_by_perimeter() for r in regions),
        "Part 2": sum(r.price_by_sides() for r in regions),
    }


if __name__ == "__main__":
    print(solution())
