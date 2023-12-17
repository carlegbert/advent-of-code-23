import re
import sys
import unittest


class Grid:
    def __init__(self, rows: list[list[str]]):
        self._rows = rows

    @property
    def height(self) -> int:
        return len(self._rows)

    @property
    def width(self) -> int:
        return len(self._rows[0])

    @property
    def rows(self) -> list[list[str]]:
        return self._rows[:]

    @property
    def cols(self) -> list[list[str]]:
        return [self.get_col(x) for x in range(self.width)]

    def get_row(self, idx: int) -> list[str]:
        return self._rows[idx][:]

    def get_col(self, idx: int) -> list[str]:
        return [row[idx] for row in self.rows]

    def set_row(self, idx: int, row: list[str]):
        self._rows[idx] = row

    def set_col(self, idx: int, col: list[str]):
        for y in range(self.height):
            self._rows[y][idx] = col[y]

    def roll_north(self) -> None:
        for i, col in enumerate(self.cols):
            new_col = roll_boulders(col)
            self.set_col(i, new_col)

    def calculate_load(self) -> int:
        return sum([
            column_load(col)
            for col in self.cols
        ])

    def print(self) -> None:
        print("-- grid --")
        for row in self.rows:
            print("".join(row))
        print("----------")


def get_grid(fname: str) -> Grid:
    with open(fname) as fptr:
        return Grid([list(line.strip()) for line in fptr])


def roll_boulders(row_or_col: list[str]) -> list[str]:
    s = "".join(row_or_col)
    parts = s.split("#")
    parts = ["".join(sorted(p, reverse=True)) for p in parts]
    s = "#".join(parts)
    return list(s)


def column_load(col: list[str]) -> int:
    height = len(col)
    return sum([
        height - i
        for i, c in enumerate(col)
        if c == "O"
    ])


def solve_p1(fname: str) -> int:
    grid = get_grid(fname)
    grid.print()
    grid.roll_north()
    grid.print()

    return grid.calculate_load()


def solve_p2(fname: str) -> int:
    return 0


class TestCase(unittest.TestCase):
    def test_p1(self):
        self.assertEqual(solve_p1("test_inputs/day_14.txt"), 136)

    def test_p2(self):
        self.assertEqual(solve_p2("test_inputs/day_14.txt"), 64)


if __name__ == "__main__":
    filename = "inputs/day_14.txt"
    if len(sys.argv) == 1:
        result = "ERROR: Specify part 1 or 2."
    elif sys.argv[1] == "1":
        result = solve_p1(filename)
    elif sys.argv[1] == "2":
        result = solve_p2(filename)
    else:
        result = "ERROR: Specify part 1 or 2."

    print(result)
