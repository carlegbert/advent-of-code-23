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

    def roll_west(self) -> None:
        for i, row in enumerate(self.rows):
            new_row = roll_boulders(row)
            self.set_row(i, new_row)

    def roll_south(self) -> None:
        for i, col in enumerate(self.cols):
            col.reverse()
            new_col = roll_boulders(col)
            new_col.reverse()
            self.set_col(i, new_col)

    def roll_east(self) -> None:
        for i, row in enumerate(self.rows):
            row.reverse()
            new_row = roll_boulders(row)
            new_row.reverse()
            self.set_row(i, new_row)

    def cycle(self) -> None:
        self.roll_north()
        self.roll_west()
        self.roll_south()
        self.roll_east()


    def calculate_load(self) -> int:
        return sum([
            column_load(col)
            for col in self.cols
        ])

    def serialize(self) -> tuple[int, ...]:
        return tuple([
            row_to_int(row)
            for row in self.rows
        ])

    def print(self) -> None:
        print("-- grid --")
        for row in self.rows:
            print("".join(row))
        print("----------")


def row_to_int(row: list[str]) -> int:
    s = "".join(row)
    s = s.replace(".", "0").replace("#", "0").replace("O", "1")
    return int(s, 2)


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
    grid.roll_north()

    return grid.calculate_load()


def solve_p2(fname: str) -> int:
    CYCLE_AMOUNT = 1000000000

    grid = get_grid(fname)
    cycles = 0
    repetition_detector: dict[tuple[int, ...], int] = {}
    while cycles < CYCLE_AMOUNT:
        key = grid.serialize()
        if key in repetition_detector:
            repetition_length = cycles - repetition_detector[key]
            cycles_remaining = CYCLE_AMOUNT - cycles
            cycles_remaining %= repetition_length
            for _ in range(cycles_remaining):
                grid.cycle()
            return grid.calculate_load()
        else:
            repetition_detector[key] = cycles

            grid.cycle()
            cycles += 1

    return grid.calculate_load()


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
