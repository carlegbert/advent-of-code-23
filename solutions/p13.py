import sys
import unittest


def get_sections(fname: str) -> list[list[str]]:
    with open(fname) as fptr:
        return [
            s.strip().split("\n")
            for s in fptr.read().split("\n\n")
        ]


def check_mirror(section: list[str], i: int) -> int:
    stack = section[:i]
    cur = i

    while stack and cur < len(section):
        if section[cur] != stack.pop():
            return 0
        cur += 1

    return i


def horizontal_score(section: list[str]) -> int:
    return sum([
        check_mirror(section, x)
        for x in range(1, len(section))
    ])


def vertical_score(section: list[str]) -> int:
    flipped_section = []
    for i in range(len(section[0])):
        s = "".join([
            section[j][i]
            for j in range(len(section))
        ])
        flipped_section.append(s)

    return horizontal_score(flipped_section)


def score(section: list[str]) -> int:
    horizontal  = 100 * horizontal_score(section)
    vertical = vertical_score(section)

    return horizontal + vertical


def solve_p1(fname: str) -> int:
    return sum([
        score(s) for s in get_sections(fname)
    ])


def solve_p2(fname: str) -> int:
    return 0


class TestCase(unittest.TestCase):
    def test_p1(self):
        self.assertEqual(solve_p1("test_inputs/day_13.txt"), 405)

    def test_p1_offby1(self):
        self.assertEqual(solve_p1("test_inputs/day_13_b.txt"), 1200)

    def test_p2(self):
        self.assertEqual(solve_p2("test_inputs/day_13.txt"), 0)


if __name__ == "__main__":
    filename = "inputs/day_13.txt"
    if len(sys.argv) == 1:
        result = "ERROR: Specify part 1 or 2."
    elif sys.argv[1] == '1':
        result = solve_p1(filename)
    elif sys.argv[1] == '2':
        result = solve_p2(filename)
    else:
        result = "ERROR: Specify part 1 or 2."

    print(result)
