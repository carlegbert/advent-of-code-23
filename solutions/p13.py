import math
import sys
import unittest


def to_bitvec(s: str) -> int:
    return int(s.replace(".", "0").replace("#", "1"), 2)


def is_off_by_1(a: str, b: str) -> bool:
    diff = to_bitvec(a) ^ to_bitvec(b)
    if diff == 0:
        return False

    diff = math.log(diff, 2)
    return math.floor(diff) == diff


def get_sections(fname: str) -> list[list[str]]:
    with open(fname) as fptr:
        return [s.strip().split("\n") for s in fptr.read().split("\n\n")]


def check_mirror_for_offby1(section: list[str], i: int) -> bool:
    stack = section[:i]
    cur = i

    found_the_smudge = False
    while stack and cur < len(section):
        item = stack.pop()
        if not found_the_smudge and is_off_by_1(item, section[cur]):
            found_the_smudge = True
        elif section[cur] != item:
            return False

        cur += 1

    return found_the_smudge


def check_mirror(section: list[str], i: int) -> int:
    stack = section[:i]
    cur = i

    while stack and cur < len(section):
        if section[cur] != stack.pop():
            return 0

        cur += 1

    return i


def horizontal_score(section: list[str], smudged: bool) -> int:
    func = check_mirror_for_offby1 if smudged else check_mirror

    for x in range(1, len(section)):
        if func(section, x):
            return x

    return 0


def vertical_score(section: list[str], smudged: bool) -> int:
    flipped_section = []
    for i in range(len(section[0])):
        s = "".join([section[j][i] for j in range(len(section))])
        flipped_section.append(s)

    return horizontal_score(flipped_section, smudged)


def score(section: list[str], smudged=False) -> int:
    horizontal = horizontal_score(section, smudged)
    if horizontal:
        return horizontal * 100

    return vertical_score(section, smudged)


def solve_p1(fname: str) -> int:
    return sum([score(s) for s in get_sections(fname)])


def solve_p2(fname: str) -> int:
    return sum([score(s, True) for s in get_sections(fname)])


class TestCase(unittest.TestCase):
    def test_p1(self):
        self.assertEqual(solve_p1("test_inputs/day_13.txt"), 405)

    def test_p1_offby1(self):
        self.assertEqual(solve_p1("test_inputs/day_13_b.txt"), 1200)

    def test_p2(self):
        self.assertEqual(solve_p2("test_inputs/day_13.txt"), 400)


if __name__ == "__main__":
    filename = "inputs/day_13.txt"
    if len(sys.argv) == 1:
        result = "ERROR: Specify part 1 or 2."
    elif sys.argv[1] == "1":
        result = solve_p1(filename)
    elif sys.argv[1] == "2":
        result = solve_p2(filename)
    else:
        result = "ERROR: Specify part 1 or 2."

    print(result)
