import re
import sys
import unittest


def solve_p1(fname: str) -> int:
    return 0


def solve_p2(fname: str) -> int:
    return 0


class TestCase(unittest.TestCase):
    def test_p1(self):
        self.assertEqual(solve_p1("test_inputs/day_{{DAY}}.txt"), 0)

    def test_p2(self):
        self.assertEqual(solve_p2("test_inputs/day_{{DAY}}.txt"), 0)


if __name__ == "__main__":
    filename = "inputs/day_{{DAY}}.txt"
    if len(sys.argv) == 1:
        result = "ERROR: Specify part 1 or 2."
    elif sys.argv[1] == '1':
        result = solve_p1(filename)
    elif sys.argv[1] == '2':
        result = solve_p2(filename)
    else:
        result = "ERROR: Specify part 1 or 2."

    print(result)
