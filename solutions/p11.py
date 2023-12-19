import re
import sys
import unittest


def _count_combos(
    s: str,
    expected_spring_groups: list[int],
    memo: dict[tuple, int],
) -> int:
    print(s)
    key = (s, tuple(expected_spring_groups))
    if key in memo:
        return memo[key]

    first_group = expected_spring_groups[0] if expected_spring_groups else 0
    expected_string = "#" * first_group

    if not s:
        val = not expected_spring_groups
    elif s[0] == ".":
        val = _count_combos(s[1:], expected_spring_groups, memo)
    elif "?" in s:
        v1 = _count_combos(s.replace("?", ".", 1), expected_spring_groups, memo)
        v2 = _count_combos(s.replace("?", "#", 1), expected_spring_groups, memo)
        val = v1 + v2
    elif s.split(".").pop() == expected_string:
        val = _count_combos(s[first_group:], expected_spring_groups[1:], memo)
    else:
        val = 0

    memo[key] = val
    return val


def count_combos(
    groups: list[str],
    expected_spring_groups: list[int],
    memo: dict[str, int],
) -> int:
    group_lens = [len(g) for g in groups]
    key = (tuple(expected_spring_groups), tuple(groups))
    if key in memo:
        return memo[key]

    if group_lens == expected_spring_groups:
        val = 1
    elif not groups or not expected_spring_groups:
        val = 0
    elif group_lens[0] == expected_spring_groups[0]:
        val = count_combos(groups[1:], expected_spring_groups[1:], memo)
    elif group_lens[0] < expected_spring_groups[0]:
        val = 0
    elif "?" in groups[0]:
        print(groups[0])
        s1 = groups[0].replace("?", "#", 1)
        print(s1)
        new_groups = [g for g in groups[0].replace("?", ".", 1).split(".") if g]
        print(new_groups)
        print("***")
        val1 = count_combos([s1] + groups[1:], expected_spring_groups, memo)
        val2 = count_combos(new_groups + groups[1:], expected_spring_groups, memo)
        val = val1 + val2
    else:
        val = 0

    memo[key] = val
    return val


def combos_per_line(s: str) -> int:
    parts = s.split(" ")
    nums = [int(x) for x in parts[1].split(",")]
    groups = [x for x in parts[0].split(".") if x]
    return _count_combos(parts[0], nums, {})


def solve_p1(fname: str) -> int:
    with open(fname) as fptr:
        return sum(
            combos_per_line(s)
            for s in fptr
        )


class TestCase(unittest.TestCase):
    def test_single_line_no_wildcard(self):
        self.assertEqual(combos_per_line("#.#.### 1,1,3"), 1)

    def test_single_line_wildcard(self):
        self.assertEqual(combos_per_line("???.### 1,1,3"), 1)

    def test_example(self):
        self.assertEqual(solve_p1("test_inputs/day_12.txt"), 21)


if __name__ == "__main__":
    filename = "inputs/day_12.txt"
    print(solve_p1(filename))

