import math
import sys
import unittest
from typing import Union

VRange = tuple[int, int]
Rule = tuple[str, str, Union[int, float], str]
Workflow = list[Rule]
Part = dict[str, VRange]


def parse_rule(s: str) -> Rule:
    part_category = s[0]
    c = s[1]
    val, key = s[2:].split(":")
    val = int(val)
    return part_category, c, val, key


def parse_workflow(s: str) -> tuple[str, Workflow]:
    key, rest = s.split("{")
    rest = rest[:-1]
    rule_parts = rest.split(",")
    default_key = rule_parts.pop()
    default_rule: Rule = 'a', '<', math.inf, default_key
    rules = [parse_rule(x) for x in rule_parts]

    return key, [*rules, default_rule]


def build_workflow_map(fname: str) -> dict[str, Workflow]:
    result: dict[str, Workflow] = {}
    with open(fname) as fptr:
        s = fptr.read()
        s = s.split("\n\n")[0]
        for line in s.split("\n"):
            key, workflow = parse_workflow(line)
            result[key] = workflow

    return result


def partial_rule_match(rule: Rule, part: Part) -> tuple[Part, Part]:
    category, comparator, value, _ = rule
    to_compare = part[category]
    if comparator == "<":
        successful_range = to_compare[0], value - 1
        unsuccessful_range = value, to_compare[1]
    else:
        successful_range = value+1, to_compare[1]
        unsuccessful_range = to_compare[0], value

    unsuccessful = {**part, category: unsuccessful_range}
    successful = {**part, category: successful_range}

    return unsuccessful, successful


def fails_rule(rule: Rule, part: Part) -> bool:
    category, comparator, value, _ = rule
    to_compare = part[category]
    if comparator == ">":
        return to_compare[1] < value

    return to_compare[0] > value


def passes_rule(rule: Rule, part: Part) -> bool:
    category, comparator, value, _ = rule
    to_compare = part[category]
    if comparator == ">":
        return to_compare[0] > value

    return to_compare[1] < value


def count_combos(part: Part) -> int:
    result = 1
    for f, c in part.values():
        n = 1 + c - f
        result *= n

    return result


def solve_p1(fname: str) -> int:
    return 0


def solve_p2(fname: str) -> int:
    workflows = build_workflow_map(fname)
    initial_range: Part = {
        'x': (1, 4000),
        'm': (1, 4000),
        'a': (1, 4000),
        's': (1, 4000),
    }
    initial = 'in', initial_range
    stack: list[tuple[str, Part]] = [initial]
    accepted = []

    while stack:
        workflow_key, part = stack.pop()
        if workflow_key == 'R':
            continue
        if workflow_key == 'A':
            accepted.append(part)
            continue

        for rule in workflows[workflow_key]:
            if passes_rule(rule, part):
                stack.append((rule[3], part))
                break

            if fails_rule(rule, part):
                continue

            unsuccessful, successful = partial_rule_match(rule, part)
            stack.append((rule[3], successful))
            part = unsuccessful

    return sum([count_combos(x) for x in accepted])


class TestCase(unittest.TestCase):
    def test_p1(self):
        self.assertEqual(solve_p1("test_inputs/day_19.txt"), 0)

    def test_p2(self):
        self.assertEqual(solve_p2("test_inputs/day_19.txt"), 167409079868000)


if __name__ == "__main__":
    filename = "inputs/day_19.txt"
    if len(sys.argv) == 1:
        result = "ERROR: Specify part 1 or 2."
    elif sys.argv[1] == '1':
        result = solve_p1(filename)
    elif sys.argv[1] == '2':
        result = solve_p2(filename)
    else:
        result = "ERROR: Specify part 1 or 2."

    print(result)
