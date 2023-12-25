import sys
import unittest
from typing import Generator


Cartesian = tuple[int, int]
Instructions = Generator[Cartesian, None, None]

DIRECTIONS = "RDLU"


def _direction_to_cartesian(d: str) -> Cartesian:
    if d == 'R':
        return (1, 0)
    elif d == 'L':
        return (-1, 0)
    elif d == 'U':
        return (0, 1)
    elif d == 'D':
        return (0, -1)
    else:
        raise Exception(f"Unexpected direction: {d}")


def _move_cartesian(a: Cartesian, b: Cartesian) -> Cartesian:
    ax, ay = a
    bx, by = b
    return ax + bx, ay + by


def p1_instructions(fname: str) -> Generator[Cartesian, None, None]:
    with open(fname) as f:
        for line in f:
            d, n, _ = line.strip().split(" ")
            n = int(n)
            x, y = _direction_to_cartesian(d)
            movement = x * n, y * n
            yield movement


def p2_instructions(fname: str) -> Generator[Cartesian, None, None]:
    with open(fname) as f:
        for line in f:
            s = line.strip().split(" ").pop()
            h = s[2:-2]
            d = s[-2:-1]
            d = DIRECTIONS[int(d)]
            n = int(h, 16)
            x, y = _direction_to_cartesian(d)
            movement = x * n, y * n
            yield movement


def get_vertices(instructions: Instructions) -> list[Cartesian]:
    vertices = []
    cur = 0, 0
    for movement in instructions:
        cur = _move_cartesian(cur, movement)
        vertices.append(cur)

    return vertices


def get_outline_area(instructions: Instructions) -> int:
    return sum([
        abs(x or y)
        for x, y
        in instructions
    ])


def shoelace(vertices: list[Cartesian]) -> int:
    x = [x for x, _ in vertices]
    y = [y for _, y in vertices]
    left = sum([a*b for a, b in zip(x[:-1], y[1:])])
    right = sum([a*b for a, b in zip(x[1:], y[:-1])])

    return abs(left - right) // 2


def solve_p1(fname: str) -> int:
    vertices = get_vertices(p1_instructions(fname))
    outline_area = get_outline_area(p1_instructions(fname))
    return shoelace(vertices) + outline_area // 2 + 1


def solve_p2(fname: str) -> int:
    vertices = get_vertices(p2_instructions(fname))
    outline_area = get_outline_area(p2_instructions(fname))
    return shoelace(vertices) + outline_area // 2 + 1


class TestCase(unittest.TestCase):
    def test_p1(self):
        self.assertEqual(solve_p1("test_inputs/day_18.txt"), 62)

    def test_p2(self):
        self.assertEqual(solve_p2("test_inputs/day_18.txt"), 952408144115)


if __name__ == "__main__":
    filename = "inputs/day_18.txt"
    if len(sys.argv) == 1:
        result = "ERROR: Specify part 1 or 2."
    elif sys.argv[1] == '1':
        result = solve_p1(filename)
    elif sys.argv[1] == '2':
        result = solve_p2(filename)
    else:
        result = "ERROR: Specify part 1 or 2."

    print(result)
