from __future__ import annotations

import heapq
import sys
import unittest
from typing import Generator


Point = tuple[int, int]
Position = tuple[int, Point, Point]
Grid = list[list[str]]


def build_grid(fname) -> list[list[str]]:
    with open(fname) as fptr:
        return [
            list(line.strip())
            for line in fptr
        ]


def in_grid(point: Point, grid: Grid) -> bool:
    x, y = point
    if x < 0 or y < 0:
        return False

    return x < len(grid[0]) and y < len(grid)


def get_available_next_moves(p: Position, g: Grid, min_moves: int, max_moves: int) -> Generator[Position, None, None]:
    heat_loss, current, previous = p

    key = int(current[1] == previous[1])
    h = heat_loss
    for i in range(1, max_moves+1):
        new_point = list(current)
        new_point[key] += i
        new_point = new_point[0], new_point[1]
        if not in_grid(new_point, g):
            break

        h += int(g[new_point[1]][new_point[0]])
        if i < min_moves:
            continue

        yield h, new_point, current

    h = heat_loss
    for i in range(1, max_moves+1):
        new_point = list(current)
        new_point[key] -= i
        new_point = new_point[0], new_point[1]
        if not in_grid(new_point, g):
            break

        h += int(g[new_point[1]][new_point[0]])
        if i < min_moves:
            continue

        yield h, new_point, current


def solve(grid: Grid, min_moves: int, max_moves: int) -> int:
    target: Point = (len(grid[0]) - 1, len(grid) - 1)
    visited: set[tuple[Point, Point]] = set()
    positions: list[Position] = [
        *list(get_available_next_moves((0, (0, 0), (-1, 0)), grid, min_moves, max_moves)),
        *list(get_available_next_moves((0, (0, 0), (0, -1)), grid, min_moves, max_moves)),
    ]
    heapq.heapify(positions)

    while positions:
        p = heapq.heappop(positions)
        heat_loss, current, previous = p

        if current == target:
            return heat_loss

        key = current, previous

        if key not in visited:
            visited.add(key)
            for new_p in get_available_next_moves(p, grid, min_moves, max_moves):
                heapq.heappush(positions, new_p)

    raise Exception("Unexpectedly ran out of paths to try.")


def solve_p1(fname: str) -> int:
    grid = build_grid(fname)
    return solve(grid, 1, 3)


def solve_p2(fname: str) -> int:
    grid = build_grid(fname)
    return solve(grid, 4, 10)


class TestCase(unittest.TestCase):
    def test_p1(self):
        self.assertEqual(solve_p1("test_inputs/day_17.txt"), 102)

    def test_p2(self):
        self.assertEqual(solve_p2("test_inputs/day_17.txt"), 94)

    def test_p2_simple(self):
        self.assertEqual(solve_p2("test_inputs/day_17_b.txt"), 71)


if __name__ == "__main__":
    filename = "inputs/day_17.txt"
    if len(sys.argv) == 1:
        result = "ERROR: Specify part 1 or 2."
    elif sys.argv[1] == "1":
        result = solve_p1(filename)
    elif sys.argv[1] == "2":
        result = solve_p2(filename)
    else:
        result = "ERROR: Specify part 1 or 2."

    print(result)
