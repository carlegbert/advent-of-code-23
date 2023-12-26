import abc
from collections import deque
import re
import sys
from typing import Deque
import unittest
from enum import Enum


class PulseType(Enum):
    low = "LOW"
    high = "HIGH"

# dest, type, sender
Command = tuple[str, PulseType, str]


class AbstractModule(abc.ABC):
    key: str
    destinations: list[str]

    def handle(self, command: Command) -> list[Command]:
        return self._handle(command)

    @abc.abstractmethod
    def _handle(self, command: Command) -> list[Command]:
        ...

    def send(self, pulse_type: PulseType) -> list[Command]:
        return [(dest, pulse_type, self.key) for dest in self.destinations]


class FlipFlopModule(AbstractModule):
    def __init__(self, key: str, destinations: list[str]):
        self.key = key
        self.destinations = destinations

        self.is_on = False

    def _handle(self, command: Command) -> list[Command]:
        _, pulse, _ = command
        if pulse == PulseType.high:
            return []

        pulse_type = PulseType.low if self.is_on else PulseType.high

        self.is_on = not self.is_on

        return self.send(pulse_type)

    def __repr__(self) -> str:
        return f"FlipFlopModule({self.key}, {self.destinations})"


class ConjunctionModule(AbstractModule):
    def __init__(self, key: str, destinations: list[str]):
        self.key = key
        self.destinations = destinations

        self.memory = {}

    def all_high(self) -> bool:
        return all([p == PulseType.high for p in self.memory.values()])

    def _handle(self, command: Command) -> list[Command]:
        _, p, sender = command
        self.memory[sender] = p

        pulse_type = PulseType.low if self.all_high() else PulseType.high
        return self.send(pulse_type)

    def __repr__(self) -> str:
        return f"ConjunctionModule({self.key}, {self.destinations})"


class BroadcastModule(AbstractModule):
    def __init__(self, key: str, destinations: list[str]):
        self.key = key
        self.destinations = destinations

    def _handle(self, command: Command) -> list[Command]:
        _, p, _ = command

        return self.send(p)

    def __repr__(self) -> str:
        return f"BroadcastModule({self.key}, {self.destinations})"


class ButtonModule(AbstractModule):
    def __init__(self):
        self.key = 'button'
        self.destinations = ['broadcaster']

    def _handle(self, command):
        return self.send(PulseType.low)

    def __repr__(self) -> str:
        return f"ButtonModule({self.key}, {self.destinations})"


class OutputModule(AbstractModule):
    def __init__(self):
        self.key = 'output'
        self.destinations = []

    def _handle(self, command):
        return []


class RxModule(AbstractModule):
    def __init__(self):
        self.is_on = False
        self.destinations = []

    def _handle(self, command):
        _, p, _ = command
        if p == PulseType.low:
            self.is_on = True

        return []


def build_module_map(fname: str) -> dict[str, AbstractModule]:
    result: dict[str, AbstractModule] = {
        'button': ButtonModule(),
        'output': OutputModule(),
        'rx': RxModule(),
    }

    with open(fname) as f:
        for line in f:
            key, dests = line.split(" -> ")
            dests = dests.strip().split(", ")

            if key == 'broadcaster':
                result[key] = BroadcastModule(key, dests)
            elif key[0] == '%':
                result[key[1:]] = FlipFlopModule(key[1:], dests)
            elif key[0] == '&':
                result[key[1:]] = ConjunctionModule(key[1:], dests)

    for sender in result.values():
        for d in sender.destinations:
            d = result.get(d)
            if not d:
                continue
            if type(d) == ConjunctionModule:
                d.memory[sender.key] = PulseType.low

    return result


def solve_p1(fname: str) -> int:
    module_map = build_module_map(fname)
    low = 0
    high = 0

    for _ in range(1000):
        commands: Deque[Command] = deque([("button", PulseType.low, "<button press>")])
        while commands:
            command = commands.popleft()
            key, _, _ = command

            handler = module_map.get(key)
            if not handler:
                continue

            new_commands = handler.handle(command)
            low += len([_ for _, p, _ in new_commands if p == PulseType.low])
            high += len([_ for _, p, _ in new_commands if p == PulseType.high])
            commands.extend(new_commands)

    return low * high


def solve_p2(fname: str) -> int:
    return 0


class TestCase(unittest.TestCase):
    def test_p1(self):
        self.assertEqual(solve_p1("test_inputs/day_20.txt"), 32000000)

    def test_p1_complex(self):
        self.assertEqual(solve_p1("test_inputs/day_20_b.txt"), 11687500)

    def test_p2(self):
        self.assertEqual(solve_p2("test_inputs/day_20.txt"), 0)


if __name__ == "__main__":
    filename = "inputs/day_20.txt"
    if len(sys.argv) == 1:
        result = "ERROR: Specify part 1 or 2."
    elif sys.argv[1] == '1':
        result = solve_p1(filename)
    elif sys.argv[1] == '2':
        result = solve_p2(filename)
    else:
        result = "ERROR: Specify part 1 or 2."

    print(result)
