import re


class Interval:
    def __init__(self, floor: int, ceil: int):
        self.floor = floor
        self.ceil = ceil

    @classmethod
    def from_floor_and_offset(cls, f: int, offset: int) -> 'Interval':
        return cls(f, f + offset)

    def overlaps_from_below(self, other: 'Interval') -> bool:
        return self.ceil > other.floor and self.floor <= other.floor

    def overlaps_from_above(self, other: 'Interval') -> bool:
        return other.overlaps_from_below(self)

    def encompasses(self, other: 'Interval') -> bool:
        return self.floor <= other.floor and self.ceil >= other.ceil

    def within(self, other: 'Interval') -> bool:
        return other.encompasses(self)

    def apply_offset(self, offset: int) -> 'Interval':
        return Interval(
            self.floor + offset,
            self.ceil + offset,
        )

    def __str__(self) -> str:
        return f"<Interval: {self.floor} - {self.ceil}>"

    def send_through_mapping(
        self, mapping: 'IntervalMapping'
    ) -> tuple[list['Interval'], list['Interval']]:
        if self.encompasses(mapping.interval):
            return [
                [
                    Interval(self.floor, mapping.interval.floor),
                    Interval(mapping.interval.ceil, self.ceil)
                ],
                [mapping.interval.apply_offset(mapping.offset)],
            ]
        elif self.within(mapping.interval):
            return [[], [self.apply_offset(mapping.offset)]]
        elif self.overlaps_from_above(mapping.interval):
            return [
                [Interval(mapping.interval.ceil, self.ceil)],
                [Interval(self.floor, mapping.interval.ceil).apply_offset(mapping.offset)]
            ]
        elif self.overlaps_from_below(mapping.interval):
            return [
                [Interval(self.floor, mapping.interval.floor)],
                [Interval(mapping.interval.floor, self.ceil).apply_offset(mapping.offset)]
            ]
        else:
            return [[self], []]

class IntervalMapping:
    def __init__(self, interval: Interval, offset: int):
        self.interval = interval
        self.offset = offset


def parse_initial_intervals(s: str) -> list[Interval]:
    matches = re.compile("(\d+) (\d+)").findall(s)
    intervals = []
    for m in matches:
        nums = [int(n) for n in m]
        intervals.append(Interval.from_floor_and_offset(*nums))

    return intervals


def parse_mapping(s: str) -> IntervalMapping:
    matches = re.compile("\d+").findall(s)
    nums = [int(m) for m in matches]
    interval = Interval.from_floor_and_offset(nums[1], nums[2])
    return IntervalMapping(
        interval,
        nums[0] - nums[1]
    )


def solve(fname):
    fptr = open(fname)

    iterator = iter(fptr)
    intervals = parse_initial_intervals(next(iterator))
    mapped_intervals = []

    for line in iterator:
        if not line.strip() or ":" in line:
            intervals.extend(mapped_intervals)
            mapped_intervals = []
            continue

        interval_mapping = parse_mapping(line)
        new_intervals = []
        for interval in intervals:
            unmapped, mapped = interval.send_through_mapping(interval_mapping)
            new_intervals.extend(unmapped)
            mapped_intervals.extend(mapped)

        intervals = new_intervals

    fptr.close()

    intervals.extend(mapped_intervals)
    return sorted([i.floor for i in intervals])[0]



if __name__ == "__main__":
    print(solve("inputs/day_5.txt"))
