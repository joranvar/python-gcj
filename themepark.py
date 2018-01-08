import csv
import math

input_file_name = "/home/joranvar/C-large-practice.in"
output_file_name = "output_large.out"

# divRem(53462345645,45345)
def divRem (num, nom):
    div = num // nom
    rem = num % nom
    reconstruct = nom * div + rem
    print("div = %s, rem = %s, reconstruct = %s"%(div, rem, reconstruct))

def make_groups(k, gs):
    starts = []
    groups = []
    start = 0
    stop = 1 % len(gs)
    while start not in starts:
        current_group_size = gs[start]
        while stop != start and current_group_size + gs[stop] <= k:
            current_group_size += gs[stop]
            stop = (stop + 1) % len(gs)
        starts.append(start)
        groups.append(current_group_size)
        start = stop
        stop = (stop + 1) % len(gs)
    start_index = starts.index(start)
    return (groups[0:start_index], groups[start_index:])

def solve(R, k, gs):
    (prefix, groups) = make_groups(k, gs)
    print((prefix, groups))
    prefix_rides = prefix[0:R]
    R -= len(prefix_rides)
    looping_rides = groups * (R // len(groups))
    suffix_rides = groups[0:(R % len(groups))]
    return sum(prefix_rides) + sum(looping_rides) + sum(suffix_rides)

with open(input_file_name, 'r') as input_file:
    reader = csv.reader(input_file, delimiter=' ')
    T = int(next(reader)[0])
    with open(output_file_name, 'w') as output_file:
        for t in range(T):
            print("Case:%s"%t)
            [R, k, _] = map(int, next(reader))
            gs = list(map(int, next(reader)))
            result = solve(R, k, gs)
            output_file.write("Case #%s: %s\n" % (t+1, result))
