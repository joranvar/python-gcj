import csv
import math

input_file_name = "/home/joranvar/B-large-practice.in"
output_file_name = "output_large.out"

def distances(ts):
    t = ts[0]
    for t2 in ts[1:]:
        yield t2 - t
        t = t2

def gcd(distances):
    gcd = distances[0]
    for d in distances[1:]:
        gcd = math.gcd(gcd, d)
    return gcd

def solve(n, ts):
    sorted_ts = sorted(ts)
    last_event = min(sorted_ts)
    gcd_ts = gcd(list(distances(sorted_ts)))
    # Try to fit the last_event distance to the left of the zero line:
    # mod by a negative
    return -(last_event % -gcd_ts)

with open(input_file_name, 'r') as input_file:
    reader = csv.reader(input_file, delimiter=' ')
    _ = next(reader)
    r = 0
    with open(output_file_name, 'w') as output_file:
        for row in reader:
            r = r + 1
            result = solve(int(row[0]), list(map(int, row[1:])))
            output_file.write("Case #%s: %s\n" % (r, result))
