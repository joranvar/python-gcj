import csv

input_file_name = "/home/joranvar/A-large-practice.in"
output_file_name = "output_large.txt"

def solve(n, k):
    if (k+1) % (2**n) == 0:
        return "ON"
    else:
        return "OFF"

with open(input_file_name, 'r') as input_file:
    reader = csv.reader(input_file, delimiter=' ')
    _ = next(reader)
    r = 0
    with open(output_file_name, 'w') as output_file:
        for row in reader:
            r = r + 1
            result = solve(int(row[0]), int(row[1]))
            output_file.write("Case #%s: %s\n" % (r, result))
