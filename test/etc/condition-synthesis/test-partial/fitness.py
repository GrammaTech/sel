#!/usr/bin/env python

import argparse
import subprocess
import sys

parser = argparse.ArgumentParser(description='Fitness test')
parser.add_argument('--count', action='store_true',
                    help='Print number of test cases')
parser.add_argument('exe', metavar='EXE', help='The program to test',
                    nargs='?')
parser.add_argument('which', type=int, nargs='?', default=None,
                    help='Run just this test case')

args = parser.parse_args()

def do_test(exe, arg, *expected):
    lines = subprocess.check_output([exe, str(arg)]).decode('utf-8').strip().split("\n")
    return sum(val == expected for val, expected in zip(lines, expected)) / 2.0

tests = [(-5, "x is 5 or smaller", "x is odd"),
         (0, "x is 5 or smaller", "x is even"),
         (3, "x is 5 or smaller", "x is odd"),
         (4, "x is 5 or smaller", "x is even"),
         (5, "x is 5 or smaller", "x is odd"),
         (6, "x is larger than 5", "x is even")]

if args.count:
    print(len(tests))
    sys.exit(0)

exe = args.exe

if args.which is None:
    score = sum(do_test(exe, *f) for f in tests) / float(len(tests))
else:
    score = do_test(exe, *tests[args.which])

print(score)
