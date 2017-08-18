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

def do_test(exe, arg, expected):
    if expected == subprocess.check_output([exe, str(arg)]).decode('utf-8').strip():
        return 1
    else:
        return 0

tests = [(-5, "x is smaller than 5"),
         (0, "x is smaller than 5"),
         (3, "x is smaller than 5"),
         (4, "x is smaller than 5"),
         (5, "x is 5 or larger"),
         (6, "x is 5 or larger")]

if args.count:
    print(len(tests))
    sys.exit(0)

exe = args.exe

if args.which is None:
    score = sum(do_test(exe, *f) for f in tests) / float(len(tests))
else:
    score = do_test(exe, *tests[args.which])

print(score)
exit(1 - score)
