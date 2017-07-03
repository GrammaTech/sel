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
    try:
        lines = subprocess.check_output('set -e; timeout 0.01s %s %s' % (exe, arg),
                                        shell=True).decode('utf-8').split('\n');
        # [exe, str(arg)]).strip().split("\n")
        return sum(val == expected for val, expected in zip(lines, expected)) / 2.0
    except subprocess.CalledProcessError:
        return 0

tests = [(2, "quotient: 0", "remainder: 2"),
         (3, "quotient: 1", "remainder: 0"),
         (5, "quotient: 1", "remainder: 2"),
         (6, "quotient: 2", "remainder: 0"),
         (7, "quotient: 2", "remainder: 1")]
if args.count:
    print(len(tests))
    sys.exit(0)

exe = args.exe

if args.which is None:
    score = sum(do_test(exe, *f) for f in tests) / float(len(tests))
else:
    score = do_test(exe, *tests[args.which])

print(score)
