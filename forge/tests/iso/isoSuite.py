#!/usr/local/bin/python3

########

from collections import namedtuple
from isotest import *

########

# test entries are tuples (expectedResult, cmd1, cmd2)
Test = namedtuple('Test', ['expect', 'cmd1', 'cmd2'])

# creates a test entry to compare boundsy vs not
def boundsy(file): return Test(True, f'racket {file}', f'racket {file} --boundsy 0')
# creates a test entry to compare 2 files without flags
def simple(exp, file1, file2): return Test(exp, f'racket {file1}', f'racket {file2}')

########

tests = [
    boundsy('files/AB_func.rkt'),
    boundsy('files/AB_inj.rkt'),
    boundsy('files/AB_surj.rkt'),
    boundsy('files/AB_bij.rkt'),
    boundsy('files/AB_pbij.rkt'),

    boundsy('files/A_tree.rkt'),
    boundsy('files/A_ref.rkt'),       # FIXME: FAIL! Testing: J=#hash((A . ((A0) (A1) (A2))) (Int . ((-8) (-7) (-6) (-5) (-4) (-3) (-2) (-1) (0) (1) (2) (3) (4) (5) (6) (7))) (f . ((A0 A2) (A1 A0) (A1 A1)))) Failed: No such I # we should get such an I
    boundsy('files/A_acyclic.rkt'),
    boundsy('files/A_linear.rkt'),

    boundsy('files/ABC_func.rkt'),

    boundsy('files/isoEx.rkt'),
    boundsy('files/isoEx2.rkt'),
    simple(False, 'files/isoEx.rkt', 'files/isoEx2.rkt'),
]

def runSuite():
    failed = []
    for test in tests:
        (exp, cmd1, cmd2) = test
        print("-"*32)
        print(f'Expecting {"ISO" if exp else "DIFF"}: "{cmd1}" VS "{cmd2}"')
        print()
        res = isotest(cmd1, cmd2)
        print()
        print(f'Got {"ISO" if res else "DIFF"} --> {"SUCCESS" if res==exp else "FAILURE"}')
        print("-"*32)
        if res != exp: failed.append(test)
    if failed:
        print(":( :( :( SOME TESTS FAILED :( :( :(")
        for test in failed: print(test)
    else:
        print(":) :) :) ALL TESTS PASSED :) :) :)")

########

if __name__ == '__main__':
    runSuite()