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
    boundsy('isoEx.rkt'),
    boundsy('isoEx2.rkt'),
    simple(False, 'isoEx.rkt', 'isoEx2.rkt')
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