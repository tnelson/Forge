#!/usr/local/bin/python3

import os
from subprocess import Popen, PIPE, check_output

########

def forgeW(cmd):
    # TODO: make this interactive with subprocess.Popen
    cmd = f"{cmd} --write"
    for line in check_output(cmd.split()).decode().splitlines():
        if line.startswith("INSTANCE : "):
            line = line[len("INSTANCE : "):]
            yield(line)

def forgeR(cmd, inst):
    os.system(f"{cmd} --read {inst}")

def isotest(cmd1, cmd2):
    for inst in forgeW(cmd1):
        # forgeR(cmd2)
        print(inst)

########

if __name__ == "__main__":
    import sys

    cmd1 = sys.argv[1]
    cmd2 = sys.argv[2]

    isotest(cmd1, cmd2)






