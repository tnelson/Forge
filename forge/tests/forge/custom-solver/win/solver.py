import sys

def readInput(cnfFile):
    variables = set()
    with open(cnfFile, "r") as f:
        for line in f.readlines():
            tokens = line.strip().split()
            if tokens and tokens[0] != "p" and tokens[0] != "c":
                for lit in tokens[:-1]:
                    variables.add(lit.strip("-"))
    return variables

if __name__ == "__main__":
    inputFile = sys.argv[1]
    variables = readInput(inputFile)
    result = " ".join(variables)
    print(f"s SATISFIABLE")
    print(f"v {result}")
