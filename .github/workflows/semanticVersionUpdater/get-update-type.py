from sys import argv
from os import environ
import re


def parseUpdateType(string):
    words = string.strip().split()
    
    match = re.match(r'\[(major|minor|patch)(-([a-zA-z0-9]+))?\]', words[0])
    if match:
        updateType = match.group(1)
        prerelease = match.group(3)

        return updateType, prerelease    
    else:
        return "minor", None

if __name__ == "__main__":
    prTitle = argv[1]

    updateType, prerelease = parseUpdateType(prTitle)
    print(f"{updateType} {prerelease if prerelease else ''}")