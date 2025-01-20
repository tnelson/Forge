from sys import argv
import re


def updateVersion(curVersion, updateType, prerelease=None):
    # updateType = major | minor | patch
    major = None
    minor = None
    patch = None

    # read existing version
    cnt = 0
    for v in re.split("[\.\-]", curVersion):
        v = v.strip()

        if cnt == 0:
            major = int(v)
        elif cnt == 1:
            minor = int(v)
        elif cnt == 2:
            patch = int(v)
        elif cnt >= 4:
            raise Exception("Invalid version string given")
        cnt += 1
    
    # update version
    if updateType == "major":
        if major != None: major += 1 
        else: major = 1
        minor = 0
        patch = None
    elif updateType == "minor":
        if minor != None: minor += 1
        else: minor = 0
        patch = None
    elif updateType == "patch":
        if patch != None: patch += 1
        else: patch = 1
    else:
        raise Exception("Invalid version update type given")

    # generate new version
    newVer = f"{major}"
    if minor is not None:
        newVer += f".{minor}"
    if patch is not None:
        newVer += f".{patch}"
    if prerelease is not None:
        newVer += f"-{prerelease}"

    return newVer


def writeVersion(filename, updateType, prerelease=None):
    fileInfo = []
    with open(filename, "r") as file:
        for line in file.readlines():
            fileInfo.append(line)
    

    for i in range(len(fileInfo)):
        line = fileInfo[i]
        match = re.match(r'^\(define version "(\d+\.\d+(\.\d+)?(\-[a-z0-9]*)?)"\)$', line)
        if match:
            newVer = updateVersion(match.group(1), updateType, prerelease)
            fileInfo[i] = f'(define version "{newVer}")\n'
            print(newVer)
            break;
    
    with open(filename, "w") as file:
        file.writelines(fileInfo)
        
    


if __name__ == "__main__":
    if len(argv) == 3:
        file = argv[1]
        updateType = argv[2]
        prerelease = None
    elif len(argv) == 4:
        file = argv[1]
        updateType = argv[2]
        prerelease = argv[3]
    else:
        raise Exception("Invalid number of args given. Must be either 2 or 3")

    writeVersion(file, updateType, prerelease=prerelease)
