#!/bin/bash

# Exit full script on interrupt
trap "exit" INT

# Check for args
if [ $# -eq 0 ]; then
    echo "Usage: $0 <test-directory>"
    exit 1
fi

# Get test files
testDir=$1
testFiles="$(find $testDir -type f -name "*.rkt")"
numTestFiles="$(echo "$testFiles" | wc -l)"

# Helper variables
breakLine="-------------------------------------------\n"
exitCode=0

# Print header of test files found
echo -e "Found the following $numTestFiles test files:\n$breakLine$testFiles\n$breakLine"

# Run tests and report progress
for testFile in $testFiles; do
    echo -e "\nRunning $testFile"
    
    racket $testFile > /dev/null

    if [[ $? ]]; then
        exitCode=1
    fi
done

exit $exitCode
