#!/bin/bash

# Exit full script on interrupt
trap "exit" INT

# Get test files
testDir=$1
testFiles="$(find $testDir -type f -name "*.rkt")"
numTestFiles="$(echo "$testFiles" | wc -l)"

# Helper variables
_breakLine="-------------------------------------------\n"
_numTestsCompleted=0
_progressBarSize=40

# Print header of test files found
echo -e "Found the following $numTestFiles test files:\n$_breakLine$testFiles\n$_breakLine"

# Helper functions
repeatChar () {
     echo -n "$(printf "$1%.0s" $(seq 1 $2))"
}

printBar () {
    barSize="$((($_progressBarSize * $1) / $2))"
    echo -ne "[$(repeatChar '█' $barSize)$(repeatChar ' ' $(($_progressBarSize - $barSize)))]"
}

# Run tests and report progress
for testFile in $testFiles; do
    echo -e "\r$(printf "%-50.50s" "Running $testFile")"
    printBar $_numTestsCompleted $numTestFiles

    racket $testFile > /dev/null

    ((_numTestsCompleted++))
done
#racket testFiles > /dev/null
