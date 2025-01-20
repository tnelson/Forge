#!/bin/bash

# Exit full script on interrupt
trap "exit" INT

# Check for args
if [ $# -eq 0 ]; then
    echo "Usage: $0 <test-directory>"
    exit 1
fi

# Compile Forge, for test speed
echo "Re-running Forge setup for test efficiency."
raco setup forge

# Get test files
testDir=$1
testFiles="$( find $testDir -type f \( -name "*.rkt" -o -name "*.frg" \) )"
numTestFiles="$(echo "$testFiles" | wc -l)"

# Helper variables
breakLine="-------------------------------------------\n"
exitCode=0

# Print header of test files found
echo -e "Found the following $numTestFiles test files:\n$breakLine$testFiles\n$breakLine"

# Run tests and report progress
for testFile in $testFiles; do
    current=`date "+%X"`
    echo -e "\nRunning $testFile ($current)"

    start=`date +%s` 
    # Use permanent (-O) option flag to always disable Sterling   
    racket $testFile -O run_sterling \'off > /dev/null
    end=`date +%s`
    echo "$testFile, $((end-start))s" >> $testDir/benchmark_times.csv
    testExitCode=$?

    if [[ $testExitCode -ne 0 ]]; then
        echo "Test failed with code $testExitCode"
        exitCode=1
    fi
done

exit $exitCode
