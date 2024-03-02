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
# In "basic" regular expressions, use the backslashed versions of "(", ")", and "|"
# Also, apply the pattern deeper than just one directory level (handle error/loc/*.frg)
#doNotTestPattern="\(error\|srclocs\)/[^/]*\\.frg"
doNotTestPattern="\(error\|srclocs\)/.*\\.frg"
# ^ these tests get checked by tests/error/main.rkt
testFiles="$( find $testDir -type f \( -name "*.rkt" -o -name "*.frg" \) | grep --invert-match ${doNotTestPattern} )"
numTestFiles="$(echo "$testFiles" | wc -l)"

# Helper variables
breakLine="-------------------------------------------\n"
exitCode=0

# Print header of test files found
echo -e "Found the following $numTestFiles test files:\n$breakLine$testFiles\n$breakLine"

Run tests and report progress
for testFile in $testFiles; do
    current=`date "+%X"`
    echo -e "\nRunning $testFile ($current)"

    #start=`date +%s`    
    racket $testFile > /dev/null
    #end=`date +%s`
    #echo -e "Testfile took $((end-start)) seconds."
    testExitCode=$?

    if [[ $testExitCode -ne 0 ]]; then
        echo "Test failed with code $testExitCode"
        exitCode=1
    fi
done

exit $exitCode
