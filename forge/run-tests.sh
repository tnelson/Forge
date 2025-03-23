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

# This will create a newline-separated list of file names. But note comments on `for` below.
testFiles="$( find $testDir -type f \( -name "*.rkt" -o -name "*.frg" \) | grep --invert-match ${doNotTestPattern} )"
numTestFiles="$(echo "$testFiles" | wc -l)"

# Helper variables
breakLine="-------------------------------------------\n"
exitCode=0

# Print header of test files found
echo -e "Found the following $numTestFiles test files:\n$breakLine$testFiles\n$breakLine"

# Run tests and report progress
# Default bash `for` will word-split on blank space only, ignoring the quotes added above.
# So we set the "internal field separator" and then reset it after the loop.
IFS='
'
for testFile in $testFiles; do
    current=`date "+%X"`
    echo -e "\nRunning $testFile ($current)"

    #start=`date +%s` 
    # Use permanent (-O) option flag to always disable Sterling   
    racket $testFile -O run_sterling \'off > /dev/null
    #end=`date +%s`
    #echo -e "Testfile took $((end-start)) seconds."
    testExitCode=$?

    if [[ $testExitCode -ne 0 ]]; then
        echo "Test failed with code $testExitCode"
        exitCode=1
    fi
done

# Windows disallows quotes in a filename, but Linux and MacOS permit it. 
# To test that Forge is properly handling these without breaking the 
# test suite on Windows, we create the file dynamically based on OS. 
if [[ $(uname) != "Windows" ]]; then
    echo "Creating file with quotes in its name..."
    touch "$testDir/forge/other/quotes_in_\"_'_filename.frg"
    cat "$testDir/forge/other/QUOTES_TEMPLATE.txt" > "$testDir/forge/other/quotes_in_\"_'_filename.frg"
    
    racket "$testDir/forge/other/quotes_in_\"_'_filename.frg" -O run_sterling \'off > /dev/null
    testExitCode=$?
    if [[ $testExitCode -ne 0 ]]; then
        echo "Test failed with code $testExitCode"
        exitCode=1
    fi
else
    echo "Windows forbids files with quotes in their name; skipping that test..."
fi
exit $exitCode
