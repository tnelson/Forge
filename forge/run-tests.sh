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

smtTestPattern=".*/smtlibtor/.*"

# This will create a newline-separated list of file names. But note comments on `for` below.
testFiles="$( find $testDir -type f \( -name "*.rkt" -o -name "*.frg" \) | grep --invert-match ${doNotTestPattern} )"
numTestFiles="$(echo "$testFiles" | wc -l)"

# Helper variables
breakLine="-------------------------------------------\n"
exitCode=0

# Print header of test files found
echo -e "Found the following $numTestFiles test files:\n$breakLine$testFiles\n$breakLine"

##########################
# Run the given test suite
##########################

which cvc5 &> /dev/null;
cvc5OnPathES=$?

if [[ $cvc5OnPathES -eq 0 ]]; then 
  echo "Found cvc5, will run SMT tests if any."
else 
  echo "Could not find cvc5, will skip SMT tests if any."
fi

# Run tests and report progress
# Default bash `for` will word-split on blank space only, ignoring the quotes added above.
# So we set the "internal field separator" and then reset it after the loop.
IFS='
'
for testFile in $testFiles; do
    current=`date "+%X"`
    [[ $testFile =~ ${smtTestPattern} ]]
    matchedSMT=$?

    echo -e -n "\nRunning $testFile ($current)"
    if [[ $matchedSMT -eq 0 ]]; then 
        echo " (smt)"
    else 
        echo " " 
    fi

    # Only run SMT tests if cvc5 is on the path.
    if [[ $cvc5OnPathES != 0 && $matchedSMT -eq 0 ]]; then
        echo "Skipping SMT backend test because cvc5 is not on the PATH: $testFile"
    else
        #start=`date +%s` 
        # Use permanent (-O) option flag to always disable Sterling   
        racket $testFile -O run_sterling \'off > /dev/null
        #end=`date +%s`
        #echo -e "Testfile took $((end-start)) seconds."
        testExitCode=$?
    fi

    if [[ $testExitCode -ne 0 ]]; then
        echo "Test failed with code $testExitCode"
        exitCode=1
    fi
done  

#####################################
# Check for unusual filename handling
#####################################

# Windows disallows quotes in a filename, but Linux and MacOS permit it. 
# To test that Forge is properly handling these without breaking the 
# test suite on Windows, we create the file dynamically based on OS.
#   If running from Git Bash, uname will return a different value. 
osid=$(uname)
if [[ "$testDir" != "tests" ]]; then 
    echo "Skipping unusual filename handling, as script was run on a subdirectory of tests/"
elif [[ $(uname) != "Windows" && ! $(uname) =~ ^MINGW ]]; then
    echo "Testing unusual filename handling: creating file with quotes in its name..."
    touch "$testDir/forge/other/quotes_in_\"_'_filename.frg"
    cat "$testDir/forge/other/QUOTES_TEMPLATE.txt" > "$testDir/forge/other/quotes_in_\"_'_filename.frg"
    
    racket "$testDir/forge/other/quotes_in_\"_'_filename.frg" -O run_sterling \'off > /dev/null
    testExitCode=$?
    if [[ $testExitCode -ne 0 ]]; then
        echo "Test failed with code $testExitCode"
        exitCode=1
    fi
    rm "$testDir/forge/other/quotes_in_\"_'_filename.frg"
else
    echo "Windows (uname = $osid) forbids files with quotes in their name; skipping that test..."
fi
exit $exitCode


# Notes on bash scripting:
#  * when using [[ ... ]]; in an if-statement, there must be spaces between the 
#    ... and the brackets. This is because of how bash tokenizes by blank space.
#
#  * the conditional of an if-statement is about exit status. So sometimes you
#    can use a command directly, without brackets. But use of brackets forms a 
#    combined command. See the "test" command, e.g., 
#  test 1 = 1
#  echo $?
#  test 1 = 2
#  echo $?
#  While `test` implements [ ], double-brackets are extended syntax. Try:
#    [[ "food" =~ "foo" ]] ; echo $?

