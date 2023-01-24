#lang forge

open "invalid-example.frg"
// Opening a file that contains an invalid example
// should not cause the main file to fail

// This file has a `.rkt` extension so the `run-tests.sh`
// script finds it (even in the error/ folder)
