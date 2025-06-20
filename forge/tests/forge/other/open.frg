#lang forge

// Quoted filepaths relative to the directory of this module
open "other-file.frg"
open "other-file2.frg"

// Note: both files have unnamed tests and runs

option run_sterling off

sig B extends A {}
no_error: assert {some B} is sat
