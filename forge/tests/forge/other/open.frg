#lang forge

// Quoted filepaths relative to the directory of this module
open "other-file.frg"
open "other-file2.frg"
// Even on Windows, module paths need to use Unix formatting (as of Dec 2025)
open "./diff_dir/other_file3.frg"

// Note: both files have unnamed tests and runs

option run_sterling off

sig B extends A {}
no_error: assert {some A and some B and some C and some D } is sat
