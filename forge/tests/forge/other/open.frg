#lang forge

// Quoted filepaths relative to the directory of this module
open "other-file.frg"
open "other-file2.frg"
// Even on Windows, module paths need to use Unix formatting (as of Dec 2025)
// Check various:
open "./../other/diff_dir/../diff_dir/other_file3.frg"
open "../other/diff_dir/../diff_dir/other_file4.frg"

option run_sterling off

sig B extends A {}
no_error: assert {some A and some B and some C and some D and some E } is sat
