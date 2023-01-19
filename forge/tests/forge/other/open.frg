#lang forge

open "other-file.frg"
open "other-file2.frg"
// both files have unnamed tests and runs

option run_sterling off
sig B extends A {}

test expect {
  no_error: {some B} is sat
}
