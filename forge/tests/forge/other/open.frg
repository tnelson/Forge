#lang forge

open "other-file.frg"

option run_sterling off
sig B extends A {}

test expect {
  no_error: {some B} is sat
}
