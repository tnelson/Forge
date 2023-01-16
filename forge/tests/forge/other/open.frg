#lang forge

option run_sterling off

open "other-file.frg"

sig B extends A {}

test expect {
  no_error: {some B} is sat
}
