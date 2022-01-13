#lang forge
open "other-file.frg"

sig B extends A {}

test expect {
  no_error: {some B} is sat
}
