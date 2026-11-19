#lang forge
option run_sterling off
option verbose 0
option backend smtlibtor

sig A {}

test expect {
  someSet: { some X: set A | #X > 1 } for exactly 3 A is forge_error "require the Pardinus backend"
}
