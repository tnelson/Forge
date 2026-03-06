#lang forge
option run_sterling off
option solver MiniSat

sig A {}

test expect {
  sat_minisat: {some A} is sat
  unsat_minisat: {some A and no A} is unsat
}
