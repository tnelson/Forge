#lang forge
-- Smoke test: verify that all native JNI solvers load and solve correctly.
-- Uses Glucose; other solvers tested in companion files.

option run_sterling off
option solver Glucose

sig A {}

test expect {
  sat_glucose: {some A} is sat
  unsat_glucose: {some A and no A} is unsat
}
