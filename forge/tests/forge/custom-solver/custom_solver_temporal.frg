#lang forge/temporal
option solver "./run.sh"
option run_sterling off

one sig Counter { var c: one Int }
pred traces {
    Counter.c = 0
    always { Counter.c' = add[1, Counter.c]}
}

sat_traces: assert {traces} is sat for 2 Int
