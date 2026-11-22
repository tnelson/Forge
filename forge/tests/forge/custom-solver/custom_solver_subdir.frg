#lang forge
option solver "./subdir/run.sh"
// This test must run every time, so it properly exercises the solver.
option solve_cache off
option run_sterling off

sig Node {edges: set Node}

test expect {
    s: {some edges} for 1 Node is sat
}
