#lang forge/froglet
option solver "./run.sh"
// This test must run every time, so it properly exercises the solver.
option solve_cache off
option run_sterling off

sig Node {edges: lone Node}

test expect {
    s: {some n: Node | some n.edges} for 1 Node is sat
}
