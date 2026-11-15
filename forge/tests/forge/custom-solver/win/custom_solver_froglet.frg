#lang forge/froglet
option solver "./run.bat"
option run_sterling off

sig Node {edges: lone Node}

test expect {
    s: {some n: Node | some n.edges} for 1 Node is sat
}
