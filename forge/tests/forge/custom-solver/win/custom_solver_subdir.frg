#lang forge
option solver "./subdir/run.bat"
option run_sterling off

sig Node {edges: set Node}

test expect {
    s: {some edges} for 1 Node is sat
}
