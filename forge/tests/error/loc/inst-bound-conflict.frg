#lang forge
option run_sterling off
option verbose 0

-- Known limitation: error blames the first `in` binding rather than
-- the conflicting `ni`, because get-blame-node returns the first binding.
sig Node {}

test expect {
    shouldFail: {} for {
        Node in `A + `B
        Node ni `C
    } is sat
}
