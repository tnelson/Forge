#lang forge
option run_sterling off
option verbose 0

-- Error should blame the conflicting ni (line 10), not the first in (line 9).
-- Currently blames line 9 because get-blame-node returns the first binding.
sig Node {}

test expect {
    shouldFail: {} for {
        Node in `A + `B
        Node ni `C
    } is sat
}
