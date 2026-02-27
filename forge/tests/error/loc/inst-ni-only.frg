#lang forge
option run_sterling off
option verbose 0

-- Using only ni (backwards in, i.e. superset bound) without in should produce
-- an error, not a crash. Error should blame the ni binding.
sig Node {}

test expect {
    shouldFail: {} for {
        Node ni `A + `B
    } is sat
}
