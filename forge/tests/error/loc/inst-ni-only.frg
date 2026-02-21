#lang forge
option run_sterling off
option verbose 0

-- ni-only bounds crash (breaks.rkt). Error should blame the ni line.
sig Node {}

test expect {
    shouldFail: {} for {
        Node ni `A + `B
    } is sat
}
