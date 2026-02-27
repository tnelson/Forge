#lang forge
option run_sterling off
option verbose 0

-- Child atom not in parent. Error should blame the child binding (Child = ...),
-- not the parent binding.
sig Parent {}
sig Child extends Parent {}

test expect {
    shouldFail: {some Child} for {
        Parent = `P1
        Child = `C1
    } is sat
}
