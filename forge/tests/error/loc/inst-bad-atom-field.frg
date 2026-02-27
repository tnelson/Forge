#lang forge
option run_sterling off
option verbose 0

-- Field bound references atom not in sig. Error should blame the field binding (top = ...).
sig StackState {top: lone StackElement}
sig StackElement {}

test expect {
    shouldFail: {some top} for {
        StackState = `Foo + `Bar
        StackElement = `A + `B
        top = `Initial -> `A
    } is sat
}
