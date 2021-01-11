#lang forge

option problem_type temporal

sig Node {
    node1 : set Node,
    node2 : set Node,
    node3 : set Node
}
sig Stepper {
    var step : set Stepper //will be used to restrict posssible transitions
}

//Initial State
pred init {
    Node in Node.node1
    no node2
    Node->Node in node3
    no Stepper
}

//There will be three types of transitions,
//First prog1, then prog2, then prog3
//After the first prog2, prog1 is no longer allowed
//After the first prog3, prog2 is no longer allowed
//Note that it is possible to get to prog3 before prog2

//step must be empty here,
//but in prog2 and prog3 it will stop being empty,
//which means that after prog2 or prog3 this pred is no longer possible
pred prog1 {
    some Stepper
    no step
}

//step is no longer empty,
//but Stepper.step is not all of Stepper either
//step in step', so step' has at least as many elements as step,
//so step' cannot be empty, meaning prog1 can't be possible ever again
pred prog2 {
    some Stepper
    some step and Stepper.step != Stepper
    step in step'
}

//Now, Stepper.step = Stepper,
//and step' = step, so it will always be true from now on that Stepper.step = Stepper,
//meaning that prog1 and prog2 will never be possible again
pred prog3 {
    some Stepper
    Stepper in Stepper.step
    step' = step
}

test expect StepperProper {
    thing1 : {prog3 and eventually prog2} is unsat
    thing2 : {prog3 and eventually prog1} is unsat
    thing3 : {prog2 and eventually prog1} is unsat
    thing4 : {prog1 and eventually prog2 and eventually prog3} is sat
    thing5 : {prog1 and eventually prog2 and not eventually prog3} is sat
    thing6 : {prog1 and not eventually prog2 and eventually prog3} is sat
    thing7 : {prog2 and eventually prog3} is sat
    thing8 : {prog3 and eventually prog3} is sat
}
