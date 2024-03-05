#lang forge

option run_sterling off


option verbose 0
option problem_type temporal

sig Node {
    first : set Node,
    var second : set Node
}

pred secondIsFirst {
    first = second implies first' = second'
}

// WHOOPS I NEVER FINISHED THIS ONE
