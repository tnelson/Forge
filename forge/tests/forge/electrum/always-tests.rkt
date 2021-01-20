#lang forge

option verbose 0
option problem_type temporal

sig Node {
    first : set Node,
    var second : set Node
}

pred secondIsFirst {
    first = second implies first' = second'
}
