#lang forge

option problem_type temporal

sig Node {
    first : set Node,
    var second : set Node
}

pred secondIsFirst {
    first = second implies first' = second'
}