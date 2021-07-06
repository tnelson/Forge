#lang forge

option verbose 0
option problem_type temporal

sig Node {
    var edge: set Node,
    var fruit: set Node
}

pred F1 {
    (some fruit) implies (no edge)
}

pred F2 {
    all e : edge | ~e in edge
}

pred F3 {
    edge = ^fruit
}


test expect{
    NotUntil12: {(not F1 until F2) iff ((not F1) until F2)} is theorem
    NotUntil12: {not ((not F1 until F2) iff (not (F1 until F2)))} is sat
    NotUntil13: {(not F1 until F3) iff ((not F1) until F3)} is theorem
    NotUntil13: {not ((not F1 until F3) iff (not (F1 until F3)))} is sat
    NotUntil23: {(not F2 until F3) iff ((not F2) until F3)} is theorem
    NotUntil23: {not ((not F2 until F3) iff (not (F2 until F3)))} is sat
}