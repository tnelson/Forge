#lang forge

sig A {
    r: set Int
}

inst myInst {
    A = `AAA
    r = `AAA -> (1 + 2 + sing[3])
}

run {} for myInst
