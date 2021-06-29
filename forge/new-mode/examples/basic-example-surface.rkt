#lang forge/new-mode

sig A {
    r : set B
}

sig B {}

example AnExample is A = A for {
    A = `A0 + `A1
}