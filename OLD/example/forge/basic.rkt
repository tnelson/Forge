#lang forge

sig A {}
sig B {}

pred myPred {
    all x: A | A = B
}