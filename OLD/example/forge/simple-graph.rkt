#lang forge

sig A {}
sig B {}

sig Node1 {
    edges11, edges12: set A -> B,
    edges21, edges22: set B -> A
}

myRun : run {}