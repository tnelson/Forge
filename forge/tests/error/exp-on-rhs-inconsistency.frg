#lang forge

option run_sterling off
option verbose 0
option test_keep last


sig N {children: set N}

pred p {

}

// Inconsistency
assert p is inconsistent with {}